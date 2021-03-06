// JIT Convolutional Layer on GPU
// (See tf.nn.conv2d)

// On linux, you can compile and run it like so:
// g++ conv2d_gpu.cpp -g -std=c++17 -I <path/to/Halide.h> -I <path/to/tools/halide_image_io.h> -L <path/to/libHalide.so> -lHalide `libpng-config --cflags --ldflags` -ljpeg -lpthread -ldl -o conv2d_gpu
// LD_LIBRARY_PATH=<path/to/libHalide.so> ./conv2d_gpu

// On os x:
// g++ conv2d_gpu.cpp -g -std=c++17 -I <path/to/Halide.h> -I <path/to/tools/halide_image_io.h> -L <path/to/libHalide.so> -lHalide `libpng-config --cflags --ldflags` -ljpeg -o conv2d_gpu
// DYLD_LIBRARY_PATH=<path/to/libHalide.dylib> ./conv2d_gpu

#include <stdio.h>
#include <string>
#include "Halide.h"
#include "clock.h"

using namespace std;
using namespace Halide;
using namespace Halide::Tools;

Target find_gpu_target();

// Conv2DLayerGPU follows the tf.nn.conv2d implementation of conv2d layer.
// Note that in this implementation no bias or activation function is applied.
// Data must follows NHWC format.
class Conv2DLayerGPU {
public:
    Var n, x, y, ci, co, x_outer, y_outer, x_inner, y_inner, tile_index;
    Func pad, conv;
    Buffer<float> input;
    Buffer<float> filters;
    const int stride;
    RDom r;

    // Constructor parameters:
    //  input: 4-D tensor of shape [batch_size, in_height, in_width, in_channels].
    //  filters: 4-D tensor of shape [out_channels, in_height, in_width, in_channels]. (TODO: change to TF format)
    //  stride: int for the stride of the sliding window.
    Conv2DLayerGPU(Buffer<float> input, Buffer<float> filters, const int stride)
        : input(input), filters(filters), stride(stride){

        // Make sure we have a square kernel.
        assert(filters.dim(1).extent() == filters.dim(2).extent());
        // Make sure we have a valid kernel (size is odd).
        assert(filters.dim(1).extent() % 2 == 1);
        // Make sure the kernel has the same channels as input.
        assert(filters.dim(3).extent() == input.dim(3).extent());

        // Pad input.
        pad = BoundaryConditions::constant_exterior(input, 0);

        // Apply filters.
        int kernel_size = filters.dim(1).extent();
        int offset = kernel_size / 2;
        r = RDom(0, kernel_size, 0, kernel_size, 0, input.dim(3).extent());
        conv(n, x, y, co) += filters(co, r.x, r.y, r.z) * pad(n, stride * x + r.x - offset, stride * y + r.y - offset, r.z);
    }

    // Now a schedule that uses CUDA or OpenCL.
    bool schedule_for_gpu() {
        Target target = find_gpu_target();
        if (!target.has_gpu_feature()) {
            return false;
        }

        conv.tile(x, y, x_outer, y_outer, x_inner, y_inner, 32, 32)
            .fuse(x_outer, y_outer, tile_index)
            .gpu_blocks(tile_index)
            .gpu_threads(x_inner);

        printf("Target: %s\n", target.to_string().c_str());
        conv.compile_jit(target);

        return true;
    }

    void test_performance(int num_runs=100) {
        // Test the performance of the scheduled Conv2DLayerGPU.
        Buffer<float> output(input.dim(0).extent(), input.dim(1).extent(), input.dim(2).extent(), filters.dim(0).extent());

        // Run the filter once to initialize any GPU runtime state.
        conv.realize(output);

        // Run pipeline for multiple times.
        double total_time = 0.0;
        double best_time = 0.0;
        for (int i = 0; i < num_runs; i++) {

            double t1 = current_time();
            conv.realize(output);

            // Force any GPU code to finish by copying the buffer back to the CPU.
            output.copy_to_host();

            double t2 = current_time();

            double elapsed = (t2 - t1);
            if (i == 0 || elapsed < best_time) {
                best_time = elapsed;
            }
            total_time += elapsed;
        }
        printf("%d runs in total\n", num_runs);
        printf("Average: %1.4f milliseconds\n", total_time / num_runs);
        printf("Best: %1.4f milliseconds\n", best_time);
    }
};

int main(int argc, char **argv) {
    // Params:
    //   batch_size: number of images (in a single batch).
    //   channels_in: number of input channels (depth of the input).
    //   channels_out: number of ouput channels (the number of filters).
    //   height: height of the image.
    //   width: width of the image.
    //   kernel_size: width and height of the filters. (3 for 3 x 3 conv layer).
    //   stride: the stride for sliding window.
    const int batch_size = 8, width = 120, height = 100, channels_in = 3, channels_out = 3, kernel_size = 5, stride = 1;

    // Generate random input.
    // Input shape follows TensorFlow convention (N, H, W, C)
    printf("Generating input with dimensions: batch_size: %d, height: %d, width: %d, channels: %d\n", batch_size, height, width, channels_in);

    Buffer<float> input(batch_size, height, width, channels_in);
    for (int b = 0; b < batch_size; b++) {
        for (int h = 0; h < height; h++) {
            for (int w = 0; w < width; w++) {
                for (int c = 0; c < channels_in; c++) {
                    input(b, h, w, c) = rand();
                }
            }
        }
    }

    // Generate random filters.
    printf("Generating filters with dimensions: num_filters: %d, height: %d, width: %d, channels: %d\n", channels_out, kernel_size, kernel_size, channels_in);

    Buffer<float> filters(channels_in, kernel_size, kernel_size, channels_out);
    for (int co = 0; co < channels_in; co++) {
        for (int x = 0; x < kernel_size; x++) {
            for (int y = 0; y < kernel_size; y++) {
                for (int ci = 0; ci < channels_out; ci++) {
                    filters(co, x, y, ci) = rand();
                }
            }
        }
    }

    printf("Running pipeline on GPU:\n");
    Conv2DLayerGPU conv_layer(input, filters, stride);
    conv_layer.schedule_for_gpu();

    printf("Testing performance on GPU:\n");
    conv_layer.test_performance();

    return 0;
}

// A helper function to check if OpenCL, Metal or D3D12 is present on the host machine.

Target find_gpu_target() {
    // Start with a target suitable for the machine you're running this on.
    Target target = get_host_target();

    vector<Target::Feature> features_to_try;
    if (target.os == Target::Windows) {
        // Try D3D12 first; if that fails, try OpenCL.
        if (sizeof(void*) == 8) {
            // D3D12Compute support is only available on 64-bit systems at present.
            features_to_try.push_back(Target::D3D12Compute);
        }
        features_to_try.push_back(Target::OpenCL);
    } else if (target.os == Target::OSX) {
        // OS X doesn't update its OpenCL drivers, so they tend to be broken.
        // CUDA would also be a fine choice on machines with NVidia GPUs.
        features_to_try.push_back(Target::Metal);
    } else {
        features_to_try.push_back(Target::CUDA);
    }
    // Uncomment the following lines to also try CUDA:
    // features_to_try.push_back(Target::CUDA);

    for (Target::Feature f : features_to_try) {
        Target new_target = target.with_feature(f);
        if (host_supports_target_device(new_target)) {
            return new_target;
        }
    }

    printf("Requested GPU(s) are not supported. (Do you have the proper hardware and/or driver installed?)\n");
    return target;
}
