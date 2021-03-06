#include <omp.h>
#include <stdio.h>
#define PAD 8
#define NUM_THREADS 8
static long num_steps = 10000000;
double step;
int main() {
    int i, nthreads;
    double pi, sum[NUM_THREADS][PAD], tdata;
    step = 1.0/(double) num_steps;
    omp_set_num_threads(NUM_THREADS);
    #pragma omp parallel
    tdata = omp_get_wtime();
    {
        int i, id, nthrds;
        double x;
        id = omp_get_thread_num();
        nthrds = omp_get_num_threads();
        if (id == 0) {
            nthreads = nthrds;
        }
        for (i=id, sum[id][0]=0.0; i<num_steps; i=i+nthrds) {
            x = (i+0.5)*step;
            sum[id][0] += 4.0/(1.0+x*x);
        }
    }
    for (i=0, pi=0.0; i<nthreads; i++) {
        pi += sum[i][0] * step;
    }
    tdata = omp_get_wtime() - tdata;
    printf("Pi is estimated to be %f\n", pi);
    printf("Program finished in %f secs\n", tdata);
}
