# Compute E(Y^2) where Y has density prop to  y^3 sin(y^4) cos(y^5) 1(0<y<1)

# numerical integration:

numfn = function(y) { y^5 * sin(y^4) * cos(y^5) }
denomfn = function(y) { y^3 * sin(y^4) * cos(y^5) }
numval = integrate( numfn, 0, 1 )$value
denomval = integrate( denomfn, 0, 1 )$value
numint = numval/denomval
cat("Numerical integration:", numint, "\n")

# importance sampling, method #1:

M = 10^6
uniflist = runif(M)
xlist = uniflist^(1/6)
numlist = sin(xlist^4) * cos(xlist^5)
denomlist = sin(xlist^4) * cos(xlist^5) / xlist^2
imp1 = mean(numlist) / mean(denomlist)
diff1 = abs(imp1-numint)
cat("Importance sampling, same samples: ", imp1, "; diff =", diff1, "\n" )

# importance sampling, method #1, but, independent samples:

M = 10^6
uniflist = runif(M)
xlist = uniflist^(1/6)
numlist = sin(xlist^4) * cos(xlist^5)
uniflist2 = runif(M)
xlist2 = uniflist2^(1/6)
denomlist = sin(xlist2^4) * cos(xlist2^5) / xlist2^2
imp2 = mean(numlist) / mean(denomlist)
diff2 = abs(imp2-numint)
cat("Importance sampling, indep samples:", imp2, "; diff =", diff2, "\n" )

# decide the winner:
if (diff1 < diff2) {
  cat("Winner: Method #1 (same samples)!\n\n")
} else if (diff1 > diff2) {
  cat("Winner: Method #2 (indep samples)!\n\n")
} else {
  cat("It's a tie!!\n\n")
}
