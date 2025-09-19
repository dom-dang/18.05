#---------------------------------
# Sample code for 18.05 Studio 7
#--------------------------------

# 1. The Beta distribution

# rbeta, dbeta, pbeta, qbeta works just like for other distributions

# Recall that Beta(1,1) is the uniform distribution on [1,1]
a = 1
b = 1

# Let's check its density at a point
x = 0.5
print(dbeta(x, a, b))

# We can also check it at a vector of points
x = seq(0, 1, 0.1)
print(dbeta(x, a, b))

# For different values of a and b, Beta(a,b) is no longer uniform
a = 3
b = 5

x = 0.5
print(dbeta(x, a, b))

x = seq(0, 1, 0.1)
print(dbeta(x, a, b))

# We'll plot the density for different values of a and b
# Recall that the Beta pdf is zero outside the interval [0,1]
# We'll discretize the interval in steps of .01
a = 3
b = 5

x = seq(0,1,.01)  

# Plot the result
plot(x,dbeta(x, a, b), type='l', col='blue', lwd=2)

a = 4
b = 2
lines(x,dbeta(x, a, b), col='orange', lwd=2)

a = 4
b = 4
lines(x,dbeta(x, a, b), col='green', lwd=2)

# 2. Numerical integration
# It's not always possible to evaluate integrals analytically (in symbols).
# Often the only possibility is to estimate the value of the integral numerically.

# We will use a simple numerical integration scheme to convince ourselves that the
# density of the Beta(3,5) distribution integrates to 1.

a = 3
b = 5

# For this scheme we discretize the interval [0,1] into m subintervals 
m = 5

# We will estimate the integral by writing the expression for the Riemann sum 
# We'll use the right endpoints of each subinterval.
points = seq(1/m, 1, 1/m)

# Now calculate the density at those points
densities = dbeta(points, a, b)

# Finally we multiply the densities by the widths and sum to get the Riemann sum:
print(approx_integral)

# The result is a number which is close to 1, but not quite.
# We can improve the approximation by using more (smaller) sub-intervals.

m = 10
approx_integral = sum(dbeta(seq(1/m, 1, 1/m), a, b) * 1/m)
print(approx_integral)

# By using a finer subdivision, we can make the estimate arbitrarily close to 1
m = 1000
approx_integral = sum(dbeta(seq(1/m, 1, 1/m),a,b)*1/m)
print(approx_integral)

# A refresher on drawing heat maps
x = 0:100
y = 0:100
tab = matrix(0, nrow=100, ncol=100)
for (i in 1:100) {
  for (j in 1:100) {
    tab[i,j] = i + 2*j
  }
}
opar = par(mfrow=c(2,2), mar=c(1.8,2.0,1,1),mgp=c(.5,1,0))
image(x, y, tab, col=rainbow(100), xlab='', ylab='')
text(0,40, 'rainbow', pos=4, cex=1.3)
image(x, y, tab, col=heat.colors(10), xlab='', ylab='')
text(0,40, 'heat.colors', pos=4, cex=1.3)
image(x, y, tab, col=topo.colors(100), xlab='', ylab='')
text(0,40, 'topo.colors', pos=4, cex=1.3)
image(x, y, tab, col=terrain.colors(100), xlab='', ylab='')
text(0,40, 'terrain.colors', pos=4, cex=1.3)
par(opar)