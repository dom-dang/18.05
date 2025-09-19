v_0 = 10
k = 2 

lambda = 2 * v_0/sqrt(pi)
v = seq(0, 50, length = 100)
density = dweibull(v, k, lambda)

plot(v, density, main = "Weibull Distribution", xlab = "Wind Speed (m/s)", ylab = "Density")


#part e
high = 1 - pweibull(16, k, lambda)
low = pweibull(6, k, lambda)
print(high + low)


