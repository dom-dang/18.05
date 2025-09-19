
# Sample code for 18.05 Studio 11
#--------------------------------
# 1. matrixStats package
# 2. Log normal distribution: rlnorm

#------------------------------
# 1. matrixStats package
cat("----\nmatrixStats package.\n")

# The matrixStats package has functions colMeans2, colMedians, colSds, colQuantiles that are optimized for speed on matrices. This means you can generate all your bootstrap samples at once and put them in a matrix. This will be much faster than doing them one at a time in a loop.

# Note: Everything you can do with matrixStats could be done with a loop, but it will run much slower

### Install the package ###
#You may need to use the Package tab in R Studio to install the matrixStats package.
# OR Type 
# install.packages("matrixStats") 
#into your console below!

# The following line loads the package
library('matrixStats')

# Set up a few constants
# Data is generated using a normal distribution
mu = 1
sigma = 2
n = 10   # number of values in one sample
m = 12   # number of samples
alpha = 0.1

# Generate random normal data
data = rnorm(n, mu, sigma)  

# Generate m resamples each of size n. Put this in a matrix.
x = sample(data, n*m, replace=TRUE)
resample_data = matrix(x, nrow=n, ncol=m)

# Use matrixStats to compute various statistics in the resampled data
resample_means = colMeans2(resample_data)
resample_medians = colMedians(resample_data)
resample_sd = colSds(resample_data)

cat('resample_means', resample_means, '\n')
cat('resample_medians', resample_medians, '\n')
cat('resample_sd', resample_sd, '\n')

# We can use the quantile function to find a percentile confidence interval
p = c(alpha/2, 1-alpha/2)
mean_percentile_ci = quantile(resample_means, p)
median_percentile_ci = quantile(resample_medians, p)
sd_percentile_ci = quantile(resample_sd, p)

cat('mean_percentile_ci', mean_percentile_ci, '\n')
cat('median_percentile_ci', median_percentile_ci, '\n')
cat('sd_percentile_ci', sd_percentile_ci, '\n')

# Plot histograms of the statistics
hist(resample_means, col = "orange", main = "histogram for means")
hist(resample_medians, col = "blue", main = "histogram for median")
hist(resample_sd, col = "pink", main = "histogram for standard deviation")

#---------------------------
# 2. Log normal: rlnorm
cat("----\nLog normal distribution.\n")

meanlog = 1
sdlog = 2
n_data = 1000

# General log normal data
data = rlnorm(n_data, meanlog = meanlog, sdlog = sdlog)

sample_mean = mean(data)
sample_sd = sd(data)

cat('meanlog =', meanlog, ', sdlog =', sdlog, '\n')
cat('sample_mean =', sample_mean, ', sample_sd =', sample_sd, '\n')
cat('Note that the mean and  and standard deviation of the log normal distribution are not the same as meanlog and sdlog.', '\n')
