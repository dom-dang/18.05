#---------------------------------
# Sample code for 18.05 Studio 10
#--------------------------------
# 1. z-confidence intervals
# 2. t-confidence and chi square intervals

#------------------------------
# 1. z-confidence intervals
# Generate some normal data and create 95% and 80% z-confidence intervals for the mean. For each interval, check if there is a type 1 CI error.

cat("----\nz-confidence interval for mean.\n")
# Set parameters
sigma = 2
mu = 4
n_data = 64
cat('mu =', mu, '\n')
cat('sigma =', sigma, '\n')
cat('n_data =', n_data, '\n')
cat('--\n')
  
# Generate data
data1 = rnorm(n_data, mu, sigma)

# Compute mean
xbar = mean(data1)
cat('xbar =', xbar, '\n')

# 95%  interval
confidence = 0.95
alpha = 1 - confidence
z_alpha2 = qnorm(1-alpha/2, 0, 1)

# The margin of error does not depend on the data.
# This confidence interval is symmetric around xbar, so we can compute the 'margin_of_error' using just the right critical value.
margin_of_error = z_alpha2*sigma/sqrt(n_data)

# Create and print the confidence interval
ci = c(xbar-margin_of_error, xbar+margin_of_error)
cat(confidence, 'z-CI for mean:', '[', ci[1], ',', ci[2], ']', '\n')

# Check for a type 1 error. Is xbar more than the margin of error away from mu. (We can only do this since we know the true value of mu.)
is_type1_CI_error = abs(xbar - mu) > margin_of_error 
if (is_type1_CI_error) {
  cat('Made type 1 CI error\n')
} else {
  cat('No type 1 CI error\n')
}

# 80%  interval
confidence = 0.8
alpha = 1 - confidence
z_alpha2 = qnorm(1-alpha/2, 0, 1)

# The margin of error does not depend on the data.
# This confidence interval is symmetric around xbar, so we can compute the margin_of_error' using just the right critical value.
margin_of_error = z_alpha2*sigma/sqrt(n_data)

# Create and print the confidence interval
ci = c(xbar-margin_of_error, xbar+margin_of_error)
cat(confidence, 'z-CI for mean:', '[', ci[1], ',', ci[2], ']', '\n')

# Check for a type 1 error. Is xbar more than the margin of error away from mu. (We can do this since we know the true value of mu.)
is_type1_CI_error = abs(xbar - mu) > margin_of_error 
if (is_type1_CI_error) {
  cat('Made type 1 CI error\n')
} else {
  cat('No type 1 CI error\n')
}

#---------------------------
# 2. t-confidence and chi square intervals

# Generate some normal data 
# (i) Create a 90% t-confidence interval for the mean. Check if there is a type 1 CI error.
# (ii) Create a 90% chi square confidence interval for the variance. Check if there is a type 1 CI error.

cat("---------\nt-confidence interval for mean and chi square-confidence interval for the variance.\n")

# Set parameters
sigma = 2
mu = 4
n_data = 64
cat('mu =', mu, '\n')
cat('sigma =', sigma, '\n')
cat('n_data =', n_data, '\n')
cat('--\n')
  
# Generate data
data2 = rnorm(n_data, mu, sigma)

# Compute summary statistics
xbar = mean(data2)
s2 = var(data2)
s = sqrt(s2)
df = n_data - 1
cat('xbar =', xbar, '\n')
cat('s^2 =', s2, '\n')
  
# 90%  t-interval
confidence = 0.90
alpha = 1 - confidence
t_alpha2 = qt(1-alpha/2, df)

# The margin of error depends on s.
# This confidence interval is symmetric around xbar, so we can compute the 'margin_of_error' using the right critical value.
margin_of_error = t_alpha2*s/sqrt(n_data)

# Create and print the confidence interval
ci = c(xbar-margin_of_error, xbar+margin_of_error)
cat(confidence, 't-CI for mean:', '[', ci[1], ',', ci[2], ']', '\n')

# Check for a type 1 error. Is xbar more than the margin of error away from mu. (We can do this since we know the true value of mu.)
is_type1_CI_error = abs(xbar - mu) > margin_of_error 
if (is_type1_CI_error) {
  cat('Made type 1 CI error\n')
} else {
  cat('No type 1 CI error\n')
}

# 90%  chi square interval
confidence = 0.90
alpha = 1 - confidence

# This interval is not symmetric around s2, so we need both left and right critical points.
crit_right = qchisq(1-alpha/2, df)
crit_left = qchisq(alpha/2, df)

# Create and print confidence interval.
ci_variance = c((n_data-1)*s2/crit_right,  (n_data-1)*s2/crit_left)
cat(confidence, "chi square confidence interval for the variance: [", ci_variance[1], ' , ', ci_variance[2], ']\n')

# Check for a type 1 error. (We can do this since we know the true value of sigma.)
if (sigma^2 < ci_variance[1] || sigma^2 > ci_variance[2]) {
  cat('Made type 1 CI error\n')
} else {
  cat('No type 1 CI error\n')
}

