# Studio 11 ----

# 1. Be sure to read the instructions file!

# 2. The test answers file gives sample output and the functions we ran to get it. You should use this to verify your code is giving correct output in the format asked for.


#--------------------------------------
# **** matrixStats package *****
# You should used the Package tab in R Studio to install matrixStats.
# Type > install.packages("matrixStats") into your console below!
library('matrixStats')


#--------------------------------------
# Problem 1: Simulated type 1 bootstap CI error rate for log normal data
# See instructions for this studio

studio11_problem_1a = function(meanlog, sdlog) {
  cat("\n----------------------------------\n")
  cat("Problem 1a: Print true mean and true std of the log normal distribution\n")

  # Arguments:
  # meanlog = value of meanlog parameter in rlnorm
  # sdlog = value of sdlog parameter in rlnorm

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********
  true_mean = exp(meanlog + sdlog^2/2)
  true_median = exp(meanlog)
  true_sd = sqrt((exp(sdlog^2) - 1) * exp(2*meanlog + sdlog^2))
  
  cat("LogNormal: meanlog=", meanlog, ", sdlog=", sdlog, "\n")
  cat("Distribution mean=", true_mean,", median=", true_median, ", std dev=", true_sd, "\n")

}

studio11_problem_1b = function(meanlog, sdlog, n_data, n_boot, n_trials, confidence) {
  cat("-----\n")
  cat("Problem 1b: Simulated type 1 empirical bootstrap CI error rate: log normal distribution\n")

  # Arguments:
  # meanlog = value of meanlog parameter in rlnorm
  # sdlog = value of sdlog parameter in rlnorm
  # n_data = number of values in one sample. (Original generated using a log normal distribution.)
  # n_boot = number of bootstrap samples to use in each trial
  # n_trials = number of trials to run in simulation
  # confidence = the bootstrap confidence level


  # Do not change the above code.
  # ********* YOUR CODE HERE ***********
  true_mean = exp(meanlog + sdlog^2/2)
  true_median = exp(meanlog)
  true_sd = sqrt((exp(sdlog^2) - 1) * exp(2*meanlog + sdlog^2))
  
  mean_percentile_errors = 0 
  mean_basic_errors = 0 
  
  median_percentile_errors = 0 
  median_basic_errors = 0 
  
  sd_percentile_errors = 0 
  sd_basic_errors = 0 
  
  alpha = 1 - confidence 
  lower_percentile = alpha/2
  upper_percentile = 1 - alpha/2
  
  for (trial in 1:n_trials) { 
    original_sample = rlnorm(n_data, meanlog, sdlog)
    
    original_mean = mean(original_sample)
    original_median = median(original_sample)
    original_sd = sd(original_sample)
    
    bootstrap = matrix(sample(original_sample, n_data * n_boot, replace = TRUE), nrow = n_data, ncol = n_boot)
    
    boot_means = colMeans2(bootstrap)
    boot_medians = colMedians(bootstrap)
    boot_sds = colSds(bootstrap)
    
    mean_percentile_ci = quantile(boot_means, c(lower_percentile, upper_percentile))
    median_percentile_ci = quantile(boot_medians, c(lower_percentile, upper_percentile))
    sd_percentile_ci = quantile(boot_sds, c(lower_percentile, upper_percentile))
    
    mean_basic_ci = c(2*original_mean - quantile(boot_means, upper_percentile)[1], 2*original_mean - quantile(boot_means, lower_percentile)[1])
    median_basic_ci = c(2*original_median - quantile(boot_medians, upper_percentile)[1], 2*original_median - quantile(boot_medians, lower_percentile)[1])
    sd_basic_ci = c(2*original_sd - quantile(boot_sds, upper_percentile)[1], 2*original_sd - quantile(boot_sds, lower_percentile)[1])
    
    if (true_mean < mean_percentile_ci[1] || true_mean > mean_percentile_ci[2]) {
      mean_percentile_errors = mean_percentile_errors + 1
    }
    if (true_mean < mean_basic_ci[1] || true_mean > mean_basic_ci[2]) {
      mean_basic_errors = mean_basic_errors + 1
    }
    
    if (true_median < median_percentile_ci[1] || true_median > median_percentile_ci[2]) {
      median_percentile_errors = median_percentile_errors + 1
    }
    if (true_median < median_basic_ci[1] || true_median > median_basic_ci[2]) {
      median_basic_errors = median_basic_errors + 1
    }
    
    if (true_sd < sd_percentile_ci[1] || true_sd > sd_percentile_ci[2]) {
      sd_percentile_errors = sd_percentile_errors + 1
    }
    if (true_sd < sd_basic_ci[1] || true_sd > sd_basic_ci[2]) {
      sd_basic_errors = sd_basic_errors + 1
    }
    
    mean_percentile_error_rate = mean_percentile_errors / n_trials
    mean_basic_error_rate = mean_basic_errors / n_trials
    
    median_percentile_error_rate = median_percentile_errors / n_trials
    median_basic_error_rate = median_basic_errors / n_trials
    
    sd_percentile_error_rate = sd_percentile_errors / n_trials
    sd_basic_error_rate = sd_basic_errors / n_trials
    
  }
  
  cat("LogNormal: meanlog=", meanlog, ", sdlog=", sdlog, " \n")
  cat("Distribution mean=", true_mean, ", median=",true_median, 
      ", std dev=", true_sd, " \n")
  cat("n_data =", n_data, " n_boot =", n_boot, " n_trials = ", n_trials, " \n")
  cat("Nominal confidence: ", confidence, " \n")
  cat("Type 1 error rates (percentile, basic):\n")
  cat("mean:", mean_percentile_error_rate, " ", mean_basic_error_rate, " \n")
  cat("median: ", median_percentile_error_rate, " ", median_basic_error_rate, " \n")
  cat("sd: ", sd_percentile_error_rate, " ", sd_basic_error_rate)
}

studio11_problem_1c = function(meanlog, sdlog, n_data, n_boot) {
  cat("-----\n")
  cat("Problem 1c: Historgram for statistics of the bootstrap samples\n")
  
  # Arguments:
  # meanlog = value of meanlog parameter in rlnorm
  # sdlog = value of sdlog parameter in rlnorm
  # n_data = number of values in one sample. (Original generated using a log normal distribution.)
  # n_boot = number of bootstrap samples to use in each trial
  
  
  # Do not change the above code.
  # ********* YOUR CODE HERE ***********
  
}
