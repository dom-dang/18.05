# Studio 10 ----

# 1. Be sure to read the instructions file!

# 2. The test answers file gives sample output and the functions we ran to get it. You should use this to verify your code is giving correct output in the format asked for.


#--------------------------------------
# Problem 1: Simulated confidence intervals for normal data
# See instructions for this studio

# Problem 1a: Simulated type 1 CI error rate for z-confidence intervals
studio10_problem_1a = function(theta_vals, theta_prior, sigma, n_data, confidence, n_trials) {
  cat("\n----------------------------------\n")
  cat("Problem 1a: Simulated type 1 CI error rate for z-confidence intervals \n")

  # Arguments:
  #  theta_vals = possible values for the mean of the normal distribution
  #  theta_prior = probabilities for choosing a theta from theta_vals
  #  sigma = standard deviation of the normal distribution
  #  n_data = the number of data values in each trial
  #  confidence = the confidence level, e.g. 0.95, 0.9 etc
  #  n_trials = number of trials in the simulation
  
  type1_errors = 0 
  
  for (i in 1:n_trials) {
    theta_index = sample(1:length(theta_vals), 1, prob = theta_prior)
    theta = theta_vals[theta_index]
    
    data = rnorm(n_data, theta, sigma)
    
    
    xbar = mean(data) 
    z_critical = qnorm(1 - (1-confidence)/ 2)
    error_margin = z_critical * sigma/sqrt(n_data)
    ci_lower = xbar - error_margin
    ci_higher = xbar + error_margin
    
    if (i == n_trials) { 
      last_ci = c(ci_lower, ci_higher)
    }
    
    if (theta < ci_lower || theta > ci_higher) { 
      type1_errors = type1_errors + 1
      }
  }
  
  frac_typeI_ci_errors = type1_errors / n_trials
  
  cat("Last confidence interval: [", last_ci[1], ",", last_ci[2], "] \n")
  cat("Chosen theta:", theta, "\n")
  cat("Type 1 CI-error rate:", frac_typeI_ci_errors, "\n")

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********
  # your code should create a variable called theta, last_ci (which is a list: last_ci = c(..., ...))
  # as well as frac_typeI_ci_errors, corresponding to the fraction of type 1 CI-errors

  # Do not change the line below.
  return(list(last_ci, theta, frac_typeI_ci_errors))
}


# Problem 1b: Simulated type 1 CI error rate for t-confidence intervals
studio10_problem_1b = function(theta_vals, theta_prior, sigma, n_data, confidence, n_trials) {
  cat("\n----------------------------------\n")
  cat("Problem 1b: Simulated type 1 CI error rate for t-confidence intervals \n")

  # Arguments:
  #  theta_vals = possible values for the mean of the normal distribution
  #  theta_prior = probabilities for choosing a theta from theta_vals
  #  sigma = standard deviation of the normal distribution
  #  n_data = the number of data values in each trial
  #  confidence = the confidence level, e.g. 0.95, 0.9 etc
  #  n_trials = number of trials in the simulation

  # Remember: in this problem, you use sigma to generate the data, but not in computing the confidence interval.

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********
  # your code should create a variable called theta, last_ci (which is a list: last_ci = c(..., ...))
  # as well as frac_typeI_ci_errors, corresponding to the fraction of type 1 CI-errors.
  type1_errors = 0 
  
  for (i in 1:n_trials) {
    theta_index = sample(1:length(theta_vals), 1, prob = theta_prior)
    theta = theta_vals[theta_index]
    
    data = rnorm(n_data, theta, sigma)
    
    
    xbar = mean(data) 
    std = sd(data)
    z_critical = qt(1 - (1-confidence)/ 2, df = n_data -1)
    error_margin = z_critical * std/sqrt(n_data)
    ci_lower = xbar - error_margin
    ci_higher = xbar + error_margin
    
    if (i == n_trials) { 
      last_ci = c(ci_lower, ci_higher)
    }
    
    if (theta < ci_lower || theta > ci_higher) { 
      type1_errors = type1_errors + 1
    }
  }
  
  frac_typeI_ci_errors = type1_errors / n_trials
  
  cat("Last confidence interval: [", last_ci[1], ",", last_ci[2], "] \n")
  cat("Chosen theta:", theta, "\n")
  cat("Type 1 CI-error rate:", frac_typeI_ci_errors, "\n")

  # Do not change the line below.
  return(list(last_ci, theta, frac_typeI_ci_errors))
}


# Problem 1c: Bayesian updating and probability of hypotheses
studio10_problem_1c = function(theta_vals, theta_prior, sigma, n_data, confidence, xbar) {
  cat("\n----------------------------------\n")
  cat("Problem 1c: Bayesian updating and probability of hypotheses\n")

  # Arguments:
  #  theta_vals = possible values for the mean of the normal distribution
  #  theta_prior = probabilities for choosing a theta from theta_vals
  #  sigma = standard deviation of the normal distribution
  #  n_data = the number of data values in each trial
  #  confidence = the confidence level, e.g. 0.95, 0.9 etc
  #  xbar = the mean of the data

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********
  likelihood = numeric(length(theta_vals))
  
  for (i in 1:length(theta_vals)) { 
    likelihood[i] = dnorm(xbar, mean = theta_vals[i], sd = sigma/sqrt(n_data))
  }
  
  unnorm_post = theta_prior * likelihood
  posterior = unnorm_post / sum(unnorm_post)
  
  z_critical = qnorm(1 - (1-confidence)/2)
  error_margin = z_critical * sigma/sqrt(n_data)
  ci_lower = xbar - error_margin
  ci_higher = xbar + error_margin
  
  confidence_interval = c(ci_lower, ci_higher)
  in_ci_index = which(theta_vals >= ci_lower & theta_vals <= ci_higher)
  prior_prob = sum(theta_prior[in_ci_index])
  posterior_prob = sum(posterior[in_ci_index])
  
  cat("1c(i) theta_prior", paste(theta_prior, collapse = " "), "\n")
  cat("1c(i) theta_posterior", paste(posterior, collapse = " "), "\n")
  cat("1c(ii)", confidence, "z confidence interval: [", ci_lower, ",", ci_higher, "] \n")
  cat("1c(iii) prior prob. theta is in the CI:", prior_prob, "\n")
  cat("1c(iii) posterior prob. theta is in the CI:", posterior_prob, "\n")
  
}


#---------------------------
# OPTIONAL Problem 2: Simulated polling confidence intervals
# See instructions for this studio

studio10_problem_2 = function(true_theta, n) {
  cat("\n----------------------------------\n")
  cat("Problem 2: Simulated polling confidence interval\n")

  # Arguments
  # true_theta = true proportion who prefer Lincoln. Use this to generate your simulated polling data
  # n = size of the sample


  # Do not change the above code.
  # ********* YOUR CODE HERE ***********
  # your code should create two variables: estimated_theta and margin_of_error.


  # Do not change the line below.
  return(c(estimated_theta, margin_of_error))
}
