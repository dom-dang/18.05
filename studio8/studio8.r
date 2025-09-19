# Studio 8  ----

# 1. Be sure to read the instructions file!

# 2. The test answers file gives sample output and the functions we ran to get it. You should use this to verify your
#    code is giving correct output in the format asked for.


#--------------------------------------
# Problem 1: Significance and power/type 1 and type 2 errors
# See instructions for this studio

studio8_problem_1 <- function(theta_HA, alpha, n_tosses) {
  cat("----------------------------------\n")
  cat("Problem 1: Rejection region, actual significance, power\n")

  # **** Be careful to avoid an 'off by 1 error' here.

  # Arguments:
  #   theta_HA = the probability of heads in the alternate hypothesis
  #   alpha = significance level for our significance test
  #   n_tosess = the number of tosses in one trial

  theta_H0 <- 0.5 # probability of heads in the null hypothesis

  # Enforce theta_HA > theta_H0
  if (theta_HA < theta_H0) {
    warning("Problem 1: We require theta_HA > theta_H0", immediate. = TRUE)
    return()
  }

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********
  # you should set these variables in your code:
  #
  # rejection_region
  # significance
  # power
  
  critical_val = qbinom(1 - alpha, n_tosses, theta_H0) 
  start = critical_val + 1
  rejection_region = start:n_tosses
  
  significance = 1 - pbinom(start - 1, n_tosses, theta_H0)
  power = 1 - pbinom(start - 1, n_tosses, theta_HA)
  


  # Do not change the below code.
  cat("  Rejection region: ", rejection_region, "\n")
  cat("  True significance=", significance, "\n")
  cat("  Power=", power, "\n")
  return(list(rejection_region, significance, power))
}


#--------------------
# Problem 2: Simulation with a known prior.
# See instructions for this studio

studio8_problem_2 <- function(theta_HA, alpha, n_tosses, n_trials, secret_prior) {
  cat("----------------------------------\n")
  cat("Problem 2: Simulation with a mixture of coins\n")

  # Arguments:
  #   theta_HA = the probability of heads in the alternate hypothesis
  #   alpha = significance level for our significance test
  #   n_tosses = the number of tosses in one trial
  #   n_trials = the number of trials in the simulation
  #   secret_prior = the secret prior used to pick the type of coin for each trial = c(prob. of H0, prob of HA)

  theta_H0 <- 0.5

  # Enforce theta_HA > theta_H0
  if (theta_HA < theta_H0) {
    warning("Problem 1: We require theta_HA > theta_H0", immediate. = TRUE)
    return()
  }

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********
  # You should set these variables in your code:
  #
  # number_rejections
  # number_type_1
  # number_type_2
  # p_rejection_h0
  # p_h0_rejection
  # p_rejection_ha
  # p_ha_rejection
  # p_rejection
  critical_val = qbinom(1 - alpha, n_tosses, theta_H0) 
  start = critical_val + 1
  rejection_region = start:n_tosses
  
  h0_chosen = 0 
  ha_chosen = 0 
  number_rejections = 0 
  number_type_1 = 0 
  number_type_2 = 0 
  
  
  for (i in 1:n_trials) { 
    coin = sample(c("H0", "HA"), 1,replace = TRUE, prob = secret_prior)
    
    if (coin == "H0") { 
      h0_chosen = h0_chosen + 1
      theta = theta_H0
    } else {
      ha_chosen = ha_chosen + 1
      theta = theta_HA
    }
    
    num_heads = rbinom(1, n_tosses, theta) 
    reject = num_heads %in% rejection_region
    
    if (reject) { 
      number_rejections = number_rejections + 1
    
      if (coin == "H0") { 
        number_type_1 = number_type_1 + 1 
      }
    } else {
      if (coin == "HA") { 
        number_type_2 = number_type_2 + 1 
      }
    }
  }
  
  p_rejection_h0 = ifelse(h0_chosen > 0, number_type_1 / h0_chosen, 0)
  p_rejection_ha = ifelse(ha_chosen > 0, (ha_chosen - number_type_2) / ha_chosen, 0)
  p_rejection = number_rejections / n_trials
  
  p_h0_rejection <- ifelse(number_rejections > 0, number_type_1 / number_rejections, 0)
  p_ha_rejection <- ifelse(number_rejections > 0, (number_rejections - number_type_1) / number_rejections, 0)
  

  # Do not change the below code.
  cat("  theta_HA=", theta_HA, ", alpha=", alpha, "\n", sep = "")
  cat("  n_tosses=", n_tosses, ", n_trials=", n_trials, "\n", sep = "")
  cat("  secret_prior=", secret_prior, "\n")
  cat("  Number of rejections: ", number_rejections, "\n")
  cat("  Number of type 1: ", number_type_1, "\n")
  cat("  Number of type 2: ", number_type_2, "\n")
  cat("  P(rejection | H0): ", p_rejection_h0, "\n")
  cat("  P(H0 | rejection): ", p_h0_rejection, "\n")
  cat("  P(rejection | HA): ", p_rejection_ha, "\n")
  cat("  P(HA | rejection): ", p_ha_rejection, "\n")
  cat("  P(rejection): ", p_rejection, "\n")
  return(list(number_rejections, number_type_1, number_type_2, p_rejection_h0, p_h0_rejection, p_rejection_ha, p_ha_rejection, p_rejection))
}

#--------------------
# Problem 3: Simulation with only fair coins
# See instructions for this studio

studio8_problem_3a <- function(theta_HA, alpha, n_tosses, n_trials) {
  cat("----------------------------------\n")
  cat("3a: Simulation with all fair coins: explain results\n")

  secret_prior <- c(1.0, 0) # Prob of H0, prob of HA
  cat("----\n")
  cat("# Problem 2 called from problem 3a\n")
  studio8_problem_2(theta_HA, alpha, n_tosses, n_trials, secret_prior)
  cat("----\n")

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  cat("When all the coins are fair, H0 should always be true: \n")
  cat("- that means P(rejection | H0) and P(rejection) should equal alpha (seen) since there is a 5% chance we will randomly reject our null hypothesis. \n")
  cat("- P(rejection | HA) and P(HA | rejection) should equal 0 since it will never occur (seen) \n")
  cat("- This shows that the significance level (alpha) represents the probability of incorrectly rejecting H0 when it is true. \n")
}

studio8_problem_3b <- function(theta_HA, alpha, n_tosses, n_trials) {
  cat("-----\n")
  cat("3b: Simulation with all unfair coins: explain results\n")

  secret_prior <- c(0.0, 1.0) # Prob of H0, prob of HA
  cat("----\n")
  cat("# Problem 2 called from problem 3b\n")
  studio8_problem_2(theta_HA, alpha, n_tosses, n_trials, secret_prior)
  cat("----\n")

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  cat("When all the coins are unfair, HA should always be true: \n")
  cat("- that means P(rejection | H0) and P(H0 | rejection) should equal 0 (seen) since H0 will never occur and is never true. \n")
  cat("- P(rejection | HA) is the power of the test \n")
  cat("- P(HA | rejection) = 1 since HA is always true \n")
  cat("- P(rejection) should be equal to the power (seen) \n ")
  cat("- This shows that the power represents the probability of correctly rejecting H0 when HA is true. \n")
}

studio8_problem_3c <- function() {
  cat("-----\n")
  cat("3c: Explain difference between P(H0 | rejection) and significance\n")


  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  cat("The significance (P(rejection | H0) is the probablity of rejecting H0 given that H0 is true.\n")
  cat("P(H0 | rejection) is the probablity that H0 is true given that we rejected it. This is the probablity of making a Type 1 error when we reject. \n")
  cat("To calculate P(H0 | rejection), we need a prior proablity on H0 which we don't use in frequentist statistics.")
}

studio8_problem_3d <- function() {
  cat("-----\n")
  cat("3d: Shout it out!\n")

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  for (i in 1:5){ 
    cat("  THE SIGNIFICANCE IS NOT THE PROBABILITY THAT A REJECTION IS IN ERROR!\n")
    }
  cat("“It is the probability of rejection given H0. Frequentists don’t compute P(Error | rejection)\n")
}


#--------------------
# OPTIONAL Problem 4: Run a z test
# See instructions for this studio

studio8_problem_4 <- function(data, mu0, known_sigma, alpha) {
  cat("----------------------------------\n")
  cat("Problem 4: Code z-test by hand\n")

  # Arguments
  # data = list of sample values
  # mu0 = hypothesized mean in the null hypothesis
  # known_sigma = known value of the standard deviation
  # alpha = significance for the test


  # Do not change the above code.
  # ********* YOUR CODE HERE ***********
}
