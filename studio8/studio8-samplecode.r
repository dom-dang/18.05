#---------------------------------
# Sample code for 18.05 Studio 8
#--------------------------------
# Topics:
# 1. qbinom and off by 1 errors
# 2. The %in% operator
# 3. sample with prescribed probabilities

# Flags for running code
do_topic1_qbinom = T
do_topic2_in_operator = T
do_topic3_prescribing_probabilities = T

#-----------------------------------
if (do_topic1_qbinom) {
  # qbinom and off by 1 errors
  cat("----------------\n1. qbinom and off by 1 errors\n")

  # With discrete distributions we have to be careful not to make 'off by 1 errors'
  
  # Because:
  # k = qbinom(alpha, n, theta)  gives the smallest value of k so that P(x <= k) >= alpha, we shouldn't  put k in the rejection region
  #For example
  n = 8
  theta = 0.5
  alpha_desired = 0.1
  cat("alpha_desired =", alpha_desired, '\n')

  # Create and view the probability table
  x = 0:n
  prob = dbinom(0:n, n, theta)
  prob_table = cbind(x,prob)
  s = sprintf("\nProbability table for binom(%d, %.1f)", n, theta)
  cat(s,"\n")
  print(prob_table, digits=2)
  
  cat("\nLook for left-sided rejection region\n")
  # qbinom returns the first value of k with cumulative probability GREATER than alpha
  k = qbinom(alpha_desired, n, theta)
  cat("k =", k, '\n')
  cat("Try reject_region =", 0:k, '\n')
  alpha_actual = pbinom(k, n, theta)
  cat("alpha_actual =", alpha_actual, '> alpha_desired.\n')
  cat("This is an off-by-one error.\n")
  
  cat("\nGet correct rejection region.\n")
  # So if we want a left-sided 
  # rejection region for binom(8, 0.5) 
  # with significance 0.1 we need
  critical_left = qbinom(alpha_desired, n, theta) - 1
  reject_region_left = 0:critical_left
  cat('critical_left:', critical_left, '\n')
  cat('reject_region_left:', reject_region_left, '\n') 
  
  cat("\nCheck actual significance is <= desired significance.\n")
  # Check probability in rejection region.
  # Using dbinom and the rejection region, helps avoid off by one errors.
  alpha_actual_left = sum(dbinom(reject_region_left, n, theta))
  cat("alpha_actual_left =", alpha_actual_left,  "< alpha_desired:", '\n')
  
  # Likewise for a right-sided rejection region we need to add 1 to get the first value IN the region
  # **** Note the 1-alpha
  critical_right = qbinom(1-alpha_desired, n, theta) + 1
  reject_region_right = critical_right:n
  cat('\ncritical_right:', critical_right, '\n')
  cat('reject_region_right:', reject_region_right, '\n') 
  alpha_actual_right = sum(dbinom(reject_region_right, n, theta))
  cat("Prob. in alpha_actual_right =", alpha_actual_right, "< alpha_desired", '\n')
  
  cat("\nNote the use of dbinom and the rejection region instead of pbinom. I find this helps me catch and avoid off-by-one errors.\n")
  cat("---------------\n")
}

#-----------------------------
if (do_topic2_in_operator) { # The %in% operator tells you if a value is in a list
  cat("2. The %in% operator\n")
  rejection_region = c(0,1,2,14,15,16)
  x = 5
  reject = x %in% rejection_region
  cat("x = ", x, ": Reject =", reject, '\n')
  
  x = 16
  reject = x %in% rejection_region
  cat("x = ", x, ": Reject =", reject, '\n')

  # You can even use it for checking if lists are in lists
  x = c(1,3,5,7,9)
  y = 4:8
  z = x %in% y
  print(z)
  cat("---------------\n")
}

#-----------------------------
if (do_topic3_prescribing_probabilities) { 
  # Sampling with prescribed probabilities
  cat("3. Sampling with prescribed probabilities\n")
  
  #--------------------------
  # REVIEW: Prescribed probabilities
  cat("----\nPrescribing the sampling probabilities.\n")
  prescribed_prob = c(1, 2, 3.5)
  range = c('A','B','C')
  # Choose 1 sample from the range with the prescribed probabilities
  # Note prescribedProb doesn't have to sum to 1. sample() will normalize it to 1. That is, it will choose (on average) values from range in the given proportions. So this example, the number of A, B, C will be roughly in proportin 1:2:3,5
  #
  x = sample(range, 1, prob=prescribed_prob)
  print(x)
  x = sample(range, 1, prob=prescribed_prob)
  print(x)
  x = sample(range, 1, prob=prescribed_prob)
  print(x)

  # You can also choose many samples at once
  x = sample(range, 8, prob=prescribed_prob, replace=TRUE)
  print(x)
  
  # REVIEW of rbinom ----
  # In cases where you want to sample from a known distribution, it is faster to use the the random sampler built for that distribution. For example, rbinom
  cat("----\nrbinom: simulating random binomial values.\n")
  # Toss a coin with 0.7 probability 10 times. Report the number of heads.
  x = rbinom(1, 10, 0.7)
  cat("Number of heads =", x, '\n')
  
  # You can simulate the 10 toss experiment many times, say 16.
  x = rbinom(16, 10, 0.7)
  print(x) 
}
