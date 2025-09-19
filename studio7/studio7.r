# Studio 7----

# 1. Be sure to read the instructions file!

# 2. The instruction file gives instructions for testing your code. You can see the output of the tests in studio6-test-answers.html. You should use this to verify your code is giving correct output in the format asked for.

#--------------------------------------
# Setup ----
# We will explore conjugate priors using beta-binomial and Dirichlet-categorical examples.

#-------------------------------
# Problem 1: Conjugate priors - Beta. ----
# See the instructions for this studio.

# 1a: Calculate the likelihood function, Bayes numerator, and approximate normalization to estimate for the posterior density.
studio7_problem_1a = function(a, b, n_heads, n_tails, theta, m) {
  cat("\n----------------------------------\n")
  cat("1a: Estimate the posterior density.\n")

  # Your code should change the values of these variables.
  likelihood = 0
  estimated_posterior_density = 0
  
  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********
  likelihood = choose (n_tails + n_heads, n_heads) *theta^n_heads * (1-theta)^n_tails
  
  bayes_numerator = likelihood * dbeta(theta, a, b)
  
  points = seq(1/m, 1, 1/m)
  likelihoods = sapply(points, function(p) {
    choose(n_tails + n_heads, n_heads) * p^n_heads * (1-p)^n_tails
  })
  
  priors = dbeta(points, a, b)
  approx_integral = sum(likelihoods * priors * 1/m)
  
  estimated_posterior_density = bayes_numerator/approx_integral
 
  
  cat('Likelihood:', likelihood, '\n')
  cat('Estimated posterior density', estimated_posterior_density, '\n')
}

# 1b. Compute and return the posterior density by using the exact formula of the conjugate posterior
studio7_problem_1b = function(a, b, n_heads, n_tails, theta) {
  cat("-----\n")
  cat("1b: Calculate the posterior density.\n")

  # Your code should change the values of this variable.
  posterior_density = 0
  
  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********
  posterior_density = dbeta(theta, a + n_heads, b + n_tails)
  
  return(posterior_density)
}

# 1c: Plot the posterior density
studio7_problem_1c = function(a, b, n_heads, n_tails) {
  cat("-----\n")
  cat("1c. Plot the posterior density.\n")
  
  # we plot the density between 0 and 1
  x = seq(0,1,.01)
  
  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********
  posterior_densities = dbeta(x, a + n_heads, b + n_tails)
  
  plot(x, posterior_densities, col='blue',
       main=paste("Posterior Density Graph"),
       xlab="x", ylab="y")
  
}

# 1d: Explain the shape of the posterior
studio7_problem_1d = function() {
  cat("-----\n")
  cat("1d. Explain the shape of the posterior.\n")
  

  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********
  
  cat('When the number of tosses grows, the posterior becomes more and more concentrated around the observed proportion of heads. The graph becomes narrower and more peaked.\n')
  cat('The difference between a flat prior and a biased prior lies in how quickly the posterior converges to the true proportion. \n')
  
}

#-------------------------------
# Problem 2: Conjugate priors - Dirichlet. ----
# See the instructions for this studio.

# 2a. Compute the posterior density by using the exact formula of the conjugate posterior
studio7_problem_2a = function(alphas, ns, probs) {
  cat("-----\n")
  cat("2a: Calculate the posterior density.\n")
  
  # Your code should change the values of this variable.
  posterior_density = 0
  
  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********
  posterior_alphas = alphas + ns
  
  posterior_density = ddirichlet(probs, posterior_alphas)
  
  cat('Posterior density', posterior_density, '\n')
}

# 2b: Plot a heat map for the posterior density
studio7_problem_2b = function(alphas, ns) {
  cat("-----\n")
  cat("2b. Plot the posterior density.\n")
  
  # we plot the density in the square [0,1] X [0,1]
  headsVals = seq(0,1,.01)
  tailsVals = seq(0,1,.01)
  table = matrix(0, nrow=length(headsVals), ncol=length(tailsVals))

  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********
  posterior_alphas = alphas + ns
  
  for (i in 1:length(headsVals)) { 
    for (j in 1:length(tailsVals)) {
      p1 = headsVals[i]
      p2 = tailsVals[j]
      p3 = 1- p1 - p2
      
      if (p3 >=0) {
        probablity = c(p1, p2, p3)
        table[i, j] = ddirichlet(probablity, posterior_alphas)
      } else { 
        table[i, j] = 0
        }
    }
  }
  
  image(headsVals, tailsVals, table,col = heat.colors(100),
        xlab = "p_1", ylab = "p_2",
        main = "Posterior Density Heat Map")
}

# 2c: Explain the shape of the posterior
studio7_problem_2c = function() {
  cat("-----\n")
  cat("2c. Explain the shape of the posterior.\n")
  
  
  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********
  
  cat('When the number of tosses grows, the posterior becomes more and more concentrated around the observed proportion of outcomes. The probablity mass becomes more focused in a smaller region, indicating a stronger certainty about the true population. \n')
  cat('The difference between a flat prior and a biased prior lies in how the intial behavior acts. With a flat prior, the posterior is mainly shaped by the observed data in the beginning. With a biased prior, it requires more observations to overcome this bias. \n')
  
}


ddirichlet =  function(x, alpha){
  if(abs(sum(x) - 1) > 1e-10){
    stop("sum(x) should be 1")
  } 
  if(any(x < 0)){
    return(0)
  }
  c = prod(gamma(alpha))/gamma(sum(alpha))
  return(1/c*prod(x^(alpha - 1)))
}
