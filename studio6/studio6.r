# Studio 6 ----

# 1. Be sure to read the instructions file!

# 2. The instruction file gives instructions for testing your code. You can see the output of the tests in studio6-test-answers.html. You should use this to verify your code is giving correct output in the format asked for.

#--------------------------------------
# Setup ----
# We gathered data of money invested in advertisement from a 100 different companies.
# For the purpose of the studio, we will generate the total revenue of each company as 
# a normal random variable with variance sigam^2 and mean ax + b, where a and b are 'unknown' 
# and x is the money invested in advertisement. We will then use Bayesian linear regression to
# estimate a and b.

# DATA
# First we found data consisting collected from a 100 different companies. The following code loads that data. (Make sure your working directory is set to the source file directory.)
companiesData = read.csv('companies.csv')

x = companiesData$AdSpend

#-------------------------------
# Problem 1: Linear regression. ----
# See the instructions for this studio.

# 1a: Generate y values
studio6_problem_1a = function(x, a, b, sigma, draw_scatter_plot = TRUE) {
  cat("\n----------------------------------\n")
  cat("1a: generate y values and draw scatter plot if draw_scatter_plot = TRUE.\n")

  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********
  mean_values = a * x + b
  y = rnorm(length(x), mean_values, sigma)
  
  if (draw_scatter_plot) {
    plot(x, y, 
         main = paste("Scatter Plot of Revenue vs. Ad Spend"),
         xlab = "Advertisement Spending (millions $)", 
         ylab = "Revenue", 
         pch = 16,
         col = "black")
  }

  #uncomment the line below to return a variable named y
  return(y)
}

# 1b. Print your description of what happens as sigma increases
studio6_problem_1b = function() {
  cat("-----\n")
  cat("1b. Describe behavior as sigma increases\n")

  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********
  cat('As sigma increases, the data points become more scatted and spread out from the linear regression line. As sigma increases, the noise also increases, making the underlying linear relatonship less obvious.')

}  
  
# 1c: Compute and plot likelihood table
studio6_problem_1c = function(x_value, y_value, sigma, draw_likelihood_table = TRUE) {
  cat("-----\n")
  cat("1c. Compute and plot likelihood table if draw_likelihood_table = TRUE.\n")
  
  #We make the assumption that a and b are between -1 and 1
  aVals = seq(-1,1,.01)
  bVals = seq(-1,1,.01)
  
  # We initiate an empty likelihood table. Your code should fill in the values.
  likelihoodTable = matrix(NA, nrow=length(aVals), ncol=length(bVals))
  
  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********
  
  for (i in 1:length(aVals)) {
    for (j in 1:length(bVals)) {
      mean_value = aVals[i] * x_value + bVals[j]
      likelihoodTable[i,j] = dnorm(y_value, mean = mean_value, sd = sigma)
    }
  }
  
  if (draw_likelihood_table) {
    image(aVals, bVals, likelihoodTable, 
          col=terrain.colors(100),
          xlab = "a", 
          ylab = "b",
          main = paste("Likelihood table"))
  }

  # Return the likelihood table once everything is done:
  return(likelihoodTable)
  
}


# 1d: Find the and display the maximum likelihood estimate. ----
studio6_problem_1d = function(x, a, b, sigma) {
  cat("-----\n")
  cat("1d. Find the maximum likelihood estimate.\n")
  
  # we use the solution to problem 1a to get y values.
  y = studio6_problem_1a(x, a, b, sigma) 
  
  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********
  data = data.frame(x, y)
  lm = lm(y ~ x, data)
  
  print(coef(lm))
  
  abline(lm, col = "red")
  curve(a*x + b, from = 0, to = 5, col = "blue", add = TRUE)
  
  legend("topleft", legend=c("True Line", "MLE"),  
         fill = c("blue","red"))
  
}

# 1e. Print your description of what happens as sigma increases and the size of the data decreases
studio6_problem_1e = function() {
  cat("-----\n")
  cat("1e. Describe and explain behavior as sigma increases and when the size of the data decreases\n")
  
  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********
  
  cat('As sigma increases from 0 to 0.5, the data points become more scattered around the true regression line. When sigma = 0, all points lie exactly on the true line which results in a perfect MLE estimate. But as sigma increases, the MLE line deviates more. \n')
  cat('As the size of the data decreases, the MLE estimate becomes less reliable and more variable. \n')
  
}

# 1f. Print your description of what happens as sigma increases and the size of the data decreases
studio6_problem_1f = function() {
  cat("-----\n")
  cat("1f. Describe and explain behavior as sigma increases and when the size of the data decreases\n")
 
   # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********
  
  cat('As sigma increases, each observation provides less information, causing the posterior to potentially remain centered away from the true values. \n')
  cat('As the number of data points increases, the posterior distribution becomes centered around the true values. Eventually, the likelihood dominates the prior, and the distribution narrows. \n')
  
}


show_evolving_posteriors = function(x, sigma, a = 0.5, b = -0.3, sigma_a = 1, sigma_b = 1){
  
  # We assume that a = 0.5 and b = -0.3
  aVals = seq(-1,1,.01)
  bVals = seq(-1,1,.01)
  
  
  # Generate y values using problem 1a
  y = studio6_problem_1a(x, a, b, sigma, FALSE)
  
  # Distribution table will hold the posterior distribution after every update. 
  # We start with a centered binormal prior on a and b with standard deviations sigma_a and sigma_b
  distributiontable = matrix(NA, nrow=length(aVals), ncol=length(bVals))
  
  # Fill in the discretized values for the prior
  for (i in 1:length(aVals)) {
    for (j in 1:length(bVals)) {
      distributiontable[i,j] =  dnorm(aVals[i],0,sigma_a)*dnorm(bVals[j],0,sigma_b)
    }
  }
  
  # Normalize the values to form a probability distirbution
  normalize = sum(distributiontable)
  priortable = distributiontable/normalize
  
  # Display the prior using a heat map 
  hm_col_scale<-colorRampPalette(c("black","blue","green","orange","red"))(1000)
  image(aVals, bVals, distributiontable, xlab='a', ylab='b', col=hm_col_scale) 
  
  # Iteratively apply bayesian updates for each element in the data
  for (i in 1:length(x)){
    
    # Current (x,y) pair to update the prior with
    x_current = x[i]
    y_current = y[i]
    
    # Generate a likelihood table using problem 1c
    table = studio6_problem_1c(x_current,y_current, sigma, FALSE)
    
    # Update the posterior by multupling the prior by the likelihood
    for (i in 1:length(aVals)) {
      for (j in 1:length(bVals)) {
        distributiontable[i,j] =  distributiontable[i,j] * table[i,j]
      }
    }
    
    # Normalize by the Bayes factor
    normalize = sum(distributiontable)
    priortable = distributiontable/normalize
    
    # Display the posterior using a heat map 
    hm_col_scale<-colorRampPalette(c("black","blue","green","orange","red"))(1000)
    image(aVals, bVals, distributiontable, xlab='a', ylab='b', col=hm_col_scale) 
  }
}
