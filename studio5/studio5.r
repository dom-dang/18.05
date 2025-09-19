# Studio 5 ----

# 1. Be sure to read the instructions file!

# 2. The instruction file gives instructions for testing your code. You can see the output of the tests in studio5-test-answers.html. You should use this to verify your code is giving correct output in the format asked for.

# 3. The handouts section (right side) of our MITx course page has a link for uploading you work.

#--------------------------------------
# Setup ----
# We have five types of dice: 4, 6, 8, 12, 20 sided.
# There is a prior distribution of the quantity of each die.
# One die is chosen at random and rolled repeatedly.
# Our job is to figure out which type of die was chosen.

DICE = c(4,6,8,12,20) # Fixed for the entire studio
DICE_TYPES = c('D4', 'D6', 'D8', 'D12', 'D20')

#-------------------------------
# Problem 0: List hypotheses and outcomes. ----
# See the instructions for this studio.

# 0a: List hypotheses.
studio5_problem_0a = function() {
  cat("\n----------------------------------\n")
  cat("Problem 0a: List hypotheses.\n")

  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********
  cat("The hypotheses for the Bayesian updating are the dice picked from the dice types (4, 6, 8, 12, 20 sided).")

}


# 0b: List outcomes.
studio5_problem_0b = function() {
  cat("-----\n")
  cat("0b. List outcomes.\n")

  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********
  cat("The possible outcomes of one roll of the chosen die are the numbers between 1-20.")
  
}


# 0c: Print likelihood table for one roll. ----
studio5_problem_0c = function() {
  cat("-----\n")
  cat("0c. Print likelihood table for one roll.\n")

  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********
  likelihood_table = matrix(0, nrow=5, ncol=20)
  rownames(likelihood_table) = DICE_TYPES
  colnames(likelihood_table) = 1:20
  
  likelihood_table[1,1:4] = 1/4
  likelihood_table[2,1:6] = 1/6
  likelihood_table[3,1:8] = 1/8
  likelihood_table[4,1:12] = 1/12
  likelihood_table[5,1:20] = 1/20
  
  print(likelihood_table, digits=5)

}

#---------------------------
# Problem 1: Updating ----

# 1a: Run studio5_sample_code_example_2.
studio5_problem_1a = function() {
  cat("\n----------------------------------\n")
  cat("Problem 1a. Run example 2.\n")

  # Nothing more to do.
  cat("Read the code and looked at the plots.\n")
}


# Problem 1b: Updates, bar plots, stacked bar plot.
studio5_problem_1b = function(prior,
                              nrolls,
                              plot_individual_posteriors=FALSE) {
  cat("-----\n")
  cat("1b. Updates, bar plots, stacked bar plot.\n")

  # Arguments:
  #  prior = prior probilities for the type of die use to generate data
  #  nrolls = number of rolls to simulate
  #  plot_individual_posteriors = whether or not to make individual bar charts

  # For this problem force the chosen die to be 8-sided
  random_die = 8
  data_rolls = sample(1:random_die, size=nrolls, replace=TRUE)

  cat('The initial prior is\n')
  print(prior, digits=4)
  cat('nrolls =', nrolls, '\n')
  
  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********
  likelihood_table = matrix(0, nrow=5, ncol=20)
  rownames(likelihood_table) = DICE_TYPES
  colnames(likelihood_table) = 1:20
  
  likelihood_table[1,1:4] = 1/4
  likelihood_table[2,1:6] = 1/6
  likelihood_table[3,1:8] = 1/8
  likelihood_table[4,1:12] = 1/12
  likelihood_table[5,1:20] = 1/20
  
  if (plot_individual_posteriors) {
    title = "Prior probabilities"
    barplot(prior, col='blue', width=rep(.1,5), xlim=c(0,3),
            space=3, names=DICE, main=title)
  }
  
  posterior_mat = matrix(NA, nrow=length(DICE), ncol=nrolls )
  
  prior.jroll = prior
  
  for (jroll in 1:nrolls){
    x.jroll = data_rolls[jroll]
    
    likelihood.jroll = likelihood_table[,x.jroll]
    bayes_numerator.jroll = prior.jroll * likelihood.jroll
    posterior.jroll = bayes_numerator.jroll/sum(bayes_numerator.jroll)
    
    posterior_mat[,jroll] = posterior.jroll
    
    if (plot_individual_posteriors) {
      title = paste("Posterior prob. after roll ",
                    jroll, ": outcome =", x.jroll)
      barplot(posterior.jroll, col='orange', width=rep(.1,5),
              xlim=c(0,3), space=3, names=DICE, main=title)
    }

    prior.jroll = posterior.jroll
  }
  
  all_probs = cbind(prior, posterior_mat)
  
  barplot(all_probs, names.arg=c("0(Prior)", c(1:nrolls)),
          col=rainbow(length(DICE)), border=NA, space=0)
  
  title(xlab="Number of Rolls")
  title("Stacked Barplot of Posterior Probabilities")
  
  legend("topleft", legend=paste("D", DICE, sep=""),
         col=rainbow(length(DICE)), pch=15, horiz=TRUE, cex=0.8)
  

  final_posterior = matrix(posterior_mat[,nrolls], nrow=1)
  colnames(final_posterior) = DICE_TYPES
  cat('The final posterior is\n')
  print(final_posterior, digits=4)
  
  cat('The true type of the chosen die is', random_die, 'sided\n')


  cat('See plots\n')
}

# Problem 1c: Compare and contrast. ----
studio5_problem_1c = function() {
  cat("-----\n")
  cat("1c. Compare and contrast.\n")

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********
  cat ("1. With a uniform prior, the posterior probabilities gradually converge to the correct hypothesis. After about 5 rolls,") 
  cat ("the posterior probability for D8 becomes dominant which shows that the data overcame the prior.")
  cat ("\n2. With a more skewed prior (more biased toward the 20 sided die), it takes more rolls to overcome the prior bias.") 
  cat ("The posterior only starts to favor D8 after many more rolls as the data must show strong evidence to counteract the initial bias towards D20. ")

  }

# Problem 1d: Too certain a prior. ----
studio5_problem_1d = function() {
  cat("-----\n")
  cat("1d. Too certain a prior.\n")

  # Prior probability of D8 = 0
  prior = c(0.25, 0.25, 0, 0.25, 0.25)
  cat('Running studio5_problem_1b\n')
  studio5_problem_1b(prior, 20, FALSE)

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  cat("When the prior probability of an 8 sided die is 0, no amount of data can convince us that the chosen die has 8 sides (just impossible). As a result, it gives the next most probable answer (D12) given the data")
}


#---------------------------
# Problem 2 (OPTIONAL): Censored data. ----

# Problem 2a (OPTIONAL): List hypotheses, make likelihood table
studio5_problem_2a = function() {
  cat("\n----------------------------------\n")
  cat("OPTIONAL 2a. List hypotheses, outcomes, make likelihood table.\n")


  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

}


# Problem 2b (OPTIONAL): Censored data. ----
studio5_problem_2b = function(prior, nrolls) {
  cat("-----\n")
  cat("OPTIONAL 2b: Censored data.\n")

  # Arguments:
  #  prior = prior probilities for the type of die use to generate data
  #  nrolls = number of rolls to simulate

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********



  cat('See plots\n')
}
