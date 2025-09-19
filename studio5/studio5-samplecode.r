#---------------------------------
# Sample code for 18.05 Studio 5
#--------------------------------
# K-Sided Dice Example from Lecture 11

# Random process: choose among K-sided Dies at random and roll the die
# Conduct Bayesian analysis inferring the chosen die type using outcomes of sample rolls

# For this sample code we will assume there are 3 types of dice. In the studio we will allow 5 types of dice
DICE = c(4,6,8)
DICE_TYPES = c('D4', 'D6', 'D8')

#--------------------------------
# Example 1: Updating after one roll
#--------------------------------
cat('---------------------\n')
cat('Example 1\n')
# 4, 6 and 8 sided dice, equally likely to be chosen
prior = c(1/3, 1/3, 1/3)

# Build likelihood table: 3 dice x 8 possible outcomes
# First create the table with all 0's
likelihood_table = matrix(0, nrow=3, ncol=8)
# Name the rows and columns
rownames(likelihood_table) = DICE_TYPES
colnames(likelihood_table) = 1:8

# Make the probabilities (rows) for each die separately.
# (We could have done this in a loop)
# 4-sided die; outcomes 1-4 are set to 1/4;
# outcomes 5-8 are left at 0. The other dice are similar
likelihood_table[1,1:4] = 1/4
likelihood_table[2,1:6] = 1/6
likelihood_table[3,1:8] = 1/8

print(likelihood_table, digits=3)

# We can make a bar plot of the prior probabilities
title = "Prior probabilities"
barplot(prior, col='blue', width=rep(.1,5),
        xlim=c(0,3),space=3, names=DICE, main=title)

# Choose die according to prior distribution
random_die = sample(DICE, 1, prob=prior)
cat('random_die:', random_die, '\n')

# roll the die once
x = sample(1:random_die, size=1, replace=TRUE)
print(x)

# Choose the likelihood column for x
likelihood = likelihood_table[,x]
print(likelihood)

# Do an update
bayes_numerator = prior*likelihood
posterior = bayes_numerator/sum(bayes_numerator)

# Print a bar plot of the
title = paste("Posterior probabilities after one roll: outcome =", x)
barplot(posterior, col='orange', width=rep(.1,5),
        xlim=c(0,3), space=3, names=DICE, main=title)

#-------------------------------------
# Storing the entire table in a data frame
# This is not used in studio 5. We left it in
# the sample code in case you're interested

# print(bayes_table) will give you a nice
# printout of our usual Bayesian update table

# Data frames have named columns, the syntax is name = values.
# So dice = DICE creates a column named 'dice' with values
# from the DICE vector.
# (They don't need to be the same name, e.g.
# 'platonicSolids' = DICE would create a column
# called 'platonicSolids' with values from the DICE
# vector.)

bayes_table=data.frame(
  dice=DICE,
  prior=prior,
  likelihood=likelihood,
  bayesNumer=bayes_numerator,
  posterior=posterior
)
title = paste("Bayes table after one roll: roll =", x)
print(title)
print(bayes_table)


#--------------------------------
# Example 2: Roll the chosen die multiple times
#--------------------------------
plot_individual_posteriors = TRUE

cat('---------------------\n')
cat('Example 2\n')

# To keep this part self-contained we repeat the setup above
nrolls = 20  #roll the die nrolls times
prior = c(1/3, 1/3, 1/3)

# Build likelihood table: 3 dice x 8 possible outcomes
# First create the table with all 0s
likelihood_table = matrix(0, nrow=3, ncol=8)
# Make the probabilities (rows) for each die separately.
# (We could have done this in a loop)
# 4-sided die; outcomes 1-4 are set to 1/4;
# outcomes 5-8 are left at 0. The other dice are similar
likelihood_table[1,1:4] = 1/4
likelihood_table[2,1:6] = 1/6
likelihood_table[3,1:8] = 1/8

# Choose die according to prior distribution
random_die = sample(DICE, 1, prob=prior)

# Get all the data at once
data_rolls = sample(1:random_die, size=nrolls, replace=TRUE)

if (plot_individual_posteriors) {
  # Plot the prior
  title = "Prior probabilities"
  barplot(prior, col='blue', width=rep(.1,5), xlim=c(0,3),
          space=3, names=DICE, main=title)
}

# Initialize matrix whose jth column will store
# the posterior distribution after updating the jth roll
# We will use this to make a nifty stacked bar plot at the end
posterior_mat = matrix(NA, nrow=length(DICE), ncol=nrolls )

# NOTATIONAL WARNING: In R it is common to use dot notation.
# For example, prior.jroll. If you have used other languages
# you might think this means prior is a class or some other
# structure and jroll is a property of prior. This is NOT
# the case. The dot is just a character that can be used
# in variable names.
# Set the first prior
prior.jroll = prior

# Go throught the update process for each roll
for (jroll in 1:nrolls){
  x.jroll = data_rolls[jroll]

  likelihood.jroll = likelihood_table[,x.jroll]
  bayes_numerator.jroll = prior.jroll * likelihood.jroll
  posterior.jroll = bayes_numerator.jroll/sum(bayes_numerator.jroll)

  # store the posterior
  posterior_mat[,jroll] = posterior.jroll

  if (plot_individual_posteriors) {
    # plot the posterior
    title = paste("Posterior prob. after roll ",
                  jroll, ": outcome =", x.jroll)
    barplot(posterior.jroll, col='orange', width=rep(.1,5),
            xlim=c(0,3), space=3, names=DICE, main=title)
  }

  # SET THE PRIOR for the next roll
  prior.jroll = posterior.jroll
}
# NOTE: If plot_individual_posteriors = TRUE, there should now be a sequence of barplots -one for each roll-- in the plot window

# STACKED BARPLOT of the prior/posterior distributions
# as a function of the number of rolls
# (cbind --column bind is a easy way to add columns to a matrix)
all_probs = cbind(prior, posterior_mat)

barplot(all_probs, names.arg=c("0(Prior)", c(1:nrolls)),
        col=rainbow(length(DICE)), border=NA, space=0)

title(xlab="Number of Rolls")
title("Stacked Barplot of Posterior Probabilities")

# Add a legend to the plot
legend("topleft", legend=paste("D", DICE, sep=""),
       col=rainbow(length(DICE)), pch=15, horiz=TRUE, cex=0.8)

# This code will format and print the final posterior
final_posterior = matrix(posterior_mat[,nrolls], nrow=1)
colnames(final_posterior) = DICE_TYPES
cat('The final posterior is\n')
print(final_posterior, digits=4)

cat('The true type of the chosen die is', random_die, 'sided\n')

cat('See plots\n')
