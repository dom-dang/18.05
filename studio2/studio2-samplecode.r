#---------------------------------
# Sample code for 18.05 Studio 2
#---------------------------------

# R makes it easy to generate (pseudo) random binomial values using the function rbinom()
# The syntax is:  rbinom(ntrials, ntosses, phead)

# Generate 10 values from a Binomial(8,.7) random variable
x = rbinom(10, 8, 0.7)
cat('rbinom(10, 8, 0.7):', x, '\n')  

# Generate 15 values from a Binomial(8,.2) random variable
x = rbinom(15, 8, 0.2)
cat('rbinom(15, 8, 0.2):', x, '\n')  

# A Bernoulli(p) random variable is just a Binomial with ntosses = 1.
# Generate 10 values from a Bernoulli(.5) random variable
x = rbinom(10, 1, 0.5) # random sequence of 0s and 1s
cat('rbinom(10, 1, 0.5):', x, '\n')  

# You are now READY to do EXERCISE 1a

#---------------------------------------
# R has a function for computing 'n choose k'. 
x = choose(4, 2) #x = 6
cat('choose(4, 2):', x, '\n')  

# You are now READY to do EXERCISE 1b

#---------------------------------
# Computing binomial probabilities directly.
# Naturally R can do this:
#  dbinom(values, ntosses, phead ) is the probability mass function (pmf)
#  pbinom(values, ntosses, phead ) is the cumulative distribution function (cdf)

# For Y ~ Binomial(8,.6), compare with answer in exercise 1b.
#dbinom computes p(6) = P(Y = 6)
x = dbinom(6, 8, .6) # = 0.2090189
cat('dbinom(6, 8, 0.6):', x, '\n')  

# Values can be a vector of values, then dbinom returns a vector of probabilities
# This returns 9 probabilities (for k = 0,1,2,...,8)
x = dbinom(0:8, 8, 0.6)
cat('dbinom(0:8, 8, 0.6):\n')
print(x)
# [1] 0.00065536 0.00786432 0.04128768 0.12386304 0.23224320 0.27869184
# [7] 0.20901888 0.08957952 0.01679616

# pbinom is similar to dbinom except it returns cumulative probabilities
# pbinom(values, ntosses, phead)
#Compute P(Y <= 6) for Y ~ Binomial(8,.6). 
x = pbinom(6, 8, 0.6) # =  0.8936243
cat('pbinom(6, 8, 0.6):', x, '\n')  

# You can get a vector of probabilities. Note that for the cdf the probabilities
# increase to 1
cat('pbinom(0:8, 8, 0.6):\n')
x = pbinom(0:8, 8, 0.6)
print(x)
# [1] 0.00065536 0.00851968 0.04980736 0.17367040 0.40591360
# [6] 0.68460544 0.89362432 0.98320384 1.00000000

#-------------------------------------
# Simple plotting
x = 1:4 # Make the list (1,2,3,4)
y = x^2 + 2*x # Arithmetic works on lists just fine.

# plot with no other options plots points
plot(x, y)

# There are a ton of ways to pretty up the plot
plot(x, y, pch=19, col='red') #pch=19 says to use solid dots.

# You are now READY to do EXCERCISE 2a
# Using dbinom you are now READY to do EXCERCISE 2b
# Using rbinom you are READY to do EXCERCISE 2c

#-------------------------------------
# Permutations and derangements

# For this studio we will think of a permutation as n people getting 
# out of their seats, running around and then sitting down in a random seat. 

# You can use the sample function to get a permutation of 1:n
# Full permutation of 1:n
# RUN the next few lines of code several times
n = 5
x = sample(1:n, n)  # No replacement
print(x)

#A permutation is called a derangement if no one sits in their original seat.
#So 4123 is a derangement because no number is in its original position. But 1342 is not a derangement because the 1 is in position 1
# You can easily finding the number of people who sit back in their original seat
n = 5
x = 1:n
y = sample(x, n)
y == x  #This is a list of TRUE, FALSE, each TRUE is someone in their orig. seat
s = sum(y==x) #The number of TRUE, i.e. the number in their original seat
print(s)
# if s = 0 then the permutation was a derangement

#---------------------------------------
# for() loops

# Use a 'for loop' to estimate the average number of people who sit in
# their original seat
n = 9
x = 1:n
ntrials = 10000
nSame = 0  # we'll keep a running total of the people who sit in their original seat
for (j in 1:ntrials) {
  y = sample(x, n)
  nSame = nSame + sum(y == x)  #add in the number in this trial in their original seat
}
m = nSame/ntrials
print(m)

# You are now READY to do EXERCISE 3.

#-----------------------
# More code for the willing
# Using rep()
ntrials = 10
squares = rep(0, ntrials) #make a list of 0's  ntrials long
#'loop' through the sequence 1:ntrials
for (j in 1:ntrials){
    squares[j] = j^2     #store j^2 in squares[j]
}
print(squares)
# Note: there are better ways to produce this particular list, but this shows the use of rep()
#---------------------------------------
# set.seed()
# This is a very useful debugging tool. By setting the seed you get reproducible results from the random number generator.

# This code will always generate the same random numbers
# Run these two lines several times to see they always give the same sequence
# of 6 numbers. 
set.seed(1)
x = sample(1:5, 6, replace=TRUE) 
print(x)

# Now see what happens when you don't use set.seed(1) each time.
x = sample(1:5, 6, replace=TRUE) 
print(x)

#---------------------------------------
# table() and plot()

# Generate 500 Binomial(8,.7) random values.
# Then make a plot of the frequency of each value
phead = .6
ntosses = 8
ntrials = 2000
trials = rbinom(ntrials, ntosses, phead)

# table(trials) computes a frequency table:

# Comment: a common practice in R is to use dots in 
# names of variables
# For those of you used to a language with true classes this will seem odd  --and it is-- The dot doesn't have any meaning in R it is just another character in a name

trials.frequency = table(trials)   
print(trials.frequency)  
print(sum(trials == 5))
# This writes a table to the screen
# The first row are all the different values in trials
# The second row is the frequency for each value.
# Note that print(sum(trials == 5)) produces the same
# value for the frequency of 5 as the table

# Plot the table --plot() is smart enough to know how to plot the table
plot(trials.frequency, col='blue')     #

