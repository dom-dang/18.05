#-----------------------
# Studio 5 test script

# Before running: clean your environment and source('studio5.r')

#-----------------------
studio5_problem_0a()
studio5_problem_0b()
studio5_problem_0c()

#-----------------------
studio5_problem_1a()

prior = c(0.2, 0.2, 0.2, 0.2, 0.2)
studio5_problem_1b(prior, 8, TRUE)

# The following can be used to answer problem 1c.
prior = c(0.2, 0.2, 0.2, 0.2, 0.2)
studio5_problem_1b(prior, 20, FALSE)

prior = c(0.001, 0.001, 0.001, 0.001, 0.996)
studio5_problem_1b(prior, 20, FALSE)

studio5_problem_1c()
studio5_problem_1d()

#-----------------------
# Problem 2 is optional
studio5_problem_2a()

prior=c(.2,.2,.2,.2,.2)
studio5_problem_2b(prior, 30)
studio5_problem_2b(prior, 200)

#-----------------------
