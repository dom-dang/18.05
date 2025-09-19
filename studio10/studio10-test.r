#-----------------------
# Studio 10 test script

# Before running: clean your environment and source('studio10.r')
source('./studio10-solutions.r')

#-----------------------
theta_vals =  c(0, 0.2, 0.4, 0.6, 0.8, 1)
theta_prior = c(0.02, 0.02, 0.02, 0.7, 0.2, 0.04)
n_trials = 10000
sigma = 2
n_data = 256
confidence = 0.95

studio10_problem_1a(theta_vals, theta_prior, sigma, n_data, confidence, n_trials)
studio10_problem_1b(theta_vals, theta_prior, sigma, n_data, confidence, n_trials)

xbar = 0.2
studio10_problem_1c(theta_vals, theta_prior, sigma, n_data, confidence, xbar)


#-----------------------
studio10_problem_2(0.55, 400)
