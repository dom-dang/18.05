#-----------------------
# Studio 6 test script

# Before running: clean your environment and source('studio5.r')

#-----------------------
companiesData = read.csv('companies.csv')
x = companiesData$AdSpend

studio6_problem_1a(x[1:10], 2, 5, 0)
studio6_problem_1a(x[1:10], 2, 5, 0.5)
studio6_problem_1a(x[1:10], 2, 5, 5)

studio6_problem_1b()

x_value = x[1]
y_value = 0.2*x_value + 0.5

l = studio6_problem_1c(x_value, y_value, 0.1)
l = studio6_problem_1c(x_value, y_value, 0.8)

studio6_problem_1d(x, 0.2, 0.5, 0.1)
studio6_problem_1d(x, 0.2, 0.5, 0.5)

studio6_problem_1e()

show_evolving_posteriors(x[1:4], 0.1)
studio6_problem_1f()
#-----------------------
