mean = 80
std = 10

scores <- seq(50, 110, by = 0.1)

pdf = dnorm(scores, mean, std)
plot(scores, pdf, main = "PDF", xlab = "Score", ylab = "Density")

cdf = pnorm(scores, mean, std)
plot(scores, cdf, main = "CDF", xlab = "Score", ylab = "Cumulative Probability")


#for part (ii)
print((1 - pnorm(85, mean, std))*100)

#for part (iii) googled about qnorm
print(qnorm(0.10, mean, std))

# for part (iv), we want the probability that the score is over 95 given 
# the score is over 85
over_85 = 1 - pnorm(85, mean, std)
print((1 - pnorm(95, mean, std))/over_85)
