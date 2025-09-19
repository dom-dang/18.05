plot_binom_normal_base <- function(theta, n = 250) {
  x_vals <- 0:n
  mean_val <- n * theta
  var_exact <- n * theta * (1 - theta)
  var_conservative <- n * 0.25
  
  binom_pmf <- dbinom(x_vals, size = n, prob = theta)
  normal_exact <- dnorm(x_vals, mean = mean_val, sd = sqrt(var_exact))
  normal_conservative <- dnorm(x_vals, mean = mean_val, sd = sqrt(var_conservative))
  
  
  plot(x_vals, binom_pmf, type = "p", pch = 16, col = "black",
       main = paste("Binomial vs Normal (theta =", theta, ")"),
       xlab = "x", ylab = "Probability / Density", ylim = c(0, max(binom_pmf, normal_exact, normal_conservative)))
  
  
  lines(x_vals, normal_exact, col = "blue", lwd = 2)
  lines(x_vals, normal_conservative, col = "red", lwd = 2, lty = 2)
  
  legend("bottomright", legend = c("Binomial PMF", "Normal (Exact Var)", "Normal (Conservative Var)"),
         col = c("black", "blue", "red"), lty = c(NA, 1, 2), pch = c(16, NA, NA), lwd = c(NA, 2, 2))
}

# Plot for each value of theta
plot_binom_normal_base(0.5)
plot_binom_normal_base(0.3)
plot_binom_normal_base(0.1)
