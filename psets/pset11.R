x <- c(0, 1, 2, 3, 4)
y <- c(0, 1, 4, 9, 16)
data <- data.frame(x, y)

pearson_cor <- cor(x, y, method = "pearson")

# Linear regression
model <- lm(y ~ x, data = data)

plot(x, y, pch = 19, col = "blue", 
     main = "Least Squares Line and Pearson Correlation",
     xlab = "x", ylab = "y", xlim = c(0, 4), ylim = c(0, 18))
abline(model, col = "red", lwd = 2)
legend("topleft", legend = c("Data", "Least Squares Line"),
       col = c("blue", "red"), pch = c(19, NA), lty = c(NA, 1))

text(1, 15, paste("Pearson correlation:", round(pearson_cor, 3)), col = "black")
