n = 5
alpha = 0.05
a = 1

expected_widths = numeric(n)

for (k in 1:n) {
  q_low = qbeta(alpha / 2, shape1 = k, shape2 = n - k + 1)
  q_high = qbeta(1 - alpha / 2, shape1 = k, shape2 = n - k + 1)
  
  expected_xk = a * k / (n + 1)
  expected_width = expected_xk * (1 / q_low - 1 / q_high)
  
  expected_widths[k] = expected_width
}

names(expected_widths) = paste0("k=", 1:n)
print(expected_widths)
