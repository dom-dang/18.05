# for (n in 1:1060) {
#   left_int = qbinom(0.025,n,0.5) - 1
#   right_int = qbinom(0.975, n, 0.5) + 1
#   
#   rejection = c(0:left_int, right_int:n)
#   power = sum(dbinom(rejection, n, 0.55))
#   
#   if (power >= 0.9) {
#     print(n)
#   }
# }
# 
# # 
# 
# for (n in 0:20) { 
#   critical = qnorm(0.96, 40, 5/sqrt(n))
#   power = 1 - pnorm(critical, 45, 5/sqrt(n))
#   print(cat(str(n), " ", str(power)))
#   }


data=read.csv('/Users/dominiquedang/Downloads/climate-data-MA.csv')
plot(data)
lm = lm(AverageTempMA ~ Year, data) 
abline(lm)

early = subset(data, Year >= 1895 & Year <= 1914)
early_mean_value = mean(early$AverageTempMA) 
early_sample_variance = var(early$AverageTempMA)


later = subset(data, Year >= 2004 & Year <= 2023)
later_mean_value = mean(later$AverageTempMA)
later_sample_variance = var(later$AverageTempMA)

early_temp = subset(data, Year >= 1895 & Year <= 1914)$AverageTempMA
later_temp = subset(data, Year >= 2004 & Year <= 2023)$AverageTempMA
t_test_results = t.test(later_temp, early_temp, alternative = "greater", var.equal=TRUE)
print(t_test_results)



