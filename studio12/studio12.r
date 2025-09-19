# Studio 12 (Climate Change) ----

# Be sure to read the instructions file!

#-------------------------------
# Problem 1
# See the instructions for this studio.

dataFile <- "~/Documents/18.05_studios/studio12/climate.csv"

climateData <- read.csv(dataFile, header = TRUE)
obs <- climateData$obs
ghg <- climateData$ghg
nat <- climateData$nat
aer <- climateData$aer
ant <- ghg + aer
years <- climateData$year

# convert the observations (which are anomalies with respect to the mean of the period 1961-1990) to be anomalies with
# respect to pre-industrial
refp <- 1850:1870
avgRef <- mean(obs[match(refp, years)])
obs <- obs - avgRef

# -------------------------------------
# Problem 1a: Plotting the observations and different forcings
# see the instructions for this studio.
studio12_problem_1a = function() {
  plot(
    years,
    obs,
    col = "black",
    type = "o",
    ylim = c(min(c(obs, ghg, aer, nat)), max(c(obs, ghg, aer, nat))),
    xlab = "Year",
    ylab = "Temperature anomaly [K]"
  )
  lines(years, ghg, col = "red")
  lines(years, aer, col = "blue")
  lines(years, nat, col = "green4")

  legend_labels = c("HadCRUT5", "hist-ghg", "hist-aer", "hist-nat")
  legend(
    "topleft",
    legend_labels,
    col = c("black", "red", "blue", "green4"),
    lty = rep(1, 6),
    pch = c(1, NA, NA, NA, NA, NA)
  )

  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********
  cat("1a: The reason for the upward trend in GHG can be attributed to the continuous increase in emissions from human activities since the industrial revolution. The NAT does not show any clear long-term trend (some fluctuations) which indicates that natural factors alone cannot explain the observed warning. The AER show a cooling effect especially after 1950, probably due to increased industrial pollution and human activities. This decrease seemed to slow down by 2000, maybe because of clean air regulations. ")
}

# -------------------------------------
# Problem 1b: Linear regression model 1
# see the instructions for this studio.
studio12_problem_1b = function() {
  # ********* YOUR CODE BELOW HERE ***********
  model = lm(obs ~ ghg + nat + aer)
  print(summary(model))

  cat("problem 1b:\n")
  cat("----------------------------------------\n")
  cat("R^2 =", summary(model)$r.squared, "\n")
  cat("This means that about", summary(model)$r.squared* 100, "% of the variance in observed temperature anomaly can be explained by GHG, NAT, and AER.\n")
  
  coefs = coef(model)
  cat("obs =", coefs[1], "+", coefs[2], "× ghg +", 
      coefs[3], "× nat +", coefs[4], "× aer\n")
  
  p_values = summary(model)$coefficients[, 4][-1]  # Exclude intercept
  var_names = c("ghg", "nat", "aer")
  least_sig_var = var_names[which.max(p_values)]
  
  cat("Least significant is", least_sig_var, "\n")
  cat("The reason that", least_sig_var, "is the least significant is probably because aerosols have a more regional effect on temperature compared to GHG. \n")
  

  # Do not change the line below.
  return(model)
}

# -------------------------------------
# Problem 1c: Linear regression model 2
# see the instructions for this studio.
studio12_problem_1c = function() {
  # ********* YOUR CODE BELOW HERE ***********
  model = lm(obs ~ nat + ant)
  print(summary(model))

  cat("problem 1c:\n")
  cat("----------------------------------------\n")
  cat("R^2 =", summary(model)$r.squared, "\n")
  cat("This means that approximately", summary(model)$r.squared * 100, "% of the variance in observed temperature anomaly can be explained by just the two forcing factors (NAT and ANT), adjusted for the number of predictors. \n")
  
  coefs = coef(model)
  cat("obs =", coefs[1], "+", coefs[2], "× nat +", coefs[3], "× ant\n")
  
  p_values =  summary(model)$coefficients[, 4][-1]  # Exclude intercept
  
  cat("Significance: Both natural and anthropogenic forcings are highly significant, which means that both factors play important roles in explaining observed temperature variations. The anthropogenic forcing is more significant (smaller p-value) which means that human activities play a large role in recent climate change. \n")
  

  # Do not change the line below.
  return(model)
}

# -------------------------------------
# Problem 1d: Plot fitted values
# see the instructions for this studio.
studio12_problem_1d = function() {
  GhgNatAer <- studio12_problem_1b()
  AntNat <- studio12_problem_1c()

  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********
  # extend the plotting code in problem 1a
  fitted_GhgNatAer = fitted(GhgNatAer)
  fitted_AntNat = fitted(AntNat)
  
  plot(
    years,obs, col = "black", type = "o",
    ylim = c(min(c(obs, ghg, aer, nat, fitted_GhgNatAer, fitted_AntNat)), 
             max(c(obs, ghg, aer, nat, fitted_GhgNatAer, fitted_AntNat))),
    xlab = "Year",
    ylab = "Temperature anomaly [K]"
  )
  
  lines(years, ghg, col = "red")
  lines(years, aer, col = "blue")
  lines(years, nat, col = "green4")
  
  lines(years, fitted_GhgNatAer, col = "purple", lwd = 2)
  lines(years, fitted_AntNat, col = "orange", lwd = 2)
  
  legend_labels = c("HadCRUT5 (Observed)", "hist-ghg", "hist-aer", "hist-nat", 
                    "Fitted (GHG+NAT+AER)", "Fitted (NAT+ANT)")
  legend(
    "topleft",
    legend_labels,
    col = c("black", "red", "blue", "green4", "purple", "orange"),
    lty = rep(1, 6),
    pch = c(1, NA, NA, NA, NA, NA),
    cex = 0.8
  )
}

# -------------------------------------
# Problem 1e: Attributable warming
# see the instructions for this studio.
studio12_problem_1e = function() {
  # ********* YOUR CODE BELOW HERE ***********
  model = lm(obs ~ ghg + nat + aer)
  coefs =coef(model)
  idx_2014 = which(years == 2014)

  attribution_ghg = coefs["ghg"] * ghg[idx_2014]
  attribution_nat = coefs["nat"] * nat[idx_2014]
  attribution_aer = coefs["aer"] * aer[idx_2014]
  
  cat("Attribution of ghg:", attribution_ghg, "degrees Celsius\n")
  cat("Attribution of nat:", attribution_nat, "degrees Celsius\n")
  cat("Attribution of aer:", attribution_aer, "degrees Celsius\n")
}
