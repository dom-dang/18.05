alpha = 44
beta = 2.2

mode = (alpha - 1) / beta  

find_interval <- function(alpha, beta, mode, prob = 0.8) {
  objective <- function(eps) {
    lower = mode - eps
    upper = mode + eps
    if (lower < 0){
      return(1)
    }
    p = pgamma(upper,alpha, beta) - pgamma(lower, alpha, beta)

    return(abs(p - prob))
  }

  eps_opt <- optimize(objective, c(0.001, 50))$minimum
  c(mode - eps_opt, mode + eps_opt)
}
