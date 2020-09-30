library(magrittr)

rabbits <- readr::read_csv("bp_rabbits.csv", col_types = readr::cols())

y <- rabbits$BPchange
X_reduced <- model.matrix(BPchange ~ Dose + Animal + Treatment, data = rabbits)
X_complete <- model.matrix(BPchange ~ Dose + Animal + Treatment + Animal:Treatment, data = rabbits)

####################################
## RSS calculation functions
beta_est <- function(X, y)
  solve(t(X) %*% X) %*% t(X) %*% y

y_est <- function(X, y)
  X %*% beta_est(X, y)

calc_RSS <- function(X, y)
  sum((y - y_est(X, y))^2)
####################################

obs_RSS <- calc_RSS(X_complete, y)

N <- 1000
set.seed(101)
perm_RSS <- 1:N %>% 
  lapply(function(i) {cbind(X_reduced, X_complete[sample(n), 8:11])}) %>% 
  sapply(calc_RSS, y)

p <- sum(perm_RSS < obs_RSS)/N
p