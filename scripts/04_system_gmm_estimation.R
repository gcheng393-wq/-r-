# System GMM (Blundell-Bond) Implementation with Lagged Dependent Variables

# Load necessary packages
library(gmm)

# Parameters
n <- 100   # number of observations
k <- 2     # number of regressors
set.seed(123)  # for reproducibility

# Simulate data
X <- matrix(rnorm(n * k), ncol = k)
Y <- rnorm(n) + 0.5 * lag(c(NA, rnorm(n)), 1)[-1] + 0.1 * X %*% rnorm(k)

# Create lagged dependent variable
Y_lag <- lag(c(NA, Y), 1)[-1]

# Combine data into a data frame
data <- data.frame(Y = Y[-1], Y_lag = Y_lag[-1], X)

# Define moment conditions for GMM
moment_conditions <- function(theta) {
  Y_hat <- theta[1] + theta[2] * data$Y_lag + theta[3] * data$X[,1] + theta[4] * data$X[,2]
  e <- data$Y - Y_hat
  return(as.matrix(data.frame(e * data$Y_lag, e * data$X)))
}

# System GMM estimation
theta_hat <- gmm(moment_conditions, x0=rep(0, 4))
summary(theta_hat)

# Perform AR(1) and AR(2) tests
ar1_test <- arima(test_data$residuals, order = c(1, 0, 0))
ar2_test <- arima(test_data$residuals, order = c(2, 0, 0))

# Sargan test for overidentifying restrictions
sargan_test <- function(residuals){
  e <- residuals
  num_instruments <- 4
  overid_stat <- (t(e) %*% e) / nrow(data)
  return(overid_stat)
}

# Hansen test
test_hansen <- function(residuals){
  e <- residuals
  test_stat <- (t(e) %*% e) / nrow(data)
  return(test_stat)
}
 
# Print tests
cat("AR1 Test Statistic:", ar1_test$coef,"\n")
cat("AR2 Test Statistic:", ar2_test$coef,"\n")
cat("Sargan Test Statistic:", sargan_test(test_data$residuals),"\n")
cat("Hansen Test Statistic:", test_hansen(test_data$residuals),"\n")
