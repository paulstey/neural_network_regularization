library(MASS)
library(glmnet)

# Sample size and number of predictors
n <- 100
p <- 4

# Variance-covariance matrix
sigma <- matrix(0, nrow = p, ncol = p)
diag(sigma) <- 1

# Predictors 1 and 2 are highly correlated
rho12 <- 0.99
sigma[1, 2] <- rho12
sigma[2, 1] <- rho12


mu <- rep(0, p)                 # mean vector for predictors
beta <- c(1, 1, 1, 4, 4)        # vector of regression coeffs (to be recovered)
epsilon <- rnorm(n)             # Gaussian error

X <- mvrnorm(n, mu, sigma)      # predictor data matrix
X <- cbind(1, X)                # add column of 1s for intercept
y <- X %*% beta + epsilon       # outcome vector

d <- data.frame(cbind(y, X))
names(d) <- c("y", "x0", "x1", "x2", "x3", "x4")

# fit linear model (OLS)
fm1 <- lm(y ~ 1 + x1 + x2 + x3 + x4, d)       
fm1                             # solution is unstable



# Using ridge regression
Xnew <- as.matrix(d[, 3:ncol(d)])
fm2 <- glmnet(Xnew, y, alpha = 0, lambda = 0.8, standardize = FALSE)
fm2$beta



