library(ggplot2)
set.seed(42)

#constants
alpha <- 3
delta <- -2
lambda <- .5
epsilon <- c(runif(10000, -1, 1))
eta <- c(rexp(10000))

X1 <- c(rnorm(10000))
X2 <- c(alpha + .2 * X1 + epsilon)
Y <- c(delta + 1.2 * X1 + eta)
  
df <- data.frame(alpha, delta, epsilon, eta, lambda, X1, X2, Y)
head(df)

model <- lm(Y~X1 + X2, data=df)
#the values and standard error of delta, B1, and B2, are found in the summary 
#under (Intercept), X1, and X2
summary(model)

deltas <- vector("numeric", 1000)
beta1s <- vector("numeric", 1000)
beta2s <- vector("numeric", 1000)

for(i in 1:1000) {
  nX1 <- c(rnorm(100))
  nX2 <- c(alpha + .2 * nX1 + epsilon)
  nY <- c(delta + 1.2 * nX1 + eta)
  ndf <- data.frame(alpha, delta, epsilon, eta, lambda, nX1, nX2, nY)
  nmodel <- lm(nY~nX1 + nX2, data = ndf)
  matrix_coef <- summary(nmodel)$coefficients
  my_estimates <- matrix_coef[ , 1]
  deltas[i] = my_estimates[1]
  beta1s[i] = my_estimates[2]
  beta2s[i] = my_estimates[3]
}

hist(deltas)
hist(beta1s)
hist(beta2s)



