df <- with(mtcars, data.frame(y=mpg, x1=disp, x2=hp, x3=wt))

nll_lm <- function(par, data, ...){
  y.hat <- par[1] + data[[2]]*par[2] + data[[3]]*par[3] + data[[4]]*par[4]
  res <- data[[1]] - y.hat
  -sum(dnorm(res, 0, sqrt(par[5]), log = TRUE))
}

params <- optim(par = c(mean(df$y), 0.3, 0.4, 0.5, 0.3), nll_lm, data = df, 
                lower = -Inf, upper = Inf, method = "L-BFGS-B")$par

params

X <- matrix(c(rep(1, 32), df[[2]], df[[3]], df[[4]]), 32, 4)
y <- df$y

beta_hat <- solve(crossprod(X), crossprod(X, y))

matrix(c(params[1:4], beta_hat), 4, 2)

sigma_hat <- sqrt(( crossprod(y) - crossprod(y, X) %*% beta_hat - 
                      crossprod(beta_hat, crossprod(X, y)) + 
                      crossprod(beta_hat, crossprod(X)) %*% beta_hat ) / 28)

c(params[5], sigma_hat)

hessian <- optim(par = c(mean(df$y), 0.3, 0.4, 0.5, 0.3), nll_lm, data = df, 
                 lower = -Inf, upper = Inf, method = "L-BFGS-B", hessian = TRUE)$hessian

sd_beta_hat <- sqrt(diag(hessian[-5, -5]))
sd_beta_hat



fit <- lm(y ~ x1 + x2 + x3, data = df)
summary(fit)
beta_hat_lm <- coef(fit)
sigma_hat_lm <- 2.639 #from the summary we get the value 

beta_hat_lm
sigma_hat_lm
