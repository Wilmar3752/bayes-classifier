library(ggplot2)
## Distribution for y
theta <- 0.9
y <- rbinom(100, 1, theta)
n1 <- sum(y == 1)
n2 <- sum(y == 0)
## Distribution for x given y=0
mu0 <- 15
sigma0 <- 3
## Distribution for x given y=1
mu1 <- 5
sigma1 <- 3

## Create a Simulated Dataframe
xy0 <- rnorm(n2, mu0, sigma0)
xy1 <- rnorm(n1, mu1, sigma1)
sim_df <- data.frame(rbind(cbind(xy0, 0), cbind(xy1, 1)))
colnames(sim_df) <- c("x", "y")

## The calculus from y given x distribution
ygx <- function(y, x) {
  if (y == 0) {
    mu <- mu0
    sigma <- sigma0
  } else if (y == 1) {
    mu <- mu1
    sigma <- sigma1
  }
  fx <- dnorm(x, mu, sigma) * (theta^y) * (1 - theta)^(1 - y)
  return(fx)
}


## Calcule argmax
argmax <- function(x) {
  if (ygx(1, x) < ygx(0, x)) {
    return(0)
  }else {
    return(1)
  }
}

sim_df["y_pred"] <- apply(sim_df["x"], FUN = argmax, MARGIN = 1)

head(sim_df)

## Confusion
table(sim_df$y, sim_df$y_pred)


funcion_objetivo <- function(x) {
  abs(ygx(y = 1, x = x) - ygx(y = 0, x = x))
}
resultado <- optim(par=(mu0+mu1)/2,fn = funcion_objetivo, method='L-BFGS-B', lower=min(mu0,mu1), upper=max(mu0,mu1))
resultado

ygx(1, resultado$par) / ygx(0, resultado$par)

hist(sim_df$x)


plot(x=-10:30,y=ygx(y=1,-10:30), type = 'l')

points(x=-10:30,ygx(y=0,-10:30), type = 'l', col="red")
abline(v=resultado$par, lty=2, col="blue")
