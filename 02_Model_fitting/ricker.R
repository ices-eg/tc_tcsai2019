## 1  Import and explore data
cod <- read.csv("recruitment.csv")
cod
plot(cod, xlim=c(0,130), ylim=c(0,400))

## 2  Function to plot Ricker line
plot_ricker <- function(data, a, b, ...)
{
  S <- data[[1]]
  R <- data[[2]]
  plot(data, xlim=c(0,1.1*max(S)), ylim=c(0,1.1*max(R)))
  Sfit <- seq(0, 1.1*max(S), length=200)
  Rfit <- a * Sfit * exp(-b*Sfit)
  lines(Sfit, Rfit)
}
plot_ricker(cod, a=1, b=1)
## a is the initial slope
## a / exp(b) is maximum recruitment
plot_ricker(cod, a=5, b=0.01)

## 3  Function to fit model (estimate a and b)
ricker <- function(par, data)
{
  a <- exp(par[["loga"]])
  b <- exp(par[["logb"]])

  Rfit <- a * data$S * exp(-b*data$S)

  res <- log(data$R) - log(Rfit)
  sum(res^2)
}

## 4  Fit model

## Starting values
init <- c(loga=log(5), logb=log(0.01))

## Calculate RSS at starting values, before fitting model
ricker(par=init, data=cod)

## Fit model
optim(ricker, par=init, data=cod)
fit <- optim(ricker, par=init, data=cod)$par
ricker(par=fit, cod)  # RSS
fit  # estimated parameters

## Exponentiate
c(a=exp(fit[["loga"]]), b=exp(fit[["logb"]]))
