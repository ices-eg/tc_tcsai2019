schaefer <- function(par, data, report=FALSE, plot=FALSE, ...)
{
  r <- exp(par[["logr"]])
  K <- exp(par[["logK"]])
  Binit <- exp(par[["logBinit"]])
  q <- exp(par[["logq"]])
  year <- data$Year
  C <- data$Catch
  I <- data$Index
  n <- length(year)
  B <- numeric(n)
  B[1] <- Binit
  for(i in 1:(n-1))
  {
    B[i+1] <- max(B[i] + r*B[i]*(1-B[i]/K) - C[i], 1e-4)
  }
  Ifit <- q * B

  res <- log(I) - log(Ifit)
  RSS <- sum(res^2)

  pars <- c(r=r, K=K, Binit=Binit, q=q)
  refpts <- c(HRmsy=0.5*r, Bmsy=0.5*K, MSY=0.25*r*K)
  HR <- C/B
  fit <- list(t=year, B=B, C=C, I=I, Ifit=Ifit, res=res,
              HR=HR, pars=pars, refpts=refpts, RSS=RSS)

  if(plot)
    plot_schaefer(fit, ...)

  if(report)
    fit
  else
    RSS
}

plot_schaefer <- function(fit, label="")
{
  if(label != "") label <- paste0(label, ":")

  opar <- par(mfrow=c(2,2)); on.exit(par(opar))

  plot(fit$t, fit$Ifit, ylim=c(0, 1.1*max(fit$I, fit$Ifit)),
       type="l", lwd=4, col="gray", yaxs="i",
       xlab="", ylab="Biomass index", main=paste(label, "Fit to data"))
  points(fit$t, fit$I)

  plot(fit$t, fit$B, type="l", ylim=c(0, 1.1*max(fit$B)), lwd=2, yaxs="i",
       xlab="", ylab="Biomass and catch",
       main=paste(label, "Biomass and catch"))
  points(fit$t, fit$C, type="h", lwd=6)

  plot(fit$t, fit$HR, ylim=c(0, 1.1*max(fit$HR)), type="l", lwd=2, yaxs="i",
       xlab="", ylab="Harvest rate", main=paste(label, "Harvest rate"))
  abline(h=fit$refpts["HRmsy"], lty=3)

  plot(fit$t, fit$res, ylim=c(-1.1,1.1)*max(abs(fit$res)),
       xlab="", ylab="Log residuals", main=paste(label, "Residuals"))
  abline(h=0)
}

################################################################################
## South Atlantic albacore

albacore <- read.table("albacore.dat", header=TRUE)
init <- c(logr=log(0.5), logK=log(200), logBinit=log(100), logq=log(0.5))

schaefer(init, albacore, plot=TRUE, label="Initial")
optim(init, schaefer, data=albacore)
est <- optim(init, schaefer, data=albacore)$par
fit <- schaefer(est, albacore, report=TRUE, plot=TRUE, label="Albacore")

fit$pars
fit$RSS
fit$refpts

################################################################################
## Georges Bank winter flounder

flounder <- read.table("flounder.dat", header=TRUE)
init <- c(logr=log(0.5), logK=log(30), logBinit=log(20), logq=log(0.1))

schaefer(init, flounder, plot=TRUE, label="Initial")
optim(init, schaefer, data=flounder)
optim(init, schaefer, data=flounder, method="Nelder-Mead",
      control=list(maxit=1e5, reltol=1e-10))
nlminb(init, schaefer, data=flounder, control=list(eval.max=1e4, iter.max=1e4))
est <- nlminb(init, schaefer, data=flounder,
              control=list(eval.max=1e4, iter.max=1e4))$par
fit <- schaefer(est, flounder, report=TRUE, plot=TRUE, label="Flounder")

t(fit$pars)
fit$RSS
t(fit$refpts)


################################################################################
## Cod

cod <- read.csv("cod.csv")
init <- c(logr=log(0.2), logK=log(3000), logBinit=log(2000), logq=log(0.1))

schaefer(init, cod, plot=TRUE, label="Initial")
optim(init, schaefer, data=cod)
optim(init, schaefer, data=cod, method="Nelder-Mead",
      control=list(maxit=1e5, reltol=1e-10))
nlminb(init, schaefer, data=cod, control=list(eval.max=1e4, iter.max=1e4))
est <- nlminb(init, schaefer, data=cod,
              control=list(eval.max=1e4, iter.max=1e4))$par
fit <- schaefer(est, cod, report=TRUE, plot=TRUE, label="Cod")

t(fit$pars)
fit$RSS
fit$refpts
