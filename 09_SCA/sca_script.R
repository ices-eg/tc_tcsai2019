source("sca_function.R")

## Read data

C <- as.matrix(read.csv("nscod_catage.csv", check.names=FALSE, row.names=1))
I <- as.matrix(read.csv("nscod_survey.csv", check.names=FALSE, row.names=1))
M <- as.matrix(read.csv("nscod_natmort.csv", check.names=FALSE, row.names=1))

data <- list(C=C, I=I, M=M)

## Set initial parameter values

par <- c(logNa=rep(8, ncol(C)),
         logNt=rep(8, nrow(C)),
         logFa=rep(0, ncol(C)-1),
         logFt=rep(0, nrow(C)),
         logQ=rep(-5, ncol(I)))

################################################################################

## Fit model

## Initial parameter values
sca(par, data, full=TRUE)
sca(par, data)

## Best parameter values
best <- nlminb(par, sca, data=data,
              control=list(eval.max=1000, iter.max=1000))
fit <- sca(best$par, data, full=TRUE)
fit

## View results

par(mfrow=c(2,2))

## 1 Population
round(fit$N)
Year <- as.integer(rownames(fit$N))
plot(apply(fit$N, 2, median), ylim=c(0, 400),
     yaxs="i", type="l", lty=3,
     main="Current population (bars) vs.\n median population (line)",
     xlab="Age", ylab="Individuals")
points(c(tail(fit$N, 1)), type="h", lwd=6)

## 2 Recruitment
barplot(fit$N[, 1], ylab="Individuals at age 1", main="Recruitment")

## 3 Selectivity
round(fit$F, 2)
plot(colMeans(fit$F) / max(colMeans(fit$F)),
     ylim=c(0, 1.05), yaxs="i", type="l",
     main="Selectivity", xlab="Age", ylab="Average F at age")

## 4 Fbar
Fbar2.4 <- rowMeans(fit$F[,2:4])
plot(Year[-length(Year)], Fbar2.4,
     ylim=c(0, 1.2), yaxs="i", type="l",
     main="Fbar (2-4)", xlab="Year", ylab="Average F at ages 2-4")
