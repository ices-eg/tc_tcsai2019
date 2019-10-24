source("vpa_function.R")

## Import data
catage <- read.csv("cod_catch.csv", check.names=FALSE, row.names=1) / 1000
wcatch <- read.csv("cod_weights.csv", check.names=FALSE, row.names=1)
maturity <- read.csv("cod_maturity.csv", check.names=FALSE, row.names=1)

## Run model
vpa(catage, M=0.2, Fterm=0.1, Fages=3)
calibrate(catage, M=0.2, Fterm=0.1, Fages=3, Fyears=3)
calibrate(catage, M=0.2, Fterm=0.001, Fages=3, Fyears=3)
calibrate(catage, M=0.2, Fterm=10, Fages=3, Fyears=3)
model <- calibrate(catage, M=0.2, Fterm=0.1, Fages=3, Fyears=3)
Year <- as.integer(rownames(model$N))

## View results
par(mfrow=c(3,2))

## 1  Recruitment
round(model$N, 1)
barplot(model$N[,1], main="Recruitment", xlab="Year",
        ylab="Numbers at age 1 (millions)")

## 2  Catch
Y <- rowSums(catage * wcatch)
barplot(Y, main="Catch", xlab="Year", ylab="Annual catch (kt)")

## 3  SSB
SSB <- rowSums(model$N * wcatch * maturity)
plot(Year, SSB, ylim=c(0,1.1*max(SSB)), main="Stock size", ylab="SSB (kt)",
     type="l")

## 4  Fbar
round(model$F, 1)
Fbar <- rowMeans(model$F[,as.character(2:4)])
plot(Year, Fbar, ylim=c(0,1.1*max(Fbar)), main="Fishing mortality",
     ylab="Mean F (ages 2-4)", type="l")

## 5  Current population
barplot(model$N[nrow(model$N),], main="Current population", xlab="Age",
        ylab="Numbers at age (millions)")

## 6  Selectivity
plot(colMeans(model$F)/max(colMeans(model$F)), ylim=c(0,1.05),
     type="l", main="Selectivity", xlab="Age", ylab="Average F scaled to 1")
