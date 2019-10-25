## Read data
C <- as.matrix(read.csv("nscod_catage.csv", check.names=FALSE, row.names=1))
I <- as.matrix(read.csv("nscod_survey.csv", check.names=FALSE, row.names=1))
M <- as.matrix(read.csv("nscod_natmort.csv", check.names=FALSE, row.names=1))

minYear <- min(as.integer(rownames(C)))
maxYear <- max(as.integer(rownames(C)))
minAge <- min(as.integer(colnames(C)))
maxAge <- max(as.integer(colnames(C)))
nYears <- maxYear - minYear + 1
nAges <- maxAge - minAge + 1

## Prepare containers
N <- matrix(NA_real_, nrow=nYears+1, ncol=nAges,
            dimnames=list(minYear:(maxYear+1), minAge:maxAge))
F <- matrix(NA_real_, nrow=nYears, ncol=nAges, dimnames=dimnames(C))

## Set parameter initial values
logNa <- c(6.6, 5.7, 3.5, 3.1, 1.9, 3.0)
logNt <- c(7.4, 5.9, 7.4, 6.4, 6.0, 6.7, 5.8, 5.9, 6.6, 6.0, 6.8, 6.3, 5.9, 7.0,
           5.0, 5.6, 6.1, 5.0, 5.4, 4.8, 5.4, 5.2, 5.9, 5.3, 5.3, 5.3, 5.7, 5.0,
           5.4, 5.6, 5.8, 5.0, 4.6, 5.7, 4.0, 5.1)
logFa <- c(0.4, 1.6, 1.8, 1.6, 1.4)
logFt <- c(-1.9, -1.9, -1.9, -1.7, -1.9, -2.0, -1.8, -2.0, -1.9, -2.0, -2.0,
           -1.9, -2.0, -2.0, -2.0, -1.9, -1.8, -1.9, -2.4, -1.9, -2.6, -2.3,
           -2.4, -2.4, -2.5, -2.6, -2.7, -2.8, -3.0, -3.0, -3.1, -3.1, -3.0,
           -3.0, -2.8, -2.4)
logQ <- c(-4.9, -3.6, -3.0, -2.9, -2.8)

## Evaluate F, Z, and N
Fa <- exp(c(logFa, 0))
Ft <- exp(logFt)
F[] <- Ft %o% Fa
Z <- F + M
N[1,] <- exp(logNa)
N[-1,1] <- exp(logNt)

A <- ncol(N)
T <- nrow(N)
for(t in 1:(T-1))
{
  for(a in 1:(A-2))
  {
    N[t+1,a+1] <- N[t,a] * exp(-Z[t,a])
  }
  N[t+1,A] <- N[t,A-1] * exp(-Z[t,A-1]) + N[t,A] * exp(-Z[t,A])
}

## Predict C and I
Nc <- N[-nrow(N),]
Chat <- F/Z * Nc * (1-exp(-Z))
Chat <- Chat[rownames(Nc) %in% rownames(C), colnames(Nc) %in% colnames(C)]
Cres <- log(C) - log(Chat)

Ni <- N[rownames(N) %in% rownames(I), colnames(N) %in% colnames(I)]
Ihat <- sweep(Ni, 2, exp(logQ), "*")
Ires <- log(I) - log(Ihat)

## Evaluate likelihood
neglogL <- function(res)
{
  -sum(dnorm(res, sd=sqrt(mean(res^2)), log=TRUE))
}

c(catch=neglogL(Cres), survey=neglogL(Ires))
