# Calculating Yeild per recruit reference points in R

# Colin Millar

# This script shows the steps followed to Calculate
# yield per recruit reference points from the VPA fit.

#==============================================================================
#
# Use results from VPA assignment to estimate Fmax and F40%.
#
#==============================================================================

#------------------------------------------------------------------------------
# first lets rerun the assessment from yesterday:
#------------------------------------------------------------------------------

# get vpa function
source("../06_VPA/vpa_function.R")

# Read catch at age, change units
catage <- read.csv("../06_VPA/cod_catch.csv", check.names = FALSE, row.names = 1)

# Run model
vpafit <- calibrate(catage, M = 0.2, Fterm = 0.1, Fages = 3, Fyears = 3)

# inspect contents
str(vpafit)

#------------------------------------------------------------------------------
# (0) Model settings
#------------------------------------------------------------------------------

# ages and years
Amax <- 10
refA <- as.character(2:4)
refT <- "2015"

# growth parameters
Linf <- 119; k <- 0.147; t0 <- 0

# mean weight at age
alpha <- 0.0073
beta <- 3.07

# natural mortality rate
M <- 0.2

#------------------------------------------------------------------------------
# (1) Calculate partial recruitment
#------------------------------------------------------------------------------

a <- 1:Amax
Pa <- vpafit$F[refT,] / max(vpafit$F[refT,])

#------------------------------------------------------------------------------
# (2) Calculate mean lengh at age: La= L∞[1-e-k(a-to)]
#------------------------------------------------------------------------------

La <- Linf * (1 - exp(-k * (a - t0)))

#------------------------------------------------------------------------------
# (3) Calculate mean weight at age: wa = alpha * L^beta
#------------------------------------------------------------------------------

wa <- alpha * La^beta / 1000

#------------------------------------------------------------------------------
# (4) Assume an arbitrary number of recruits (N1=R=1000)
#------------------------------------------------------------------------------

R <- 1000

#------------------------------------------------------------------------------
# (5) Assume F=0 (for now)
#------------------------------------------------------------------------------

Fmult <- 0

#------------------------------------------------------------------------------
# (6) Calculate abundance at age: Na+1=Na exp(-(Pa*F + M))
#------------------------------------------------------------------------------

# set up N vector
Na <- numeric(Amax)

# assign age 1 and propagate down the ages
Na[1] <- R
for(i in seq_along(Na)[-1]) {
  Na[i] <- Na[i-1] * exp(-(Pa[i-1] * Fmult + M))
}

# have a quick peek
plot(a, Na, type = "b", ylim = c(0, 1000),
     main = "Population when F = 0 (no fishing)")

#------------------------------------------------------------------------------
# (7) Calculate catch at age
#------------------------------------------------------------------------------

Ca <- Na *
      Pa * Fmult / (Pa * Fmult + M) *
      (1 - exp(-M - Pa*Fmult))

#------------------------------------------------------------------------------
# (8) Calculate yield
#------------------------------------------------------------------------------

Y <- sum(Ca * wa)

#------------------------------------------------------------------------------
# (9) Calculate yield per recruit
#------------------------------------------------------------------------------

YPR <- Y / R

#------------------------------------------------------------------------------
# (10) Calculate spawning biomass
#------------------------------------------------------------------------------

# set up maturity
ma <- read.csv("../06_VPA/cod_maturity.csv", check.names = FALSE, row.names = 1)
ma <- unname(unlist(ma[refT,]))

# calculate ssb
SSB <- sum(Na * ma * wa)

#------------------------------------------------------------------------------
# (11) Calculate spawning biomass per recruit
#------------------------------------------------------------------------------

SPR <- SSB / R

#------------------------------------------------------------------------------
# (12) Save YPR and SPR results for the assumed F.
#------------------------------------------------------------------------------

# okay, so now we need to put this together into a recipe (function)
# In the simplest case we will need:
#   Fmult
#
# and other arguments get default values

ypr <- function(Fmult, M = 0.2, P = Pa, w = wa, mat = ma) {

  # first lets set up all the things we need
  #-----------------------------------------------------
  Amax <- length(w)
  N <- numeric(Amax)

  # now the modelling:
  #-----------------------------------------------------

  # (4) Assume an arbitrary number of recruits (N1=R=1000)
  R <- 1000

  # (6) Calculate abundance at age: N+1=N exp(-(P*Fmult + M))
  N[1] <- R
  for(i in seq_along(N)[-1]) {
    N[i] <- N[i-1] * exp(-(P[i-1] * Fmult + M))
  }

  # (7) Calculate catch at age:
  C <- N *
        P * Fmult / (P * Fmult + M) *
        (1 - exp(-M - P*Fmult))

  # (8) Calculate yield
  Y <- sum(C * w)

  # (9) Calculate yield per recruit:
  YPR <- Y / R

  # (10) Calculate spawning biomass
  SSB <- sum(N * mat * w)

  # (11) Calculate spawning biomass per recruit
  SPR <- SSB / R

  # format the results and return
  out <- c(YPR = YPR, SPR = SPR)
  out
}

# test the function
ypr(Fmult = 0)

# should be the same as:
c(YPR, SPR)

#------------------------------------------------------------------------------
# (13) Increase F by 0.1
#------------------------------------------------------------------------------

ypr(0.1)

#------------------------------------------------------------------------------
# (14) Repeat steps 6-12 up to F=1
#------------------------------------------------------------------------------

# set up all the Fs we want to try
?seq
Fsteps <- seq(0, 1, by = 0.1)

# calculate YPR and SPR for each F
results <- sapply(Fsteps, ypr)
results <- data.frame(Fmult = Fsteps, Fbar = Fsteps * mean(Pa[refA]), t(results))

#------------------------------------------------------------------------------
# (15) Plot YPR and SPR as a function of F
#------------------------------------------------------------------------------

Flab <- paste0("Average F (ages ", paste(range(refA), collapse="-"), ")")

# two plots on one page
par(mfrow = c(2, 1))

# ypr plot
plot(YPR ~ Fbar, data = results, type = "l", xlab = Flab, ylab = "Yield per recruit")

# spr plot
plot(SPR ~ Fbar, data = results, type = "l", xlab = Flab, ylab = "Spawners per recruit")

#------------------------------------------------------------------------------
# (16) Find Fmax
#------------------------------------------------------------------------------

# create a quick function, suitable for an optimiser
ypr_optim <- function(Fmult) {
  ypr(Fmult)["YPR"]
}

# test it works!
ypr_optim(1)

# now find the F that maximises YPR
opt <- optimize(ypr_optim, interval = c(0, 1), maximum = TRUE)
Fmax_mult <- opt$maximum
Fmax <- Fmax_mult * mean(Pa[refA])

#------------------------------------------------------------------------------
# (17) According to the VPA results and the YPR results, how does the historical
# fishing mortality compare to Fmax?
#------------------------------------------------------------------------------

Year <- as.integer(rownames(vpafit$F))
Fbar <- rowMeans(vpafit$F[,refA])
plot(Year, Fbar, type = "l", ylim = c(0, 1.1 * max(Fbar)), yaxs = "i",
     main = "Fmax", xlab = "Year", ylab = Flab)
abline(h = Fmax, col = "blue")



#------------------------------------------------------------------------------
# (18) Extra Credit: What is F40%?
#------------------------------------------------------------------------------

# use linear interpolation to get the value of F when SPR is 0.4 SPR when F = 0
results
SPR0 <- results$SPR[results$Fmult == 0.0]
apprx <- approx(results$SPR, Fsteps, xout = 0.4 * SPR0)
F40_mult <- apprx$y
F40 <- F40_mult * mean(Pa[refA])

# spr plot
plot(Fsteps, results$SPR, type = "b", main = "F40%",
     xlab = Flab, ylab = "Spawners per recruit", las = 1)
# a line showing F40
lines(c(F40, F40), c(0, 0.4 * SPR0), col = "red")
