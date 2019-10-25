library(icesTAF)          # download, write.taf
library(stockassessment)  # read.ices

download("https://www.stockassessment.org/datadisk/stockassessment/userdirs/user3/nscod19_ass01/data/cn.dat")
download("https://www.stockassessment.org/datadisk/stockassessment/userdirs/user3/nscod19_ass01/data/nm.dat")
download("https://www.stockassessment.org/datadisk/stockassessment/userdirs/user3/nscod19_ass01/data/survey.dat")
download("https://www.stockassessment.org/datadisk/stockassessment/userdirs/user3/nscod19_ass01/data/sw.dat")
download("https://www.stockassessment.org/datadisk/stockassessment/userdirs/user3/nscod19_ass01/data/mo.dat")

################################################################################
## 1  catage

cn <- as.data.frame(read.ices("cn.dat"))

## Start in 1983
cn <- cn[as.integer(rownames(cn)) >= 1983,]

## Divide by 1000
cn <- cn / 1000

## Calculate plus group
cn["6"] <- rowSums(cn[as.character(6:15)])
cn <- cn[as.character(1:6)]

## Round to 3 digits
cn <- round(cn, 3)

cn <- xtab2taf(cn)
write.taf(cn, "nscod_catage.csv")

################################################################################
## 2  natmort

nm <- as.data.frame(read.ices("nm.dat"))

nm <- nm[as.integer(rownames(nm)) >= 1983,]
nm <- head(nm, -1)  # don't need current year

nm <- nm[as.character(1:6)]

nm <- round(nm, 3)

nm <- xtab2taf(nm)
write.taf(nm, "nscod_natmort.csv")

################################################################################
## 3  survey

survey <- read.ices("survey.dat")
survey <- as.data.frame(survey[[1]])

survey <- survey / 1000

survey <- round(survey, 3)

survey <- xtab2taf(survey)
write.taf(survey, "nscod_survey.csv")

################################################################################
## 4  wcatch

sw <- as.data.frame(read.ices("sw.dat"))

sw <- sw[as.integer(rownames(sw)) >= 1983,]

sw <- sw[as.character(1:6)]

sw <- xtab2taf(sw)
write.taf(sw, "nscod_wstock.csv")

################################################################################
## 5  maturity

mo <- as.data.frame(read.ices("mo.dat"))

mo <- mo[as.integer(rownames(mo)) >= 1983,]

mo <- mo[as.character(1:6)]

mo <- round(mo, 3)

mo <- xtab2taf(mo)
write.taf(mo, "nscod_maturity.csv")

################################################################################

file.remove(c("cn.dat", "nm.dat", "survey.dat", "sw.dat", "mo.dat"))
