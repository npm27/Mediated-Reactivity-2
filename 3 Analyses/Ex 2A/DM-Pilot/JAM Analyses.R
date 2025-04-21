####Set up####
##read in the data
dat = read.csv("JAMs_cleaned.csv")

##Load libraries
library(ez)
library(psychReport)
library(reshape)

##turn off scientific notation
options(scipen = 999)

##fix column names
colnames(dat)[4:5] = c("Direction", "JAM")

##drop out of range JAMs
dat$JAM[dat$JAM > 100] = NA

##drop missing
dat = na.omit(dat)

####descriptives####
tapply(dat$JAM, dat$Direction, mean)

##anyone not follow task instructions?
dat.long = cast(dat, Username ~ Direction, mean) #looks good!

####Run an anova####
model = ezANOVA(dat,
                wid = Username,
                between = Direction,
                dv = JAM,
                type = 3,
                detailed = T)

model #significant. No surpise there

model$ANOVA$MSE = model$ANOVA$SSd/model$ANOVA$DFd
model$ANOVA$MSE

aovEffectSize(model, effectSize = "pes")

####Post-hocs####
#F vs M
temp = t.test(dat.long$F, dat.long$M, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #sig!
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

#d 3.19

#F vs U
temp = t.test(dat.long$F, dat.long$U, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #sig!
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

#d 3.39

#U vs M
temp = t.test(dat.long$M, dat.long$U, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #Non-sig
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

#d 0.22

####get means and sd for cohen's d####
apply(dat.long, 2, mean)
apply(dat.long, 2, sd)
