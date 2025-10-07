####Cross Experimental Analyses####
####Set up####
##read in data
Ex1A = read.csv("Data/Ex1A.csv")
Ex1B = read.csv("Data/Ex1B.csv")

Ex2A = read.csv("Data/Ex2A.csv")
Ex2B = read.csv("Data/Ex2B.csv")

##combine
colnames(Ex2A)[2] = "encoding"
colnames(Ex2B)[2] = "encoding"

colnames(Ex2A)[4] = "direction"
colnames(Ex2B)[4] = "direction"

colnames(Ex2A)[6] = "score"
colnames(Ex2B)[6] = "score"

forward_M = rbind(Ex1A, Ex2A[ , -3])
backward_M = rbind(Ex1B, Ex2B[ , -3])

##Load libraries
library(ez)
library(reshape)
library(psychReport)

##turn off scientific notation
options(scipen = 999)

####Analyses####
##test whether reactivity patterns differ between experiments
##Start with "A" experiments
model1 = ezANOVA(forward_M,
                 wid = id,
                 between = .(encoding, experiment),
                 within = direction,
                 dv = score,
                 type = 3,
                 detailed = T)

model1$ANOVA$MSE = model1$ANOVA$SSd/model1$ANOVA$DFd
model1$ANOVA$MSE

aovEffectSize(model1, effectSize = "pes")

##Main effects
tapply(forward_M$score, forward_M$encoding, mean) #main effect of encoding group (sig)
tapply(forward_M$score, forward_M$experiment, mean) #main effect of experiment (non-sig)
tapply(forward_M$score, forward_M$direction, mean) #main effet of direction (sig)

##two-way interactions
tapply(forward_M$score, list(forward_M$encoding, forward_M$experiment), mean) #encoding x experiment (non-sig)
tapply(forward_M$score, list(forward_M$encoding, forward_M$direction), mean) #encoding x direction (sig)
tapply(forward_M$score, list(forward_M$experiment, forward_M$direction), mean) #experiment x direction (sig)

##three-way (non-sig)
forward_M_JOL = subset(forward_M, forward_M$encoding == "JOL")
forward_M_R = subset(forward_M, forward_M$encoding == "Read")

tapply(forward_M_JOL$score, list(forward_M_JOL$experiment, forward_M_JOL$direction), mean)
tapply(forward_M_R$score, list(forward_M_R$experiment, forward_M_R$direction), mean)

###Pos-hocs (break down the two-way interactions)

####Now for the "B" experiments####