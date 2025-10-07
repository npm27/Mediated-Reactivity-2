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
model2 = ezANOVA(backward_M,
                 wid = id,
                 between = .(encoding, experiment),
                 within = direction,
                 dv = score,
                 type = 3,
                 detailed = T)

model2$ANOVA$MSE = model2$ANOVA$SSd/model2$ANOVA$DFd
model2$ANOVA$MSE

aovEffectSize(model2, effectSize = "pes")

backward_M_JOL = subset(backward_M, backward_M$encoding == "JOL")
backward_M_R = subset(backward_M, backward_M$encoding == "Read")

####T test comparisions for GD####
##Forward Experiments
forward_MR.wide.1 = cast(subset(forward_M_R,
                                forward_M_R$experiment == "Ex1A"), id ~ direction, value = "score", mean)
forward_MR.wide.2 = cast(subset(forward_M_R,
                                forward_M_R$experiment == "Ex2A"), id ~ direction, value = "score", mean)

##backward experiments
backward_MR.wide.1 = cast(subset(backward_M_R,
                                backward_M_R$experiment == "Ex1B"), id ~ direction, value = "score", mean)
backward_MR.wide.2 = cast(subset(backward_M_R,
                                backward_M_R$experiment == "Ex2B"), id ~ direction, value = "score", mean)

##run the t.tests
#As
temp = t.test(forward_MR.wide.1$M, forward_MR.wide.2$M, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #sig!
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

mean(forward_MR.wide.1$M); mean(forward_MR.wide.2$M)
sd(forward_MR.wide.1$M); sd(forward_MR.wide.2$M)

pbic1 = forward_MR.wide.1[ c(1, 3)]
pbic2 = forward_MR.wide.2[ c(1, 3)]

pbic1$ex = rep(1)
pbic2$ex = rep(2)

pbic3 = rbind(pbic1, pbic2)

ezANOVA(pbic3,
        wid = id,
        between = ex,
        dv = M,
        type = 3,
        detailed = T)

#Bs
temp = t.test(backward_MR.wide.1$M, backward_MR.wide.2$M, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic 
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

mean(backward_MR.wide.1$M); mean(backward_MR.wide.2$M)
sd(backward_MR.wide.1$M); sd(backward_MR.wide.2$M)

pbic1 = backward_MR.wide.1[ c(1, 3)]
pbic2 = backward_MR.wide.2[ c(1, 3)]

pbic1$ex = rep(1)
pbic2$ex = rep(2)

pbic3 = rbind(pbic1, pbic2)

ezANOVA(pbic3,
        wid = id,
        between = ex,
        dv = M,
        type = 3,
        detailed = T)
