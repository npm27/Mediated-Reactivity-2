####set up####
##read in data
JOL = rbind(read.csv("MSU/JOL Scored MSU.csv"), read.csv("USM/JOL Scored USM.csv"))
Read = rbind(read.csv("MSU/Read Scored MSU.csv"), read.csv("USM/Read Scored USM.csv"))

table(JOL$source) / 90 + table(Read$source) / 90

##load libraries
library(reshape)
library(ez)
library(psychReport)

options(scipen = 999)

##set up the dataframe
JOL = JOL[ , -c(2:4, 6, 8, 11, 14)]
Read = Read[ , -c(2:4, 6, 8, 11, 13)]

##get ns
length(unique(JOL$id)) #60
length(unique(Read$id)) #60

colnames(JOL)[3] = "Encoding_Group"
colnames(JOL)[4] = "Block"
colnames(JOL)[5] = "Direction"
colnames(JOL)[7] = "JOL"

colnames(Read)[3] = "Encoding_Group"
colnames(Read)[4] = "Block"
colnames(Read)[5] = "Direction"

#fix out of range JOLs
JOL$JOL[JOL$JOL > 100] = NA

##check for outliers
#recall
JOL.wide = cast(JOL, id ~ Direction, value = "Scored", mean)
Read.wide = cast(Read, id ~ Direction, value = "Scored", mean) #both look okay!

##anyone not do the JOL task?
JOL.wide2 = cast(JOL, id ~ Direction, value = "JOL", mean, na.rm = T) #2 participants who didn't follow JOL instructions

#drop them
JOL = subset(JOL,
             id != "M20297698AL" & id != "M20298373")

Read = subset(Read,
              id != "w10182202_WKK" & id != "M20339934.HG" & id != "W10126357_MJ")

##put everything together
combined = rbind(JOL[ , -7], Read)

combined = combined[ , c(1, 3:5, 6, 2)]

####Descriptives####
tapply(combined$Scored, list(combined$Encoding_Group, combined$Direction), mean) #reactivity patterns (recall)
tapply(JOL$JOL, JOL$Direction, mean, na.rm = T)

##test JOLs
JOL2 = na.omit(JOL)

mod.jol = ezANOVA(JOL2,
                  wid = id,
                  dv = JOL,
                  between = Direction,
                  type = 3,
                  detailed = T)

mod.jol #overall is sig

JOL.wide3 = cast(JOL2, id ~ Direction, value = "JOL", mean, na.rm = T)

apply(JOL.wide3, 2, mean)

#SDS
apply(JOL.wide3, 2, sd)

#95% Ci
(apply(JOL.wide3, 2, sd) / sqrt(nrow(JOL.wide3))) * 1.96

#F vs M
temp = t.test(JOL.wide3$F, JOL.wide3$M, paired = T, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #sig!
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

#F vs U
temp = t.test(JOL.wide3$F, JOL.wide3$U, paired = T, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #sig!
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

#M vs U
temp = t.test(JOL.wide3$M, JOL.wide3$U, paired = T, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

sd(JOL.wide3$M); sd(JOL.wide3$U)

#make recall a percent
combined$Scored = combined$Scored * 100

####Run the ANOVAs####
model1 = ezANOVA(combined,
                 wid = id,
                 dv = Scored,
                 between = Encoding_Group,
                 within = Direction,
                 type = 3,
                 detailed = T)
model1 #already getting significant everything!

model1$ANOVA$MSE = model1$ANOVA$SSd/model1$ANOVA$DFd
model1$ANOVA$MSE

aovEffectSize(model1, effectSize = "pes")

####comparisons####
tapply(combined$Scored, combined$Encoding_Group, mean) #main effect encoding
tapply(combined$Scored, combined$Direction, mean) #main effect direction
tapply(combined$Scored, list(combined$Encoding_Group, combined$Direction), mean, na.rm = T) #Interaction

####Post-Hocs####
###break down direction main effect
combined.direction = cast(combined, id ~ Direction, mean)

#F vs M
temp = t.test(combined.direction$F, combined.direction$M, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #sig!
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

#F vs U
temp = t.test(combined.direction$F, combined.direction$U, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #sig!
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

#U vs M
temp = t.test(combined.direction$M, combined.direction$U, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #sig!
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

##get sds for d
mean(combined.direction$F); sd(combined.direction$F)
mean(combined.direction$M); sd(combined.direction$M)
mean(combined.direction$U); sd(combined.direction$U)

#Not surprisingly, everything is sig!

###Interaction
jol3 = subset(combined, combined$Encoding_Group == "JOL")
read3 = subset(combined, combined$Encoding_Group == "Read")

jol.ph = cast(jol3, id ~ Direction, mean)
read.ph = cast(read3, id ~ Direction, mean)

(apply(jol.ph[ , -1], 2, sd) / sqrt(nrow(jol.ph))) * 1.96
(apply(read.ph[ , -1], 2, sd) / sqrt(nrow(read.ph))) * 1.96

nrow(jol.ph)
nrow(read.ph)

mean(jol.ph$F); mean(jol.ph$M)
mean(read.ph$F); mean(read.ph$M)

##forward
temp = t.test(jol.ph$F, read.ph$F, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #sig!
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

sd(jol.ph$F); sd(read.ph$F)

##mediated
temp = t.test(jol.ph$M, read.ph$M, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #sig!
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

sd(jol.ph$M); sd(read.ph$M)

#unrelated
temp = t.test(jol.ph$U, read.ph$U, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic 
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

pbic1 = jol.ph[ , c(1,4)]
pbic2 = read.ph[ , c(1,4)]

pbic1$group = rep("jol")
pbic2$group = rep("read")

pbic3 = rbind(pbic1, pbic2)

ezANOVA(pbic3,
        dv = U,
        between = group,
        wid = id,
        type = 3,
        detailed = T)

#post cleaning group sizes
length(unique(jol.ph$id)) #57
length(unique(read.ph$id)) #56

##Get CIs for table
(apply(jol.ph, 2, sd) / sqrt(nrow(jol.ph))) * 1.96
(apply(read.ph, 2, sd) / sqrt(nrow(read.ph))) * 1.96
