##read in data
JOL = read.csv("Backward M/JOL Scored.csv")
Read = read.csv("Backward M/Read Scored.csv")

#load libraries
library(reshape)
library(ez)
library(psychReport)

#turn off scientific notation
options(scipen = 999)

##drop unused columns
JOL2 = JOL[ , -c(2:4, 6, 8:9, 11:14)]
Read2 = Read[ , -c(2:4, 6, 8:9, 11:13)]

#slap together and rearrange columns
combined = rbind(JOL2, Read2)

combined = combined[ , c(1, 3, 4, 2)]

#rename columns
colnames(combined)[c(2:4)] = c("encoding", "direction", "score")

#get score on correct scale
combined$score = combined$score * 100

tapply(combined$score, list(combined$encoding, combined$direction), mean, na.rm = T)

##any difference in JOLs?
JOL3 = JOL[ , -c(2:4, 6, 8:9, 11, 13:14)]

#Remove out of range JOLs
JOL3$Response.JOL[JOL3$Response.JOL > 100] = NA

tapply(JOL3$Response.JOL, JOL3$Stimuli.Stimuli.Notes, mean, na.rm = T)



####Outliers####
summary(combined)

JOL4 = subset(combined,
              combined$encoding == "JOL")

Read3 = subset(combined,
               combined$encoding == "Read")

JOL.wide = cast(JOL4, id ~ direction, mean)
Read.wide = cast(Read3, id ~ direction, mean)

##drops
combined = subset(combined,
                  combined$id != "w10141665_ljd") #correctly recalled basically all pair types -- suggests cheating

combined = subset(combined,
                  combined$id != "w10126861SES") #recalled less than 5% across all categories -- suggests not paying attention

####Run the Anova####
model1 = ezANOVA(combined,
        dv = score,
        between = encoding,
        within = direction,
        wid = id,
        type = 3,
        detailed = T)

model1$ANOVA$MSE = model1$ANOVA$SSd/model1$ANOVA$DFd
model1$ANOVA$MSE

aovEffectSize(model1, effectSize = "pes")

#Everything is already significant

####Post hocs####
tapply(combined$score, combined$encoding, mean) #main effect encoding
tapply(combined$score, combined$direction, mean) #main effect direction
tapply(combined$score, list(combined$encoding, combined$direction), mean, na.rm = T) #Interaction

###break down direction main effect
combined.direction = cast(combined, id ~ direction, mean)

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

##get d
mean(combined.direction$M); sd(combined.direction$M)
mean(combined.direction$U); sd(combined.direction$U)

#Not surprisingly, everything is sig!

###Interaction
jol3 = subset(combined, combined$encoding == "JOL")
read3 = subset(combined, combined$encoding == "Read")

jol.ph = cast(jol3, id ~ direction, mean)
read.ph = cast(read3, id ~ direction, mean)

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

##mediated
temp = t.test(jol.ph$M, read.ph$M, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #sig = .02
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

#unrelated
temp = t.test(jol.ph$U, read.ph$U, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic #Non-Sig
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

#get pbic
pbic1 = jol.ph[ , c(1, 4)]
pbic2 = read.ph[ , c(1, 4)]

pbic1$encoding = rep("JOL")
pbic2$encoding = rep("read")

pbic3 = rbind(pbic1, pbic2)

ezANOVA(pbic3,
        dv = U,
        wid = id,
        between = encoding,
        detailed = T,
        type = 3)

length(unique(jol.ph$id))
length(unique(read.ph$id))
