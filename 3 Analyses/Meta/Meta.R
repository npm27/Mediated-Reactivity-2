####Set up####
##read in data
dat = read.csv("meta_data.csv")

#load custom functions
compute_se_d_uneq = function(d, n1, n2) {
  sqrt( (n1 + n2) / (n1 * n2) + (d^2) / (2 * (n1 + n2)) )
}

#load libraries
library(metafor)
library(dplyr)

#turn off scientific notation
options(scipen = 999)

####Calculate SE####
##1A
ex1a_F = compute_se_d_uneq(d = dat$d[1], n1 = dat$n_1[1], n2 = dat$n_2[1])
ex1a_M = compute_se_d_uneq(d = dat$d[2], n1 = dat$n_1[2], n2 = dat$n_2[2])
ex1a_U = compute_se_d_uneq(d = dat$d[3], n1 = dat$n_1[3], n2 = dat$n_2[3])

#1B
ex1b_F = compute_se_d_uneq(d = dat$d[4], n1 = dat$n_1[4], n2 = dat$n_2[4])
ex1b_M = compute_se_d_uneq(d = dat$d[5], n1 = dat$n_1[5], n2 = dat$n_2[5])
ex1b_U = compute_se_d_uneq(d = dat$d[6], n1 = dat$n_1[6], n2 = dat$n_2[6])

#2A
ex2a_F = compute_se_d_uneq(d = dat$d[7], n1 = dat$n_1[7], n2 = dat$n_2[7])
ex2a_M = compute_se_d_uneq(d = dat$d[8], n1 = dat$n_1[8], n2 = dat$n_2[8])
ex2a_U = compute_se_d_uneq(d = dat$d[9], n1 = dat$n_1[9], n2 = dat$n_2[9])

#2B
ex2b_F = compute_se_d_uneq(d = dat$d[10], n1 = dat$n_1[10], n2 = dat$n_2[10])
ex2b_M = compute_se_d_uneq(d = dat$d[11], n1 = dat$n_1[11], n2 = dat$n_2[11])
ex2b_U = compute_se_d_uneq(d = dat$d[12], n1 = dat$n_1[12], n2 = dat$n_2[12])

SE = data.frame(c(ex1a_F, ex1a_M, ex1a_U, ex1b_F, ex1b_M, ex1b_U,
       ex2a_F, ex2a_M, ex2a_U, ex2b_F, ex2b_M, ex2b_U))

dat = cbind(dat, SE)

colnames(dat)[6] = "SE"

####Run the meta####
#overall
res_all = rma(yi = d, sei = SE, data = dat, method = "REML")
summary(res_all)

#subgroups
unrelated = subset(dat,
                   dat$Pair_Type == "U")
Forward = subset(dat,
                 dat$Pair_Type == "F")
Mediated = subset(dat,
                  dat$Pair_Type != "F" & dat$Pair_Type != "U")

#Forward
res_F = rma(yi = d, sei = SE, data = Forward, method = "REML")
summary(res_F)

#Medaited
res_M = rma(yi = d, sei = SE, data = Mediated, method = "REML")
summary(res_M)

#Unrelated
res_U = rma(yi = d, sei = SE, data = unrelated, method = "REML")
summary(res_U)

####Make the Forest Plot####
labels = c(
  "Forward (Ex 1A)", "Forward (Ex 1B)", "Forward (Ex 2A)", "Forward (Ex 2B)",   # Forward Associates
  "Mediated (Ex 1A)", "Mediated (Ex 1B)", "Mediated (Ex 2A)", "Mediated (Ex 2B)",         # Mediated Pairs
  "Unrelated (Ex 1A)", "Unrelated (Ex 1B)", "Unrelated (Ex 2A)", "Unrelated (Ex 2B)"    # Unrelated
)

#reorder columns
sort = data.frame(c(1,5,9,2,6,10,3,7,11,4,8,12))
dat = cbind(dat, sort)

colnames(dat)[7] = "sort"

dat2 = dat[order(dat$sort), ]

##re run overall  model with sorted data
res_all2 = rma(yi = d, sei = SE, data = dat2, method = "REML")
summary(res_all2)

forest(res_all2, 
       slab = labels,      # This adds labels per study
       xlab = "Cohen's d",
       xlim = c(-2, 3),
       col = "black",
       mlab = "Overall effect",
       header = "Pair Type")

