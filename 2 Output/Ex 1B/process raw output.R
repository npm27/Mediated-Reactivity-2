####This script will be used to read in everything and set EX 2data up for processing####
##Start by gathering all of the data
#JOLs and frequency
setwd("C:/Users/nickm/OneDrive/Documents/GitHub/Mediated-Reactivity/3 Output/Other/Backward M/JOL")

files = list.files(pattern = "*.csv")

#Put them in one dataframe. First apply read.csv, then rbind
dat = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

#get the number of participants
length(unique(dat$Username))

#read
setwd("C:/Users/nickm/OneDrive/Documents/GitHub/Mediated-Reactivity/3 Output/Other/Backward M/Read")

files2 = list.files(pattern = "*.csv")

#Put them in one dataframe. First apply read.csv, then rbind
dat2 = do.call(rbind, lapply(files2, function(x) read.csv(x, stringsAsFactors = FALSE)))

#get the number of participants
length(unique(dat2$Username))

#Now move back to the original folder
#This is where I'll store the combined final output for scoring
setwd('..')

####Clean up the data files####
##Drop unused columns
dat = dat[ , -c(2:4, 6:7, 9:10, 12, 20:23, 27:32, 34)]
dat2 = dat2[ , -c(2:4, 6:7, 9:10, 12, 20:23, 27:33)]

#Next, remove buffer trials
dat = subset(dat,
             dat$Stimuli.Stimuli.Notes != "Buffer")
dat2 = subset(dat2,
              dat2$Stimuli.Stimuli.Notes != "Buffer")

#Now remove instruction trials
dat = subset(dat,
             dat$Procedure.Trial.Type != "Instruct")
dat2 = subset(dat2,
              dat2$Procedure.Trial.Type != "Instruct")
#Now remove filler task
dat = subset(dat,
             dat$Procedure.Trial.Type != "FreeRecall")
dat2 = subset(dat2,
              dat2$Procedure.Trial.Type != "FreeRecall")

####Set the data up for scoring####
#Start by subsetting out the recall and JOL data for each dataset
dat.JOL = subset(dat,
                 dat$Procedure.Trial.Type == "JOL")
dat.Recall = subset(dat,
                    dat$Procedure.Trial.Type == "Test")

#get JOLs and Recall in the same order
dat.JOL = dat.JOL[order(dat.JOL$Stimuli.Cue), ]
dat.JOL = dat.JOL[order(dat.JOL$Condition.Number), ]
dat.JOL = dat.JOL[order(dat.JOL$Stimuli.Shuffle), ]

dat.Recall = dat.Recall[order(dat.Recall$Stimuli.Cue), ]
dat.Recall = dat.Recall[order(dat.Recall$Condition.Number), ]
dat.Recall = dat.Recall[order(dat.Recall$Stimuli.Shuffle), ]

#Okay, put it back together now
dat.R = dat.Recall[ , c(12:14)]

#Drop overlapping columns and clean things up
JOL = cbind(dat.JOL, dat.R)
JOL = JOL[ , -c(11, 13:14)]

JOL = JOL[ , -c(9:10)]
JOL = JOL[ , -2]

##Now do the same for the study only condition
#Start by subsetting out the recall and study trials for each dataset
dat2.Study = subset(dat2,
                    dat2$Procedure.Trial.Type == "Study")
dat2.Recall = subset(dat2,
                     dat2$Procedure.Trial.Type == "Test")

#get Study and Recall in the same order
dat2.Study = dat2.Study[order(dat2.Study$Stimuli.Cue), ]
dat2.Study = dat2.Study[order(dat2.Study$Condition.Number), ]
dat2.Study = dat2.Study[order(dat2.Study$Stimuli.Shuffle), ]

dat2.Recall = dat2.Recall[order(dat2.Recall$Stimuli.Cue), ]
dat2.Recall = dat2.Recall[order(dat2.Recall$Condition.Number), ]
dat2.Recall = dat2.Recall[order(dat2.Recall$Stimuli.Shuffle), ]

#Okay, put it back together now
dat2.R = dat2.Recall[ , c(12:14)]

#Drop overlapping columns and clean things up
Study = cbind(dat2.Study, dat2.R)
Study = Study[ , -c(11, 13:14)]

Study = Study[ , -c(9:10)]
Study = Study[ , -2]

####Score the recall data####
##Going to write everything to .csv and then use the old shiny app to score
#first lowercase everything
JOL$Stimuli.Cue = tolower(JOL$Stimuli.Cue)
JOL$Stimuli.Answer = tolower(JOL$Stimuli.Answer)

Study$Stimuli.Cue = tolower(Study$Stimuli.Cue)
Study$Stimuli.Answer = tolower(Study$Stimuli.Answer)

JOL$Response.Response = tolower(JOL$Response.Response)
Study$Response.Response = tolower(Study$Response.Response)

#now write to .csv for scoring
length(unique(JOL$Username)) #29
length(unique(Study$Username)) #30

##fix cases
JOL$Stimuli.Answer = tolower(JOL$Stimuli.Answer)
Study$Stimuli.Answer = tolower(Study$Stimuli.Answer)

#write.csv(JOL[ , c(1, 12, 5, 2:4, 6:11)], file = "JOL_pre_scored.csv", row.names = F)
#write.csv(Study[ , c(1, 11, 5, 2:4, 6:10)], file = "Study_pre_scored.csv", row.names = F)
