####This script will be used to read in everything and set EX 2data up for processing####
##Start by gathering all of the data
#JAMs
setwd("C:/Users/nickm/OneDrive/Documents/GitHub/Mediated-Reactivity-2/2 Output/DM-Pilot/1A/Raw")

files = list.files(pattern = "*.csv")

#Put them in one dataframe. First apply read.csv, then rbind
dat = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

#get the number of participants
length(unique(dat$Username))

#move back to parent folder
setwd('..')

####Clean up the data files####
##Drop unused columns
dat = dat[ , -c(2:4, 6:7, 9:10, 12, 20:23, 27:32, 34)]

#Next, remove Practice trials
dat = subset(dat,
             dat$Stimuli.Stimuli.Notes != "Practice")

#Now remove instruction trials
dat = subset(dat,
             dat$Procedure.Trial.Type != "Instruct")

#drop remaining columns
dat = dat[ , -c(2:4, 7, 9:14)]

#write.csv(dat, file = "JAMs_cleaned.csv", row.names = F)
