####Experiment 2-4####
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

dat = pd.read_csv("Ex 2.csv")

#make the 95% confidence intervals
dat['diff'] = dat['Upper'].sub(dat['Lower']) #get the length of the bars
dat['diff2'] = dat['diff'].div(2) #length from line to point

##set up the initial plot
fig = plt.figure()
fig.set_size_inches(18,26)

ax1 = fig.add_subplot(2, 1, 1)
ax2 = fig.add_subplot(2, 1, 2)

#fig.tight_layout()
fig.subplots_adjust(top = .925)
                    
plt.subplots_adjust(hspace = 0.20)

#fig.suptitle('Experiments 2-4: Recogniton Testing', fontsize = 24, fontweight = 'bold')

####Subset by Experiment####
exA = dat[dat['Experiment'] == "2A"]
exB = dat[dat['Experiment'] == "2B"]

####Experiment A####
#subset by task
j1 = exA[exA['Task'] == 'JOL']
s1 = exA[exA['Task'] == 'Study']

#get all the things to plug into the plots
#separate out averages and conf interval
j1_average = j1['Average']
s1_average = s1['Average']

j1_conf = j1['diff2']
s1_conf = s1['diff2']

ind = np.arange(len(j1_average))  # the x locations for the groups
width = 0.35 #bar width 

rects1 = ax1.bar(ind - width/2, j1_average, width, yerr = j1_conf, capsize = 3, color = 'white', edgecolor = 'k',
                label ='JOL')

rects2 = ax1.bar(ind + width/2, s1_average, width, yerr = s1_conf, capsize = 3, color = 'gray', edgecolor = 'k',
                label = 'No-JOL')

#Make the plot spiffy
ax1.set_title('Experiment 2A', fontsize = 40, fontweight = 'bold')
ax1.set_ylabel('Mean % Cued-Recall', fontsize = 30, fontweight = 'bold')
ax1.set_xlabel('Pair Type', fontsize = 36, fontweight = 'bold')
#ax1.xaxis.labelpad = 0
ax1.set_xticks(ind)
ax1.set_xticklabels(('Forward', 'Mediated', 'Unrelated'), fontsize = 32)
ax1.tick_params(axis="y", labelsize = 32)
ax1.legend(fontsize = 32)
ax1.set_ylim([0,100])

####Experiment B#####
#subset by task
j2 = exB[exB['Task'] == 'JOL']
s2 = exB[exB['Task'] == 'Study']

#get all the things to plug into the plots
#separate out averages and conf interval
j2_average = j2['Average']
s2_average = s2['Average']

j2_conf = j2['diff2']
s2_conf = s2['diff2']

ind = np.arange(len(j2_average))  # the x locations for the groups
width = 0.35 #bar width 

rects3 = ax2.bar(ind - width/2, j2_average, width, yerr = j2_conf, capsize = 3, color = 'white', edgecolor = 'k',
                label ='JOL')

rects4 = ax2.bar(ind + width/2, s2_average, width, yerr = s2_conf, capsize = 3, color = 'gray', edgecolor = 'k',
                label = 'No-JOL')

#Make the plot spiffy
ax2.set_title('Experiment 2B', fontsize = 40, fontweight = 'bold')
ax2.set_ylabel('Mean % Cued-Recall', fontsize = 30, fontweight = 'bold')
ax2.set_xlabel('Pair Type', fontsize = 36, fontweight = 'bold')
#ax1.xaxis.labelpad = 0
ax2.set_xticks(ind)
ax2.set_xticklabels(('Forward', 'Mediated', 'Unrelated'), fontsize = 32)
ax2.tick_params(axis="y", labelsize = 32)
ax2.legend(fontsize = 32)
ax2.set_ylim([0,100])


fig.savefig('EX2_chart.png', dip = 10000)