#Make up some time series charts from the available data taken from SNS 
#This is the generic code. Replace LOCALAUTHORITY with the Council of choice!
#the code will create a folder and add graphs to it

#clean up
rm(list=ls())
##Create the graphs folder if it does not exist (make sure to replace LOCALAUTHORITY!!)
LAfolder<-paste("S:/G - Governance & Performance Mngmt/Research Team/Fire Research/Assessments-Rproject/Additional Reports", "LOCALAUTHORITY", sep="/")
dir.create(LAfolder)
LAgraphsfolder<-paste(LAfolder, "Graphs", sep = "/")
dir.create(LAgraphsfolder)
#load required packages
library(ggplot2)
library(reshape2)
#read in the SNS data
dat<-readRDS("S:/G - Governance & Performance Mngmt/Research Team/Fire Research/Assessments-Rproject/data/SNSdata.rds")
# subset and take the mean of the desired variable
incdepvars<-c("Council", "csincdeprived_2005","csincdeprived_2008","csincdeprived_200809","csincdeprived_200910","csincdeprived_2011")
inc.dep<-dat[incdepvars]
inc.dep$Council<-ifelse(inc.dep$Council=="LOCALAUTHORITY",
                        "LOCALAUTHORITY", "Rest of Scotland")
inc.dep<-aggregate(data=inc.dep, cbind(csincdeprived_2005, csincdeprived_2008,
                          csincdeprived_200809, csincdeprived_200910,
                          csincdeprived_2011)~Council,
                   mean)
#Tidy up this data frame for plotting
inc.dep<-melt(inc.dep)
inc.dep$variable<-gsub("csincdeprived.", "",inc.dep$variable)
inc.dep$variable<-gsub("200809", "2009",inc.dep$variable)
inc.dep$variable<-gsub("200910", "2010",inc.dep$variable)
#Now make up some time-series charts
ggplot(data=inc.dep, aes(x=as.numeric(variable), y=value))+
  geom_line(size = 1, aes(colour=Council, linetype=Council))+
  xlab("Year") +
  ylab("Average Income Deprivation") +
  scale_colour_manual(name = "Geography", breaks = c("LOCALAUTHORITY", "Rest of Scotland"), values = c("red","blue")) + 
  scale_linetype_manual(name = "Geography", breaks = c("LOCALAUTHORITY", "Rest of Scotland"), values = c(1, 6)) + 
  theme_bw()+
  ggtitle("Percentage Income Deprived") +
  theme(legend.key = element_blank(),
        legend.background = element_rect(colour = 'black'),
        legend.position = "bottom",
        plot.title = element_text(lineheight = .8, face = "bold", vjust = 1),
        axis.text.x = element_text(size = 11, angle = 45, vjust = 1.1,
                                   hjust = 1),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 10, face = 'bold'),
        plot.background = element_rect(colour = 'black'))
## Save
ggsave(filename = paste(LAgraphsfolder, "IncomeDep.png", sep = "/"),
       width = 8, height = 6, dpi = 300)

#subset and take the mean of accidents
accvars<-c("Council","HSr93_2002","HSr93_2003","HSr93_2004","HSr93_2005","HSr93_2006",
           "HSr93_2007", "HSr93_2008","HSr93_2009","HSr93_2010",
           "HSr93_2011","HSr93_2012")
accidents<-dat[accvars]
accidents$Council<-ifelse(accidents$Council=="LOCALAUTHORITY",
                          "LOCALAUTHORITY", "Rest of Scotland")
accidents<-aggregate(data=accidents, cbind(HSr93_2002,HSr93_2003,
                                     HSr93_2004,HSr93_2005,HSr93_2006,
                                     HSr93_2007,HSr93_2008,
                                     HSr93_2009,HSr93_2010,
                                     HSr93_2011,HSr93_2012)~Council,
                     mean)
#Tidy up for plotting
accidents<-melt(accidents)
accidents$variable<-gsub("HSr93_", "",accidents$variable)
#now plot the graph
ggplot(data=accidents, aes(x=variable, y=value))+
  geom_line(size = 1, aes(colour=Council, linetype=Council, group=Council))+
  xlab("Year") +
  ylab("Accident Admissions per 100,000") +
  scale_colour_manual(name = "Geography", breaks = c("LOCALAUTHORITY", "Rest of Scotland"), values = c("red","blue")) + 
  scale_linetype_manual(name = "Geography", breaks = c("LOCALAUTHORITY", "Rest of Scotland"), values = c(1, 6)) + 
  theme_bw()+
  ggtitle("Hospital Admissions for Accidents per 100,000") +
  theme(legend.key = element_blank(),
        legend.background = element_rect(colour = 'black'),
        legend.position = "bottom",
        plot.title = element_text(lineheight = .8, face = "bold", vjust = 1),
        axis.text.x = element_text(size = 11, angle = 45, vjust = 1.1,
                                   hjust = 1),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 10, face = 'bold'),
        plot.background = element_rect(colour = 'black'))

## Save
ggsave(filename = paste(LAgraphsfolder, "Accidents.png", sep = "/"),
       width = 8, height = 6, dpi = 300)

#subset and take the mean of emergency admissions/100000
emadvars<-c("Council","hsr9_2005","hsr9_2006","hsr9_2007","hsr9_2008","hsr9_2009",
           "hsr9_2010", "hsr9_2011","hsr9_2012")
admissions<-dat[emadvars]
admissions$Council<-ifelse(admissions$Council=="LOCALAUTHORITY",
                          "LOCALAUTHORITY", "Rest of Scotland")
admissions<-aggregate(data=admissions, cbind(hsr9_2005,hsr9_2006,
                                           hsr9_2007,hsr9_2008,
                                           hsr9_2009,hsr9_2010,
                                           hsr9_2011,hsr9_2012)~Council,
                     mean)
#Tidy up for plotting
admissions<-melt(admissions)
admissions$variable<-gsub("hsr9_", "",admissions$variable)
#now plot the graph
ggplot(data=admissions, aes(x=variable, y=value))+
  geom_line(size = 1, aes(colour=Council, linetype=Council, group=Council))+
  xlab("Year") +
  ylab("Emergency Admissions per 100,000") +
  scale_colour_manual(name = "Geography", breaks = c("LOCALAUTHORITY", "Rest of Scotland"), values = c("red","blue")) + 
  scale_linetype_manual(name = "Geography", breaks = c("LOCALAUTHORITY", "Rest of Scotland"), values = c(1, 6)) + 
  theme_bw()+
  ggtitle("Emergency Hospital Admissions per 100,000") +
  theme(legend.key = element_blank(),
        legend.background = element_rect(colour = 'black'),
        legend.position = "bottom",
        plot.title = element_text(lineheight = .8, face = "bold", vjust = 1),
        axis.text.x = element_text(size = 11, angle = 45, vjust = 1.1,
                                   hjust = 1),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 10, face = 'bold'),
        plot.background = element_rect(colour = 'black'))

## Save
ggsave(filename = paste(LAgraphsfolder, "EmAdmissions.png", sep = "/"),
       width = 8, height = 6, dpi = 300)

#subset and take the mean of DLA/100000
dlavars<-c("Council", colnames(dat)[grepl("HS.DLA_rate", colnames(dat))])
dlallowance<-dat[dlavars]
dlallowance$Council<-ifelse(dlallowance$Council=="LOCALAUTHORITY",
                           "LOCALAUTHORITY", "Rest of Scotland")
dlallowance[,-1]<-sapply(dlallowance[,-1], as.numeric)
dlallowance<-aggregate(dlallowance[,-1], by=list(dlallowance$Council),
                      FUN="mean")
#Tidy up for plotting
dlallowance<-melt(dlallowance)
dlallowance$variable<-gsub("HS.DLA_rate ", "",dlallowance$variable)
colnames(dlallowance)[1]<-"Council"
labs<-c("2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012")
brks<-c("2002Q02","2003Q01","2004Q01","2005Q01","2006Q01","2007Q01","2008Q01","2009Q01","2010Q01","2011Q01","2012Q01")
#now plot the graph
ggplot(data=dlallowance, aes(x=variable, y=value))+
  geom_line(size = 1, aes(colour=Council, linetype=Council, group=Council))+
  xlab("Year") +
  ylab("DLA Claimants per 100,000") +
  scale_colour_manual(name = "Geography", breaks = c("LOCALAUTHORITY", "Rest of Scotland"), values = c("red","blue")) + 
  scale_linetype_manual(name = "Geography", breaks = c("LOCALAUTHORITY", "Rest of Scotland"), values = c(1, 6)) + 
  theme_bw()+
  ggtitle("Disability Living Allowance Rate per 100,000") +
  scale_x_discrete(breaks=brks, labels=labs)+
  theme(legend.key = element_blank(),
        legend.background = element_rect(colour = 'black'),
        legend.position = "bottom",
        plot.title = element_text(lineheight = .8, face = "bold", vjust = 1),
        axis.text.x = element_text(size = 11, angle = 45, vjust = 1.1,
                                   hjust = 1),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 10, face = 'bold'),
        plot.background = element_rect(colour = 'black'))

## Save
ggsave(filename = paste(LAgraphsfolder, "DisLivAll.png", sep = "/"),
       width = 8, height = 6, dpi = 300)

#subset and take the mean of emergency admissions over 65s/100000
emadvars<-c("Council","hsr12_2005","hsr12_2006","hsr12_2007","hsr12_2008","hsr12_2009",
            "hsr12_2010", "hsr12_2011","hsr12_2012")
admissions<-dat[emadvars]
admissions$Council<-ifelse(admissions$Council=="LOCALAUTHORITY",
                           "LOCALAUTHORITY", "Rest of Scotland")
admissions<-aggregate(data=admissions, cbind(hsr12_2005,hsr12_2006,
                                             hsr12_2007,hsr12_2008,
                                             hsr12_2009,hsr12_2010,
                                             hsr12_2011,hsr12_2012)~Council,
                      mean)
#Tidy up for plotting
admissions<-melt(admissions)
admissions$variable<-gsub("hsr12_", "",admissions$variable)
#now plot the graph
ggplot(data=admissions, aes(x=variable, y=value))+
  geom_line(size = 1, aes(colour=Council, linetype=Council, group=Council))+
  xlab("Year") +
  ylab("Emergency Admissions Rate") +
  scale_colour_manual(name = "Geography", breaks = c("LOCALAUTHORITY", "Rest of Scotland"), values = c("red","blue")) + 
  scale_linetype_manual(name = "Geography", breaks = c("LOCALAUTHORITY", "Rest of Scotland"), values = c(1, 6)) + 
  theme_bw()+
  ggtitle("Emergency Hospital Admissions for Over 65s per 100,000") +
  theme(legend.key = element_blank(),
        legend.background = element_rect(colour = 'black'),
        legend.position = "bottom",
        plot.title = element_text(lineheight = .8, face = "bold", vjust = 1),
        axis.text.x = element_text(size = 11, angle = 45, vjust = 1.1,
                                   hjust = 1),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 10, face = 'bold'),
        plot.background = element_rect(colour = 'black'))

## Save
ggsave(filename = paste(LAgraphsfolder, "EmAdmissionsOv65.png", sep = "/"),
       width = 8, height = 6, dpi = 300)

# subset and take the mean of employment deprivation
empdepvars<-c("Council", "csempdeprived_2005","csempdeprived_2008","csempdeprived_2009","csempdeprived_2010","csempdeprived_2011")
emp.dep<-dat[empdepvars]
emp.dep$Council<-ifelse(emp.dep$Council=="LOCALAUTHORITY",
                        "LOCALAUTHORITY", "Rest of Scotland")
emp.dep<-aggregate(data=emp.dep, cbind(csempdeprived_2005, csempdeprived_2008,
                                       csempdeprived_2009, csempdeprived_2010,
                                       csempdeprived_2011)~Council,
                   mean)
#Tidy up this data frame for plotting
emp.dep<-melt(emp.dep)
emp.dep$variable<-gsub("csempdeprived_", "",emp.dep$variable)
#Now make up some time-series charts
ggplot(data=emp.dep, aes(x=as.numeric(variable), y=value))+
  geom_line(size = 1, aes(colour=Council, linetype=Council))+
  xlab("Year") +
  ylab("Average Employment Deprivation") +
  scale_colour_manual(name = "Geography", breaks = c("LOCALAUTHORITY", "Rest of Scotland"), values = c("red","blue")) + 
  scale_linetype_manual(name = "Geography", breaks = c("LOCALAUTHORITY", "Rest of Scotland"), values = c(1, 6)) + 
  theme_bw()+
  ggtitle("Percentage Employment Deprived") +
  theme(legend.key = element_blank(),
        legend.background = element_rect(colour = 'black'),
        legend.position = "bottom",
        plot.title = element_text(lineheight = .8, face = "bold", vjust = 1),
        axis.text.x = element_text(size = 11, angle = 45, vjust = 1.1,
                                   hjust = 1),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 10, face = 'bold'),
        plot.background = element_rect(colour = 'black'))
## Save
ggsave(filename = paste(LAgraphsfolder, "EmploymentDep.png", sep = "/"),
       width = 8, height = 6, dpi = 300)


#subset and take the mean of SIMD Crime rate/10,000
Crtvars<-c("Council", colnames(dat)[grepl("CR.SIMDCRIME_totrat", colnames(dat))])
CrmRate<-dat[Crtvars]
CrmRate$Council<-ifelse(CrmRate$Council=="LOCALAUTHORITY",
                            "LOCALAUTHORITY", "Rest of Scotland")
CrmRate[,-1]<-sapply(CrmRate[,-1], as.numeric)
CrmRate<-aggregate(CrmRate[,-1], by=list(CrmRate$Council),
                       FUN="mean", na.rm=TRUE)
#Tidy up for plotting
CrmRate<-melt(CrmRate)
CrmRate$variable<-gsub("CR.SIMDCRIME_totrat ", "",CrmRate$variable)
colnames(CrmRate)[1]<-"Council"
#now plot the graph
ggplot(data=CrmRate, aes(x=variable, y=value))+
  geom_line(size = 1, aes(colour=Council, linetype=Council, group=Council))+
  xlab("Year") +
  ylab("SIMD Crimes per 10,000") +
  scale_colour_manual(name = "Geography", breaks = c("LOCALAUTHORITY", "Rest of Scotland"), values = c("red","blue")) + 
  scale_linetype_manual(name = "Geography", breaks = c("LOCALAUTHORITY", "Rest of Scotland"), values = c(1, 6)) + 
  theme_bw()+
  ggtitle("SIMD Crime Rate per 10,000") +
  theme(legend.key = element_blank(),
        legend.background = element_rect(colour = 'black'),
        legend.position = "bottom",
        plot.title = element_text(lineheight = .8, face = "bold", vjust = 1),
        axis.text.x = element_text(size = 11, angle = 45, vjust = 1.1,
                                   hjust = 1),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 10, face = 'bold'),
        plot.background = element_rect(colour = 'black'))

## Save
ggsave(filename = paste(LAgraphsfolder, "SIMDCrimeRate.png", sep = "/"),
       width = 8, height = 6, dpi = 300)

#subset and take the mean of SIMD Crime numbers
Crnovars<-c("Council", colnames(dat)[grepl("CR.SIMDCRIME_totno", colnames(dat))])
CrmNum<-dat[Crnovars]
CrmNum$Council<-ifelse(CrmNum$Council=="LOCALAUTHORITY",
                        "LOCALAUTHORITY", "Rest of Scotland")
CrmNum[,-1]<-sapply(CrmNum[,-1], as.numeric)
CrmNum<-aggregate(CrmNum[,-1], by=list(CrmNum$Council),
                   FUN="mean", na.rm=TRUE)
#Tidy up for plotting
CrmNum<-melt(CrmNum)
CrmNum$variable<-gsub("CR.SIMDCRIME_totno ", "",CrmNum$variable)
colnames(CrmNum)[1]<-"Council"
#now plot the graph
ggplot(data=CrmNum, aes(x=variable, y=value))+
  geom_line(size = 1, aes(colour=Council, linetype=Council, group=Council))+
  xlab("Year") +
  ylab("SIMD Crimes") +
  scale_colour_manual(name = "Geography", breaks = c("LOCALAUTHORITY", "Rest of Scotland"), values = c("red","blue")) + 
  scale_linetype_manual(name = "Geography", breaks = c("LOCALAUTHORITY", "Rest of Scotland"), values = c(1, 6)) + 
  theme_bw()+
  ggtitle("SIMD Crimes Average Total Number") +
  theme(legend.key = element_blank(),
        legend.background = element_rect(colour = 'black'),
        legend.position = "bottom",
        plot.title = element_text(lineheight = .8, face = "bold", vjust = 1),
        axis.text.x = element_text(size = 11, angle = 45, vjust = 1.1,
                                   hjust = 1),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 10, face = 'bold'),
        plot.background = element_rect(colour = 'black'))
## Save
ggsave(filename = paste(LAgraphsfolder, "SIMDCrimeNumbers.png", sep = "/"),
       width = 8, height = 6, dpi = 300)

#subset and take the mean of Average Fire Rate
AFrvars<-c("Council", colnames(dat)[grepl("FR.all_fires_count", colnames(dat))])
AFrRate<-dat[AFrvars]
AFrRate$Council<-ifelse(AFrRate$Council=="LOCALAUTHORITY",
                        "LOCALAUTHORITY", "Rest of Scotland")
AFrRate[,-1]<-sapply(AFrRate[,-1], as.numeric)
AFrRate<-aggregate(AFrRate[,-1], by=list(AFrRate$Council),
                   FUN="mean", na.rm=TRUE)
#Tidy up for plotting
AFrRate<-melt(AFrRate)
AFrRate$variable<-gsub("FR.all_fires_count ", "",AFrRate$variable)
colnames(AFrRate)[1]<-"Council"
#now plot the graph
ggplot(data=AFrRate, aes(x=variable, y=value))+
  geom_line(size = 1, aes(colour=Council, linetype=Council, group=Council))+
  xlab("Year") +
  ylab("Average Number of Fires") +
  scale_colour_manual(name = "Geography", breaks = c("LOCALAUTHORITY", "Rest of Scotland"), values = c("red","blue")) + 
  scale_linetype_manual(name = "Geography", breaks = c("LOCALAUTHORITY", "Rest of Scotland"), values = c(1, 6)) + 
  theme_bw()+
  ggtitle("Average Number of Fires per Data Zone") +
  theme(legend.key = element_blank(),
        legend.background = element_rect(colour = 'black'),
        legend.position = "bottom",
        plot.title = element_text(lineheight = .8, face = "bold", vjust = 1),
        axis.text.x = element_text(size = 11, angle = 45, vjust = 1.1,
                                   hjust = 1),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 10, face = 'bold'),
        plot.background = element_rect(colour = 'black'))

## Save
ggsave(filename = paste(LAgraphsfolder, "AllFires.png", sep = "/"),
       width = 8, height = 6, dpi = 300)


#subset and take the mean of accidental fires 
accvars<-c("Council", colnames(dat)[grepl("FR.AccDwellFireRate", colnames(dat))])
AccRate<-dat[accvars]
AccRate$Council<-ifelse(AccRate$Council=="LOCALAUTHORITY",
                        "LOCALAUTHORITY", "Rest of Scotland")
AccRate[,-1]<-sapply(AccRate[,-1], as.numeric)
AccRate<-aggregate(AccRate[,-1], by=list(AccRate$Council),
                   FUN="mean", na.rm=TRUE)
#Tidy up for plotting
AccRate<-melt(AccRate)
AccRate$variable<-gsub("FR.AccDwellFireRate ", "",AccRate$variable)
colnames(AccRate)[1]<-"Council"
#now plot the graph
ggplot(data=AccRate, aes(x=variable, y=value))+
  geom_line(size = 1, aes(colour=Council, linetype=Council, group=Council))+
  xlab("Year") +
  ylab("Accidental Fire Rate") +
  scale_colour_manual(name = "Geography", breaks = c("LOCALAUTHORITY", "Rest of Scotland"), values = c("red","blue")) + 
  scale_linetype_manual(name = "Geography", breaks = c("LOCALAUTHORITY", "Rest of Scotland"), values = c(1, 6)) + 
  theme_bw()+
  ggtitle("Accidental Fire Rate per 100,000") +
  theme(legend.key = element_blank(),
        legend.background = element_rect(colour = 'black'),
        legend.position = "bottom",
        plot.title = element_text(lineheight = .8, face = "bold", vjust = 1),
        axis.text.x = element_text(size = 11, angle = 45, vjust = 1.1,
                                   hjust = 1),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 10, face = 'bold'),
        plot.background = element_rect(colour = 'black'))
## Save
ggsave(filename = paste(LAgraphsfolder, "AccFires.png", sep = "/"),
       width = 8, height = 6, dpi = 300)

#subset and take the mean of deliberate fires 
delvars<-c("Council", colnames(dat)[grepl("FR.del_fire_rate", colnames(dat))])
DelRate<-dat[delvars]
DelRate$Council<-ifelse(DelRate$Council=="LOCALAUTHORITY",
                        "LOCALAUTHORITY", "Rest of Scotland")
DelRate[,-1]<-sapply(DelRate[,-1], as.numeric)
DelRate<-aggregate(DelRate[,-1], by=list(DelRate$Council),
                   FUN="mean", na.rm=TRUE)
#Tidy up for plotting
DelRate<-melt(DelRate)
DelRate$variable<-gsub("FR.del_fire_rate ", "",DelRate$variable)
colnames(DelRate)[1]<-"Council"
#now plot the graph
ggplot(data=DelRate, aes(x=variable, y=value))+
  geom_line(size = 1, aes(colour=Council, linetype=Council, group=Council))+
  xlab("Year") +
  ylab("Deliberate Fire Rate") +
  scale_colour_manual(name = "Geography", breaks = c("LOCALAUTHORITY", "Rest of Scotland"), values = c("red","blue")) + 
  scale_linetype_manual(name = "Geography", breaks = c("LOCALAUTHORITY", "Rest of Scotland"), values = c(1, 6)) + 
  theme_bw()+
  ggtitle("Deliberate Fire Rate per 100,000") +
  theme(legend.key = element_blank(),
        legend.background = element_rect(colour = 'black'),
        legend.position = "bottom",
        plot.title = element_text(lineheight = .8, face = "bold", vjust = 1),
        axis.text.x = element_text(size = 11, angle = 45, vjust = 1.1,
                                   hjust = 1),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 10, face = 'bold'),
        plot.background = element_rect(colour = 'black'))
## Save
ggsave(filename = paste(LAgraphsfolder, "DeliberateFire.png", sep = "/"),
       width = 8, height = 6, dpi = 300)

#Heart Disease Rate==============================================================

dta<-readRDS("S:/G - Governance & Performance Mngmt/Research Team/Fire Research/Assessments-Rproject/Panel dataset/paneldata.rds")
hdvars<-c("council", colnames(dta)[grep("hs.r33", colnames(dta))])
hddta<-dta[hdvars]
hddta$council<-ifelse(hddta$council == "LOCALAUTHORITY", 
                      "LOCALAUTHORITY", "Rest of Scotland")
hddta<-aggregate(hddta[,-1], by = list(hddta$council), mean, na.rm = TRUE)
#tidy up for plotting
hddta<-melt(hddta)
hddta$variable<-gsub("hs.r33 ", "", hddta$variable)
colnames(hddta)[1:2]<-c("Council", "Year")
##Plot
ggplot(data = hddta, aes(x = Year, y = value))+
  geom_line(size = 1, aes(colour=Council, linetype=Council, group=Council))+
  xlab("Year") + 
  ylab("Coronary Heart Disease Rate") +
  scale_colour_manual(name = "Geography", breaks = c("LOCALAUTHORITY", "Rest of Scotland"), values = c("red","blue")) + 
  scale_linetype_manual(name = "Geography", breaks = c("LOCALAUTHORITY", "Rest of Scotland"), values = c(1, 6)) + 
  theme_bw()+
  ggtitle("Coronary Heart Disease Rate per 100,000") +
  theme(legend.key = element_blank(),
        legend.background = element_rect(colour = 'black'),
        legend.position = "bottom",
        plot.title = element_text(lineheight = .8, face = "bold", vjust = 1),
        axis.text.x = element_text(size = 11, angle = 90, vjust = 1.1,
                                   hjust = 1),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 10, face = 'bold'),
        plot.background = element_rect(colour = 'black'))
#Save
ggsave(filename = paste(LAgraphsfolder, "HeartDis.png", sep = "/"),
       width = 8, height = 6, dpi = 300)