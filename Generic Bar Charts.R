#'This is the generic code for producing some bar charts for the report
#'You will need to replace LOCALAUTHORITY with the Council being examined
#'In addition it will be necessary to change some of the some of the bar colours
#'Such as on line 25 area.colour

# Clean, run with caution
rm(list = ls())

# load required packages
require(ggplot2)             # To make the charts
require(reshape2)            # Data Manipulation
require(scales)              # To make Percentage Graphs

##Create the graphs folder if it does not exist (make sure to replace LOCALAUTHORITY!!)
LAfolder<-paste("S:/G - Governance & Performance Mngmt/Research Team/Fire Research/Assessments-Rproject/Additional Reports", "LOCALAUTHORITY", sep="/")
dir.create(LAfolder)
LAgraphsfolder<-paste(LAfolder, "Graphs", sep = "/")
dir.create(LAgraphsfolder)
#Data folder
LAdatafolder<-paste(LAfolder, "Data", sep = "/")

# Population Density Chart-------------------------------
df_dns <- read.csv("S:/G - Governance & Performance Mngmt/Research Team/Fire Research/Assessments-Rproject/data/Batch1_LAC0R0_6_7_2015.csv", stringsAsFactors = FALSE)[,c(1,63)]
df_dns <- df_dns[-1,]
area.colour <- c(rep("black", 17), 'red', rep("black", 14)) #CHANGE!
ggplot(data = df_dns, aes(x = reorder(GeographyCode, -density), y = density)) +
  geom_bar(stat = "identity", fill = area.colour) +
  theme_bw() +
  theme(legend.key = element_blank(),
        legend.background = element_rect(colour = 'black'),
        legend.position = "bottom",
        plot.title = element_text(lineheight = .8, face = "bold", vjust = 1),
        axis.text.x = element_text(size = 11, angle = 90, vjust = 0.6,
                                   hjust = 1, colour = area.colour),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 10, face = 'bold'),
        plot.background = element_rect(colour = 'black')) +
  xlab("Council ") +
  ylab("Persons per Hectare") +
  geom_hline(yintercept = 0.7, colour = "red") +
  guides(fill = FALSE)

# Export
ggsave(filename = paste(LAgraphsfolder, "pop_dns.png", sep = "/"),
       width = 8, height = 4, dpi = 300)

#Age Composition 65+---------------------------------------------------------
df65<-readRDS(file="S:/G - Governance & Performance Mngmt/Research Team/Fire Research/Assessments-Rproject/data/ages.rds")
df65<-df65[-1,]
df65_2<-melt(df65,id.vars="LA")
clrs_pop<-c(rep("black", 20), 'red', rep("black", 11)) # Change
ggplot(data=df65_2, aes(x=reorder(LA,-value), value, fill=variable))+
  geom_bar(stat="identity")+theme_bw()+
  theme(legend.title = element_blank(),
    legend.key = element_blank(),
        legend.background = element_rect(colour = 'black'),
        legend.position = "bottom",
        plot.title = element_text(lineheight = .8, face = "bold", vjust = 1),
        axis.text.x = element_text(size = 10, angle = 90, vjust = 0.6,
                                   hjust = 1, colour=clrs_pop),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 10, face = 'bold'),
        plot.background = element_rect(colour = 'black'))+
  xlab("Council")+ylab("Population Count")+ggtitle("Age Cohorts")
#save
ggsave(filename = paste(LAgraphsfolder, "AgeCohorts.png", sep = "/"),
       width = 8, height = 4, dpi = 300)

#Age Composition 65+ as Percentages----------------------------------------
##Same Dataset as above
clr<-c(rep("black", 14), 'red', rep("black", 17)) #Change
df65$OldPerc<-(df65$`Over 65`/(df65$`Under 18`+df65$`Working Age`+df65$`Over 65`))*100
#So here i need to muck about to try and order the data frmaes so that 
#high percent old people comes first, if you don't want that, skip 
#these stages and use the dataframe (df2) from above
#also will need to change clrs to 9 black, red, 22 black
df65_3<-df65[order(-df65$`OldPerc`), 1:4]
df65_2<-melt(df65_3,id.vars="LA")
df65_2$LA <- factor(df65_2$LA, levels = unique(df65_2$LA))
#Now plot graph
ggplot(data=df65_2, aes(LA, value, fill=variable))+
  geom_bar(position="fill", stat="identity")+theme_bw()+
  scale_y_continuous(labels=percent_format())+
  theme(legend.title = element_blank(),
    legend.key = element_blank(),
        legend.background = element_rect(colour = 'black'),
        legend.position = "bottom",
        plot.title = element_text(lineheight = .8, face = "bold", vjust = 1),
        axis.text.x = element_text(size = 10, angle = 90, vjust = 0.6,
                                   hjust = 1, colour=clr),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 10, face = 'bold'),
        plot.background = element_rect(colour = 'black'))+
  xlab("Council")+ylab("Population Percent")+ggtitle("Age Cohorts")
#save
ggsave(filename = paste(LAgraphsfolder, "OrdPerAgeCohorts.png", sep = "/"),
       width = 8, height = 4, dpi = 300)


#Age Composition 75+---------------------------------------------------------
df75<-readRDS(file="S:/G - Governance & Performance Mngmt/Research Team/Fire Research/Assessments-Rproject/data/ages75.rds")
df75<-df75[-1,c(17,18,20,21)]
colnames(df75)<-c("LA","Under 18", "18 to 75", "Over 75")
df75_2<-melt(df75,id.vars="LA")
ggplot(data=df75_2, aes(x=reorder(LA,-value), value, fill=variable))+
  geom_bar(stat="identity")+theme_bw()+
  theme(legend.title = element_blank(),
    legend.key = element_blank(),
        legend.background = element_rect(colour = 'black'),
        legend.position = "bottom",
        plot.title = element_text(lineheight = .8, face = "bold", vjust = 1),
        axis.text.x = element_text(size = 10, angle = 90, vjust = 0.6,
                                   hjust = 1, colour=clrs_pop),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 10, face = 'bold'),
        plot.background = element_rect(colour = 'black'))+
  xlab("Council")+ylab("Population Count")+ggtitle("Age Cohorts")
#save
ggsave(filename = paste(LAgraphsfolder, "AgeCohorts75.png", sep = "/"),
       width = 8, height = 4, dpi = 300)


#Age Composition 75+ as Percentages----------------------------------------
##Same Dataset as above
clrs75<-c(rep("black", 13), 'red', rep("black", 18))  #CHANGE
df75$OldPerc<-(df75$`Over 75`/(df75$`Under 18`+df75$`18 to 75`+df75$`Over 75`))*100
#So here i need to muck about to try and order the data frmaes so that 
#high percent old people comes first, if you don't want that, skip 
#these stages and use the dataframe (df75_2) from above
#also will need to change clrs to 9 black, red, 22 black
df75_3<-df75[order(-df75$`OldPerc`), 1:4]
df75_2<-melt(df75_3,id.vars="LA")
df75_2$LA <- factor(df75_2$LA, levels = unique(df75_2$LA))
#Now plot graph
ggplot(data=df75_2, aes(LA, value, fill=variable))+
  geom_bar(position="fill", stat="identity")+theme_bw()+
  scale_y_continuous(labels=percent_format())+
  theme(legend.title = element_blank(),
    legend.key = element_blank(),
        legend.background = element_rect(colour = 'black'),
        legend.position = "bottom",
        plot.title = element_text(lineheight = .8, face = "bold", vjust = 1),
        axis.text.x = element_text(size = 10, angle = 90, vjust = 0.55,
                                   hjust = 1, colour=clrs75),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 10, face = 'bold'),
        plot.background = element_rect(colour = 'black'))+
  xlab("Council")+ylab("Population Percent")+ggtitle("Age Cohorts")
#save
ggsave(filename = paste(LAgraphsfolder, "OrdPerAgeCohorts75.png", sep = "/"),
       width = 8, height = 4, dpi = 300)


# Over 65 Total Percentage------------------------------------------------------------
#read data from here- should already be open
#df65<-readRDS(file="S:/G - Governance & Performance Mngmt/Research Team/Fire Research/Assessments-Rproject/data/ages.rds")
df_tot65<-df65[1:4]
#Plot the chart
df_tot65$Ov65<-(df_tot65$`Over 65`/(df_tot65$`Under 18`+df_tot65$`Working Age`+df_tot65$`Over 65`))*100
ggplot(data=df_tot65, aes(x=reorder(LA, -Ov65), y=Ov65))+
  geom_bar(stat="identity",fill=clr)+theme_bw()+
  theme(legend.key = element_blank(),
        legend.background = element_rect(colour = 'black'),
        legend.position = "bottom",
        plot.title = element_text(lineheight = .8, face = "bold", vjust = 1),
        axis.text.x = element_text(size = 10, angle = 90, vjust = 0.6,
                                   hjust = 1, colour = clr),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 10, face = 'bold'),
        plot.background = element_rect(colour = 'black'))+
  xlab("Council")+ylab("Percentage Population Over 65")+
  geom_hline(yintercept= 16.1, colour="red")+guides(fill=FALSE)
#save
ggsave(filename = paste(LAgraphsfolder, "PercentageOver65.png", sep = "/"),
       width = 8, height = 4, dpi = 300)


#Percentage of Population over 75===============================
#read data from here- should be loaded already
#df75<-readRDS(file="S:/G - Governance & Performance Mngmt/Research Team/Fire Research/Assessments-Rproject/data/ages75.rds")
df_tot75<-df75[1:4]
colnames(df_tot75)<-c("LA","Under 18", "18 to 75", "Over 75")
#Plot the chart
area.colour<-c(rep("black", 13), 'red', rep("black", 18))
df_tot75$Ov75<-(df_tot75$`Over 75`/(df_tot75$`Under 18`+df_tot75$`18 to 75`+df_tot75$`Over 75`))*100
ggplot(data=df_tot75, aes(x=reorder(LA, -Ov75), y=Ov75))+
  geom_bar(stat="identity",fill=area.colour)+theme_bw()+
  theme(legend.key = element_blank(),
        legend.background = element_rect(colour = 'black'),
        legend.position = "bottom",
        plot.title = element_text(lineheight = .8, face = "bold", vjust = 1),
        axis.text.x = element_text(size = 10, angle = 90, vjust = 0.6,
                                   hjust = 1, colour = clrs75),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 10, face = 'bold'),
        plot.background = element_rect(colour = 'black'))+
  xlab("Council")+ylab("Percentage Population Over 75")+
  geom_hline(yintercept=7.13, colour="red")+guides(fill=FALSE)
#save
ggsave(filename = paste(LAgraphsfolder, "PercentageOver75.png", sep = "/"),
       width = 8, height = 4, dpi = 300)

#dependency rate --------------------------------
clrs_dr<-c(rep("black", 13), 'red', rep("black", 18))
df<-read.csv("S:/G - Governance & Performance Mngmt/Research Team/Fire Research/Assessments-Rproject/data/Batch1_LAC0R0_6_7_2015.csv")[,c(1,61)]
df<-df[-1,]
colnames(df)<-c("Council", "Dependency Rate")
ggplot(data=df, aes(x=reorder(Council, -`Dependency Rate`), y=`Dependency Rate`))+
  geom_bar(stat="identity", fill=clrs_dr)+theme_bw()+
  theme(legend.key = element_blank(),
        legend.background = element_rect(colour = 'black'),
        legend.position = "bottom",
        plot.title = element_text(lineheight = .8, face = "bold", vjust = 1),
        axis.text.x = element_text(size = 10, angle = 90, vjust = 0.6,
                                   hjust = 1, colour = clrs_dr),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 10, face = 'bold'),
        plot.background = element_rect(colour = 'black'))+
  xlab("Council")+ylab("Dependency Ratio")+ggtitle("Dependency Ratio")+
  geom_hline(yintercept=60.6, colour="red")+guides(fill=FALSE)

ggsave(filename = paste(LAgraphsfolder, "DependencyRate.png", sep = "/"),
       width = 8, height = 4, dpi = 300)

#Council Emergency Admissions Over 65s--------------------------------------

clrs_em<-c(rep("black", 20), 'red', rep("black", 11))
df<-read.csv("S:/G - Governance & Performance Mngmt/Research Team/Fire Research/Assessments-Rproject/data/EmergencyAds65.csv", stringsAsFactors = FALSE)
colnames(df)<-c("Council", "Admission Rate")
ggplot(data=df, aes(x=reorder(Council, -`Admission Rate`), y=`Admission Rate`))+
  geom_bar(stat="identity", fill=clrs_em)+theme_bw()+
  theme(legend.key = element_blank(),
        legend.background = element_rect(colour = 'black'),
        legend.position = "bottom",
        plot.title = element_text(lineheight = .8, face = "bold", vjust = 1),
        axis.text.x = element_text(size = 10, angle = 45, vjust = 1.1,
                                   hjust = 1.1, colour - clrs_em),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 10, face = 'bold'),
        plot.background = element_rect(colour = 'black'))+
  xlab("Council")+ylab("Admission Rate")+ggtitle("Emergency Admission Rate 65+, 2013/14")+
  geom_hline(yintercept=24556, colour="red")+guides(fill=FALSE)
ggsave(filename = paste(LAgraphsfolder, "EmergencyAdRate65.png", sep = "/"),
       width = 8, height = 4, dpi = 300)

#Hospital Episodes Diagnosis
diag_dt<-read.csv(paste(LAdatafolder, "Diagnosis.csv", sep = "/"))
diag_all<-diag_dt[-1,c(1,9, 2)]
colnames(diag_all)<-c("Diagnosis", "Scotland Rate", "LOCALAUTHORITY CHP Rate") #Change LA
diag_all<-melt(diag_all)
ggplot(data=diag_all, aes(x=Diagnosis, y=value, fill= variable))+
  geom_bar(stat="identity", position=position_dodge(), colour="black")+
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.key = element_blank(),
        legend.background = element_rect(colour = 'black'),
        legend.position = "bottom",
        plot.title = element_text(lineheight = .8, face = "bold", vjust = 1),
        axis.text.x = element_text(size = 9, angle = 45, vjust = 1.05,
                                   hjust = 1, face = "bold"),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 10, face = 'bold'),
        plot.background = element_rect(colour = 'black'))+
  ylab("Total Episode Rate per 100,000")+ggtitle("Hospital Episodes by Diagnosis 2013-14")
ggsave(filename = paste(LAgraphsfolder, "EpiDiagnosis.png", sep = "/"),
       width = 8, height = 5, dpi = 300) 

#Hospital Diagnoses 85+--------------------------------------------------

diag_85<-diag_dt[-1,c(1,10,3)]
colnames(diag_85)<-c("Diagnosis", "Scotland Rate", "LOCALAUTHORITY CHP Rate") #CHANGE
diag_85<-melt(diag_85)
ggplot(data=diag_85, aes(x=Diagnosis, y=value, fill= variable))+
  geom_bar(stat="identity", position=position_dodge(), colour="black")+
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.key = element_blank(),
        legend.background = element_rect(colour = 'black'),
        legend.position = "bottom",
        plot.title = element_text(lineheight = .8, face = "bold", vjust = 1),
        axis.text.x = element_text(size = 9, angle = 45, vjust = 1.05,
                                   hjust = 1, face = "bold"),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 10, face = 'bold'),
        plot.background = element_rect(colour = 'black'))+
  ylab("Total Episode Rate per 100,000")+ggtitle("Hospital Episodes by Diagnosis for 85+, 2013-14")
ggsave(filename = paste(LAgraphsfolder, "EpiDiagnosis85.png", sep = "/"),
       width = 8, height = 5, dpi = 300) 

##Hospital diagnosis 75+--------------------------------------
diag_75<-diag_dt[-1,c(1,13,6)]
colnames(diag_75)<-c("Diagnosis", "Scotland Rate", "LOCALAUTHORITY CHP Rate") #CHANGE
diag_75<-melt(diag_75)
ggplot(data=diag_75, aes(x=Diagnosis, y=value, fill= variable))+
  geom_bar(stat="identity", position=position_dodge(), colour="black")+
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.key = element_blank(),
        legend.background = element_rect(colour = 'black'),
        legend.position = "bottom",
        plot.title = element_text(lineheight = .8, face = "bold", vjust = 1),
        axis.text.x = element_text(size = 9, angle = 45, vjust = 1.05,
                                   hjust = 1, face = "bold"),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 10, face = 'bold'),
        plot.background = element_rect(colour = 'black'))+
  ylab("Total Episode Rate per 100,000")+ggtitle("Hospital Episodes by Diagnosis for 75+, 2013-14")
ggsave(filename = paste(LAgraphsfolder, "EpiDiagnosis75.png", sep = "/"),
       width = 8, height = 5, dpi = 300) 

##Hospital Diagnosis 65+ ----------------------------------------------
diag_65<-diag_dt[-1,c(1,14,7)]
colnames(diag_65)<-c("Diagnosis", "Scotland Rate", "LOCALAUTHORITY CHP Rate")
diag_65<-melt(diag_65)
ggplot(data=diag_65, aes(x=Diagnosis, y=value, fill= variable))+
  geom_bar(stat="identity", position=position_dodge(), colour="black")+
  theme_bw()+
  theme(legend.title = element_blank(),
    legend.key = element_blank(),
        legend.background = element_rect(colour = 'black'),
        legend.position = "bottom",
        plot.title = element_text(lineheight = .8, face = "bold", vjust = 1),
        axis.text.x = element_text(size = 9, angle = 45, vjust = 1.05,
                                   hjust = 1, face = "bold"),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 10, face = 'bold'),
        plot.background = element_rect(colour = 'black'))+
  ylab("Total Episode Rate per 100,000")+ggtitle("Hospital Episodes by Diagnosis for 65+, 2013-14")
ggsave(filename = paste(LAgraphsfolder, "EpiDiagnosis65.png", sep = "/"),
       width = 8, height = 5, dpi = 300) 




#Injuries-------------------------------------
df_inj<-read.csv(paste(LAdatafolder, "Injuries.csv", sep = "/"), stringsAsFactors = FALSE)
df_inj[9,]<-c("All Other",(df_inj[7, -1]+ df_inj[8, -1]))
df_inj<-df_inj[-c(7,8),]
colnames(df_inj)<-c("Injury", "0-4", "5-9", "10-14","15-24", "25-44", "45-64", "65-74","75+")
df_inj<-melt(df_inj)
colnames(df_inj)[2]<-"Age Groups"
library(scales)
ggplot(data=df_inj, aes(x=reorder(Injury, -value),value, fill=`Age Groups`))+
  geom_bar(stat="identity")+geom_bar(stat="identity", colour = "black")+theme_bw()+
  theme(legend.key = element_blank(),
        legend.background = element_rect(colour = 'black'),
        legend.position = "bottom",
        plot.title = element_text(lineheight = .8, face = "bold", vjust = 1),
        axis.text.x = element_text(size = 10, angle = 90, vjust = 1,
                                   hjust = 1),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 10, face = 'bold'),
        plot.background = element_rect(colour = 'black'))+
  ##CHANGE!
  xlab("Injury Type")+ylab("Injury Count")+ggtitle("Unintended Injuries, LOCALAUTHORITY Community Health Partnership 2013-14") ##CHANGE
ggsave(filename = paste(LAgraphsfolder, "InjuryTypes.png", sep = "/"),
       width = 8, height = 5, dpi = 300) 