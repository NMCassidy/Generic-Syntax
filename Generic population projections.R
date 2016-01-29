#'This is the generic syntax for creating the population projection line charts
#'You will need to change LOCALAUTHORITY to the relevant name
#' - "City of Edinburgh" should be entered as "Edinburgh"
#'Make sure the correct csv file is read!
#'Be careful with spaces in Council names

rm(list = ls())
##Create the graphs folder if it does not exist (make sure to replace LOCALAUTHORITY!!)
LAfolder<-paste("S:/G - Governance & Performance Mngmt/Research Team/Fire Research/Assessments-Rproject/Additional Reports", "LOCALAUTHORITY", sep="/")
dir.create(LAfolder)
LAgraphsfolder<-paste(LAfolder, "Graphs", sep = "/")
dir.create(LAgraphsfolder)
#Data folder
LAdatafolder<-paste(LAfolder, "Data", sep = "/")

#load required packages
require(dplyr)
require (ggplot2)

# Read the annual population projections from Nomis for Scotland
dta_cncls_age <- read.csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_31_1.data.csv?geography=1941962927...1941962958&sex=7&age=1...19,22,24,25&measures=20100&select=date_name,geography_name,geography_code,sex_name,age_name,measures_name,obs_value,obs_status_name", stringsAsFactors = FALSE)
#need to rename Edinburgh for simplicity's sake
dta_cncls_age[dta_cncls_age$GEOGRAPHY_NAME == "Edinburgh, City of",2]<-"Edinburgh"
# Create flag for the LOCALAUTHORITY geographies
dta_cncls_age$IsLOCALAUTHORITY <- ifelse(dta_cncls_age$GEOGRAPHY_NAME == "LOCALAUTHORITY",
                                      "LOCALAUTHORITY","Rest of Scotland")
#load projected data, this is sourced from NRS
proj<-read.csv(paste(LAdatafolder, "demo_proj.csv", sep = "/"))
proj65<-proj[66:91,]

##Convert to numeric
proj65<-as.data.frame(apply(proj65, 2, function(y) as.numeric(gsub(",", "", y))))
projdat<-apply(proj65,2, sum)

DATE_NAME<-c("AGES",2012:2037)
projdat<-cbind(DATE_NAME, projdat)
projdat<-as.data.frame(projdat)

projdat<-projdat[5:27,]
projdat<-as.data.frame(apply(projdat,2,as.numeric))
colnames(projdat)[2]<-"OBS_VALUE"
dta2 = subset(dta_cncls_age, subset = IsLOCALAUTHORITY == "LOCALAUTHORITY" & 
                (AGE_NAME == "Aged 65 - 69 years" |AGE_NAME == "Aged 70 - 74 years"| 
                   AGE_NAME == "Aged 75 - 79 years"| AGE_NAME == "Aged 80 - 84 years"|
                   AGE_NAME == "Aged 85 and over"))
ov65<-aggregate(OBS_VALUE~DATE_NAME, data=dta2, sum)
realproj<-rbind(ov65,projdat)

realproj$pchange<-(realproj$OBS_VALUE/lag(realproj$OBS_VALUE, 1) -1)*100
realproj$porig<-(realproj$OBS_VALUE/realproj$OBS_VALUE[1])*100

# Create graph for year on year change across LOCALAUTHORITY---------------
ggplot(data = realproj) +
  aes(x = DATE_NAME, y = pchange) +
  geom_line(size=1.3) +
  geom_vline(xintercept=2014)+
  xlab("Year") +
  ylab("Percentage Change") +
  scale_x_continuous(breaks = seq(from = 1980, to = 2037, by = 2)) +
  ggtitle("LOCALAUTHORITY\nPopulation Change Over 65") +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 12, colour = "black"),
        axis.text.x = element_text(angle = 90, hjust = 1.1, vjust = 0.5),
        axis.title = element_text(size = 14, colour = "black"),
        plot.title = element_text(face = "bold"),
        plot.background = element_rect(colour = "black"))
ggsave(filename = paste(LAgraphsfolder,"YrChnge65.png", sep = "/"),
       width = 8, height = 4, dpi = 300)

#change from 1980-----------------------------------------------------
ggplot(data = realproj) +
  aes(x = DATE_NAME, y = porig) +
  geom_line(size=1.3) +
  geom_vline(xintercept=2014)+
  xlab("Year") +
  ylab("Percentage Change") +
  scale_x_continuous(breaks = seq(from = 1980, to = 2037, by = 2)) +
  ggtitle("LOCALAUTHORITY\nPopulation Change Over 65") +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 12, colour = "black"),
        axis.text.x = element_text(angle = 90, hjust = 1.1, vjust = 0.5),
        axis.title = element_text(size = 14, colour = "black"),
        plot.title = element_text(face = "bold"),
        plot.background = element_rect(colour = "black"))
ggsave(filename = paste(LAgraphsfolder,"ChngePerc65.png", sep = "/"),
       width = 8, height = 4, dpi = 300)

##Over75s with projected figures===========================
proj75<-proj[76:91,]

##Convert to numeric
proj75<-as.data.frame(apply(proj75, 2, function(y) as.numeric(gsub(",", "", y))))
projdat<-apply(proj75,2, sum)

DATE_NAME<-c("AGES",2012:2037)
projdat<-cbind(DATE_NAME, projdat)
projdat<-as.data.frame(projdat)
dta2 = subset(dta_cncls_age, subset = IsLOCALAUTHORITY == "LOCALAUTHORITY" & 
                (AGE_NAME == "Aged 75 - 79 years"| AGE_NAME == "Aged 80 - 84 years"|
                   AGE_NAME == "Aged 85 and over"))
ov75<-aggregate(OBS_VALUE~DATE_NAME, data=dta2, sum)
projdat<-projdat[5:27,]
projdat<-as.data.frame(apply(projdat,2,as.numeric))
colnames(projdat)[2]<-"OBS_VALUE"
realproj<-rbind(ov75,projdat)

realproj$pchange<-(realproj$OBS_VALUE/lag(realproj$OBS_VALUE, 1) -1)*100
realproj$porig<-(realproj$OBS_VALUE/realproj$OBS_VALUE[1])*100

# Create graph for year on year change across LOCALAUTHORITY
ggplot(data = realproj) +
  aes(x = DATE_NAME, y = pchange) +
  geom_line(size=1.3) +
  geom_vline(xintercept=2014)+
  xlab("Year") +
  ylab("Percentage Change") +
  scale_x_continuous(breaks = seq(from = 1980, to = 2037, by = 2)) +
  ggtitle("LOCALAUTHORITY\nPopulation Change Over 75") +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 12, colour = "black"),
        axis.text.x = element_text(angle = 90, hjust = 1.1, vjust = 0.5),
        axis.title = element_text(size = 14, colour = "black"),
        plot.title = element_text(face = "bold"),
        plot.background = element_rect(colour = "black"))
ggsave(filename = paste(LAgraphsfolder,"YrChnge75.png", sep = "/"),
       width = 8, height = 4, dpi = 300)

# Change since 1980------------------------------------------------
ggplot(data = realproj) +
  aes(x = DATE_NAME, y = porig) +
  geom_line(size=1.3) +
  geom_vline(xintercept=2014)+
  xlab("Year") +
  ylab("Percentage Change") +
  scale_x_continuous(breaks = seq(from = 1980, to = 2037, by = 2)) +
  ggtitle("LOCALAUTHORITY\nPopulation Change Over 75") +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 12, colour = "black"),
        axis.text.x = element_text(angle = 90, hjust = 1.1, vjust = 0.5),
        axis.title = element_text(size = 14, colour = "black"),
        plot.title = element_text(face = "bold"),
        plot.background = element_rect(colour = "black"))
ggsave(filename = paste(LAgraphsfolder,"ChngePerc75.png", sep = "/"),
       width = 8, height = 4, dpi = 300)


##Over85s with projected figures===========================
proj85<-proj[86:91,]

#Convert to numeric
proj85<-as.data.frame(apply(proj85, 2, function(y) as.numeric(gsub(",", "", y))))
projdat<-apply(proj85,2, sum)

DATE_NAME<-c("AGES",2012:2037)
projdat<-cbind(DATE_NAME, projdat)
projdat<-as.data.frame(projdat)
dta2 = subset(dta_cncls_age, subset = IsLOCALAUTHORITY == "LOCALAUTHORITY" & 
                (AGE_NAME == "Aged 85 and over"))
ov85<-aggregate(OBS_VALUE~DATE_NAME, data=dta2, sum)
projdat<-projdat[5:27,]
projdat<-as.data.frame(apply(projdat,2,as.numeric))
colnames(projdat)[2]<-"OBS_VALUE"
realproj<-rbind(ov85,projdat)

realproj$pchange<-(realproj$OBS_VALUE/lag(realproj$OBS_VALUE, 1) -1)*100
realproj$porig<-(realproj$OBS_VALUE/realproj$OBS_VALUE[1])*100

# Create graph for year on year change across LOCALAUTHORITY
ggplot(data = realproj) +
  aes(x = DATE_NAME, y = pchange) +
  geom_line(size=1.3) +
  geom_vline(xintercept=2014)+
  xlab("Year") +
  ylab("Percentage Change") +
  scale_x_continuous(breaks = seq(from = 1980, to = 2037, by = 2)) +
  ggtitle("LOCALAUTHORITY\nPopulation Change Over 85") +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 12, colour = "black"),
        axis.text.x = element_text(angle = 90, hjust = 1.1, vjust = 0.5),
        axis.title = element_text(size = 14, colour = "black"),
        plot.title = element_text(face = "bold"),
        plot.background = element_rect(colour = "black"))
ggsave(filename = paste(LAgraphsfolder,"YrChnge85.png", sep = "/"),
       width = 8, height = 4, dpi = 300)
# Change since 1980-------------------------------------------------
ggplot(data = realproj) +
  aes(x = DATE_NAME, y = porig) +
  geom_line(size=1.3) +
  geom_vline(xintercept=2014)+
  xlab("Year") +
  ylab("Percentage Change") +
  scale_x_continuous(breaks = seq(from = 1980, to = 2037, by = 2)) +
  ggtitle("LOCALAUTHORITY\nPopulation Change Over 85") +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 12, colour = "black"),
        axis.text.x = element_text(angle = 90, hjust = 1.1, vjust = 0.5),
        axis.title = element_text(size = 14, colour = "black"),
        plot.title = element_text(face = "bold"),
        plot.background = element_rect(colour = "black"))
ggsave(filename = paste(LAgraphsfolder,"ChngePerc85.png", sep = "/"),
       width = 8, height = 4, dpi = 300)