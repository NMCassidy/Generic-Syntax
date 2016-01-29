#'generic syntax for creating maps
#'Will need to replace LOCALAUTHORITY with Council being examined
#'watch out for spaces in council names
#'In addition you will need to change the zoom and centre on the map (lines 53+56)
#'Also need to play about with legend position/size and labels!

##Create the graphs folder if it does not exist (make sure to replace LOCALAUTHORITY!!)
LAfolder<-paste("S:/G - Governance & Performance Mngmt/Research Team/Fire Research/Assessments-Rproject/Additional Reports", "LOCALAUTHORITY", sep="/")
dir.create(LAfolder)
##create Maps folder if it doesn't exist
LAmapsfolder<-paste(LAfolder, "Maps", sep = "/")
dir.create(LAmapsfolder)

# Packages
require(ggplot2)     # Graphics
require(ggthemes)    # Extra themes
require(ggmap)       # Background map
require(XLConnect)   # Connect to Excel file
require(dplyr)       # Basic data manipulation
require(rgdal)       # To read shapefiles
require(gsubfn)      # Alternative way of generating cuts
require(Hmisc)       # Same as above
require(RColorBrewer)
#Nice cuts function
source('S:/G - Governance & Performance Mngmt/Research Team/Generic R Tools/Function - cuts - improved.R')

##Data Sourcing----------------------------------

#Read Data
dzs01_dta<-readRDS(file = "S:/G - Governance & Performance Mngmt/Research Team/Fire Research/Assessments-Rproject/data/SNSdata.rds")

## Load prepared data zone shapefiles
dzs01_map_fort <- readRDS(file = "S:/G - Governance & Performance Mngmt/Research Team/Fire Research/Assessments-Rproject/data/dzs01_fort.rds")

cols_keep <- c("datazone_2001","Council","csempdeprived_2011")

# Subset the data to match the selected indicator, no need to change anything
mstr_map_dta <- dzs01_dta[,(names(dzs01_dta) %in% cols_keep)]
# Merge map file with the indicator and data file
mstr_map_dta <- merge(x = dzs01_map_fort, y = mstr_map_dta,
                      by.x = "id", by.y = "datazone_2001", all.x = TRUE)

# Subset data  to get LOCALAUTHORITY
mstr_map_dta_cln <- subset(x = mstr_map_dta, subset = Council == "LOCALAUTHORITY")


##get nice breaks
mstr_map_dta_cln<-mstr_map_dta_cln[order(mstr_map_dta_cln$`csempdeprived_2011`),]
mstr_map_dta_cln$brks<-nice.cuts2(mstr_map_dta_cln$`csempdeprived_2011`)

# Background LOCALAUTHORITY map, zoom
## Geocode to have centre
CentreOfMap <- geocode(location = "CENTRE, UK")                   #CHANGE THIS!!!!!!!!
## Get background
backgr_map_zoom <- get_map(c(lon = CentreOfMap$lon,             
                               lat = CentreOfMap$lat), zoom = ,   #SET ZOOM!!
                             maptype = "terrain", source = "google")

# Apply ggmap
backgr_map_zoom <- ggmap(backgr_map_zoom, extent = "normal")

par(mar = c(0,0,0,0))
clrs<-rev(brewer.pal(length(levels(mstr_map_dta_cln$brks)), "RdYlGn"))


map_cncl_dzs_b <- backgr_map_zoom +
  geom_polygon(data = mstr_map_dta_cln, aes(x = lng, y = lat,
                                       fill = brks,
                                       group = group),
               alpha = 0.8, colour = 'grey18', size = 0.15) +
  scale_fill_manual(name = "% Deprived", values = clrs) +
  ggtitle("\nEmployment Deprivation 2011") +
  guides(fill = guide_legend(override.aes = list(colour = NULL))) +
  theme_map() +
  theme(legend.background = element_rect(colour = 'black'),
        plot.title = element_text(face = 'bold', size = 11),
        legend.position = c(0.71,0.28),  ##<<------CHANGE THIS FOR ALL MAPS
        legend.key.size = unit(2.5, "mm"),
     #   legend.text = element_text(size = 5),
        legend.title = element_text(face = 'bold'),
        legend.background = element_rect(size = 0.05, colour = 'black',
                                         linetype = 'solid'),
        plot.margin = unit(rep(0,4),"mm"),
        panel.margin = unit(rep(1,4),"mm"))

map_cncl_dzs_b

ggsave(filename = paste(LAmapsfolder, "empdep.png", sep = "/"),
       plot = map_cncl_dzs_b,  width=8, height=6.5, 
       units= "in", dpi = 450, scale = 0.75)


#===============================================
##Other Maps beginning with income deprivation
cols_keep <- c("datazone_2001","Council", "csincdeprived_2011")

# Subset the data to match the selected indicator, no need to change anything
mstr_map_dta <- dzs01_dta[,(names(dzs01_dta) %in% cols_keep)]
# Merge map file with the indicator and data file
mstr_map_dta <- merge(x = dzs01_map_fort, y = mstr_map_dta,
                      by.x = "id", by.y = "datazone_2001", all.x = TRUE)

# Subset data  to get LOCALAUTHORITY
mstr_map_dta_cln <- subset(x = mstr_map_dta, subset = Council == "LOCALAUTHORITY")

##get nice breaks
mstr_map_dta_cln<-mstr_map_dta_cln[order(mstr_map_dta_cln[["csincdeprived_2011"]]),]
mstr_map_dta_cln$brks<-nice.cuts2(mstr_map_dta_cln[["csincdeprived_2011"]], 5)

par(mar = c(0,0,0,0))
clrs<-c(rev(brewer.pal(length(levels(mstr_map_dta_cln$brks)), "RdYlGn")))


map_cncl_dzs_b <- backgr_map_zoom +
  geom_polygon(data = mstr_map_dta_cln, aes(x = lng, y = lat,
                                            fill = brks,
                                            group = group),
               alpha = 0.8, colour = 'grey18', size = 0.15) +
  scale_fill_manual(name = "% Deprived", values = clrs) +
  ggtitle("Percentage Income Deprived 2011") +
  guides(fill = guide_legend(override.aes = list(colour = NULL))) +
  theme_map() +
  theme(legend.background = element_rect(colour = 'black'),
        plot.title = element_text(face = 'bold', size = 11),
        legend.position = c(0.71,0.28),
        legend.key.size = unit(2.5, "mm"),
        #   legend.text = element_text(size = 5),
        legend.title = element_text(face = 'bold'),
        legend.background = element_rect(size = 0.05, colour = 'black',
                                         linetype = 'solid'),
        plot.margin = unit(rep(0,4),"mm"),
        panel.margin = unit(rep(1,4),"mm"))

map_cncl_dzs_b

ggsave(filename = paste(LAmapsfolder, "IncDep.png", sep = "/"),
       plot = map_cncl_dzs_b,  width=8, height=6.5, 
       units= "in", dpi = 450, scale = 0.75)

##SIMD 2012 Income Rank=========================================================
#This is already created by the housing and SIMD syntax
#but may wish to use this instead as it does not use deciles
cols_keep <- c("datazone_2001","Council", "csincrk_2012")

# Subset the data to match the selected indicator, no need to change anything
mstr_map_dta <- dzs01_dta[,(names(dzs01_dta) %in% cols_keep)]
# Merge map file with the indicator and data file
mstr_map_dta <- merge(x = dzs01_map_fort, y = mstr_map_dta,
                      by.x = "id", by.y = "datazone_2001", all.x = TRUE)

# Subset data  to get LOCALAUTHORITY
mstr_map_dta_cln <- subset(x = mstr_map_dta, subset = Council == "LOCALAUTHORITY")

##get nice breaks
mstr_map_dta_cln<-mstr_map_dta_cln[order(mstr_map_dta_cln[["csincrk_2012"]]),]
mstr_map_dta_cln$brks<-nice.cuts2(mstr_map_dta_cln[["csincrk_2012"]], 6)

par(mar = c(0,0,0,0))
clrs<-c(brewer.pal(length(levels(mstr_map_dta_cln$brks)) , "RdYlGn"))

map_cncl_dzs_b <- backgr_map_zoom +
  geom_polygon(data = mstr_map_dta_cln, aes(x = lng, y = lat,
                                            fill = brks,
                                            group = group),
               alpha = 0.8, colour = 'grey18', size = 0.15) +
  scale_fill_manual(name = "Rank", values = clrs, labels = c("1-999", "1000-1999", "2000-2999", "3000-3999", "4000-4999", "5000-5999", "6000-6501")) +
  ggtitle("SIMD Income Rank 2012") +
  guides(fill = guide_legend(override.aes = list(colour = NULL))) +
  theme_map() +
  theme(legend.background = element_rect(colour = 'black'),
        plot.title = element_text(face = 'bold', size = 11),
        legend.position = c(0.71,0.28),
        legend.key.size = unit(2.5, "mm"),
        #   legend.text = element_text(size = 5),
        legend.title = element_text(face = 'bold'),
        legend.background = element_rect(size = 0.05, colour = 'black',
                                         linetype = 'solid'),
        plot.margin = unit(rep(0,4),"mm"),
        panel.margin = unit(rep(1,4),"mm"))

map_cncl_dzs_b

ggsave(filename = paste(LAmapsfolder, "SIMDInc.png", sep = "/"),
       plot = map_cncl_dzs_b,  width=8, height=6.5, 
       units= "in", dpi = 450, scale = 0.75)
#SIMD Rank ===================================================
##This is also available through housing and SIMD syntax
cols_keep <- c("datazone_2001","Council", "cssimdrk_2012")

# Subset the data to match the selected indicator, no need to change anything
mstr_map_dta <- dzs01_dta[,(names(dzs01_dta) %in% cols_keep)]
# Merge map file with the indicator and data file
mstr_map_dta <- merge(x = dzs01_map_fort, y = mstr_map_dta,
                      by.x = "id", by.y = "datazone_2001", all.x = TRUE)

# Subset data  to get LOCALAUTHORITY
mstr_map_dta_cln <- subset(x = mstr_map_dta, subset = Council == "LOCALAUTHORITY")

##get nice breaks
mstr_map_dta_cln<-mstr_map_dta_cln[order(mstr_map_dta_cln[["cssimdrk_2012"]]),]
mstr_map_dta_cln$brks<-nice.cuts2(mstr_map_dta_cln[["cssimdrk_2012"]], 5)

par(mar = c(0,0,0,0))
clrs<-c(brewer.pal(length(levels(mstr_map_dta_cln$brks)) , "RdYlGn"))

map_cncl_dzs_b <- backgr_map_zoom +
  geom_polygon(data = mstr_map_dta_cln, aes(x = lng, y = lat,
                                            fill = brks,
                                            group = group),
               alpha = 0.8, colour = 'grey18', size = 0.15) +
  scale_fill_manual(name = "Rank", values = clrs, labels = c("1-999", "1000-1999", "2000-2999", "3000-3999", "4000-4999", "5000-5999", "6000-6501")) +
  ggtitle("SIMD Rank 2012") +
  guides(fill = guide_legend(override.aes = list(colour = NULL))) +
  theme_map() +
  theme(legend.background = element_rect(colour = 'black'),
        plot.title = element_text(face = 'bold', size = 11),
        legend.position = c(0.71,0.28),
        legend.key.size = unit(2.5, "mm"),
        #   legend.text = element_text(size = 5),
        legend.title = element_text(face = 'bold'),
        legend.background = element_rect(size = 0.05, colour = 'black',
                                         linetype = 'solid'),
        plot.margin = unit(rep(0,4),"mm"),
        panel.margin = unit(rep(1,4),"mm"))

map_cncl_dzs_b

ggsave(filename = paste(LAmapsfolder, "SIMD.png", sep = "/"),
       plot = map_cncl_dzs_b,  width=8, height=6.5, 
       units= "in", dpi = 450, scale = 0.75)

##Council Tax Bands A-C================================================
cols_keep <- c("datazone_2001","Council", "hoperbandac_2013")

# Subset the data to match the selected indicator, no need to change anything
mstr_map_dta <- dzs01_dta[,(names(dzs01_dta) %in% cols_keep)]
# Merge map file with the indicator and data file
mstr_map_dta <- merge(x = dzs01_map_fort, y = mstr_map_dta,
                      by.x = "id", by.y = "datazone_2001", all.x = TRUE)

# Subset data  to get LOCALAUTHORITY
mstr_map_dta_cln <- subset(x = mstr_map_dta, subset = Council == "LOCALAUTHORITY")

##get nice breaks
mstr_map_dta_cln<-mstr_map_dta_cln[order(mstr_map_dta_cln[["hoperbandac_2013"]]),]
mstr_map_dta_cln$brks<-nice.cuts2(mstr_map_dta_cln[["hoperbandac_2013"]], 5)

par(mar = c(0,0,0,0))
clrs<-c(rev(brewer.pal(length(levels(mstr_map_dta_cln$brks)), "RdYlGn")))

map_cncl_dzs_b <- backgr_map_zoom +
  geom_polygon(data = mstr_map_dta_cln, aes(x = lng, y = lat,
                                            fill = brks,
                                            group = group),
               alpha = 0.8, colour = 'grey18', size = 0.15) +
  scale_fill_manual(name = "% Dwellings", values = clrs) +
  ggtitle("CTB A-C 2013") +
  guides(fill = guide_legend(override.aes = list(colour = NULL))) +
  theme_map() +
  theme(legend.background = element_rect(colour = 'black'),
        plot.title = element_text(face = 'bold', size = 11),
        legend.position = c(0.71,0.28),
        legend.key.size = unit(2.5, "mm"),
        #   legend.text = element_text(size = 5),
        legend.title = element_text(face = 'bold'),
        legend.background = element_rect(size = 0.05, colour = 'black',
                                         linetype = 'solid'),
        plot.margin = unit(rep(0,4),"mm"),
        panel.margin = unit(rep(1,4),"mm"))

map_cncl_dzs_b

ggsave(filename = paste(LAmapsfolder, "CTB_AC.png", sep = "/"),
       plot = map_cncl_dzs_b,  width=8, height=6.5, 
       units= "in", dpi = 450, scale = 0.75)
#Emergency Admissions========================
cols_keep <- c("datazone_2001","Council", "hsr9_2012")

# Subset the data to match the selected indicator, no need to change anything
mstr_map_dta <- dzs01_dta[,(names(dzs01_dta) %in% cols_keep)]
# Merge map file with the indicator and data file
mstr_map_dta <- merge(x = dzs01_map_fort, y = mstr_map_dta,
                      by.x = "id", by.y = "datazone_2001", all.x = TRUE)

# Subset data  to get LOCALAUTHORITY
mstr_map_dta_cln <- subset(x = mstr_map_dta, subset = Council == "LOCALAUTHORITY")

##get nice breaks
mstr_map_dta_cln<-mstr_map_dta_cln[order(mstr_map_dta_cln[["hsr9_2012"]]),]
mstr_map_dta_cln$brks<-nice.cuts2(mstr_map_dta_cln[["hsr9_2012"]], 5)

par(mar = c(0,0,0,0))
clrs<-c(rev(brewer.pal(length(levels(mstr_map_dta_cln$brks)), "RdYlGn")))


map_cncl_dzs_b <- backgr_map_zoom +
  geom_polygon(data = mstr_map_dta_cln, aes(x = lng, y = lat,
                                            fill = brks,
                                            group = group),
               alpha = 0.8, colour = 'grey18', size = 0.15) +
  scale_fill_manual(name = "Admissions", values = clrs) +
  ggtitle("Emergency Admissions Rate 2012") +
  guides(fill = guide_legend(override.aes = list(colour = NULL))) +
  theme_map() +
  theme(legend.background = element_rect(colour = 'black'),
        plot.title = element_text(face = 'bold', size = 11),
        legend.position = c(0.71,0.28),
        legend.key.size = unit(2.5, "mm"),
        #   legend.text = element_text(size = 5),
        legend.title = element_text(face = 'bold'),
        legend.background = element_rect(size = 0.05, colour = 'black',
                                         linetype = 'solid'),
        plot.margin = unit(rep(0,4),"mm"),
        panel.margin = unit(rep(1,4),"mm"))

map_cncl_dzs_b

ggsave(filename = paste(LAmapsfolder, "EmAd.png", sep = "/"),
       plot = map_cncl_dzs_b,  width=8, height=6.5, 
       units= "in", dpi = 450, scale = 0.75)
#Emergency Admissions Over 65s==========================
cols_keep <- c("datazone_2001","Council", "hsr12_2012")

# Subset the data to match the selected indicator, no need to change anything
mstr_map_dta <- dzs01_dta[,(names(dzs01_dta) %in% cols_keep)]
# Merge map file with the indicator and data file
mstr_map_dta <- merge(x = dzs01_map_fort, y = mstr_map_dta,
                      by.x = "id", by.y = "datazone_2001", all.x = TRUE)

# Subset data  to get LOCALAUTHORITY
mstr_map_dta_cln <- subset(x = mstr_map_dta, subset = Council == "LOCALAUTHORITY")

##get nice breaks
mstr_map_dta_cln<-mstr_map_dta_cln[order(mstr_map_dta_cln[["hsr12_2012"]]),]
mstr_map_dta_cln$brks<-nice.cuts2(mstr_map_dta_cln[["hsr12_2012"]], 5)

par(mar = c(0,0,0,0))
clrs<-c(rev(brewer.pal(length(levels(mstr_map_dta_cln$brks)), "RdYlGn")))


map_cncl_dzs_b <- backgr_map_zoom +
  geom_polygon(data = mstr_map_dta_cln, aes(x = lng, y = lat,
                                            fill = brks,
                                            group = group),
               alpha = 0.8, colour = 'grey18', size = 0.15) +
  scale_fill_manual(name = "Admissions", values = clrs) +
  ggtitle("Emergency Admissions Rate Over 65s 2012") +
  guides(fill = guide_legend(override.aes = list(colour = NULL))) +
  theme_map() +
  theme(legend.background = element_rect(colour = 'black'),
        plot.title = element_text(face = 'bold', size = 11),
        legend.position = c(0.71,0.28),
        legend.key.size = unit(2.5, "mm"),
        #   legend.text = element_text(size = 5),
        legend.title = element_text(face = 'bold'),
        legend.background = element_rect(size = 0.05, colour = 'black',
                                         linetype = 'solid'),
        plot.margin = unit(rep(0,4),"mm"),
        panel.margin = unit(rep(1,4),"mm"))

map_cncl_dzs_b

ggsave(filename = paste(LAmapsfolder, "EmAd65.png", sep = "/"),
       plot = map_cncl_dzs_b,  width=8, height=6.5, 
       units= "in", dpi = 450, scale = 0.75)

#Accidents==========================================
cols_keep <- c("datazone_2001","Council", "HSr93_2012")

# Subset the data to match the selected indicator, no need to change anything
mstr_map_dta <- dzs01_dta[,(names(dzs01_dta) %in% cols_keep)]
# Merge map file with the indicator and data file
mstr_map_dta <- merge(x = dzs01_map_fort, y = mstr_map_dta,
                      by.x = "id", by.y = "datazone_2001", all.x = TRUE)

# Subset data  to get LOCALAUTHORITY
mstr_map_dta_cln <- subset(x = mstr_map_dta, subset = Council == "LOCALAUTHORITY")

##get nice breaks
mstr_map_dta_cln<-mstr_map_dta_cln[order(mstr_map_dta_cln[["HSr93_2012"]]),]
mstr_map_dta_cln$brks<-nice.cuts2(mstr_map_dta_cln[["HSr93_2012"]], 5)

par(mar = c(0,0,0,0))
clrs<-c(rev(brewer.pal(length(levels(mstr_map_dta_cln$brks)), "RdYlGn")))


map_cncl_dzs_b <- backgr_map_zoom +
  geom_polygon(data = mstr_map_dta_cln, aes(x = lng, y = lat,
                                            fill = brks,
                                            group = group),
               alpha = 0.8, colour = 'grey18', size = 0.15) +
  scale_fill_manual(name = "Accidents", values = clrs) +
  ggtitle("Accident Admissions Rate 2012") +
  guides(fill = guide_legend(override.aes = list(colour = NULL))) +
  theme_map() +
  theme(legend.background = element_rect(colour = 'black'),
        plot.title = element_text(face = 'bold', size = 11),
        legend.position = c(0.71,0.28),
        legend.key.size = unit(2.5, "mm"),
        #   legend.text = element_text(size = 5),
        legend.title = element_text(face = 'bold'),
        legend.background = element_rect(size = 0.05, colour = 'black',
                                         linetype = 'solid'),
        plot.margin = unit(rep(0,4),"mm"),
        panel.margin = unit(rep(1,4),"mm"))

map_cncl_dzs_b

ggsave(filename = paste(LAmapsfolder, "Accidents.png", sep = "/"),
       plot = map_cncl_dzs_b,  width=8, height=6.5, 
       units= "in", dpi = 450, scale = 0.75)
#Coronary Heart Disease====================================================
cols_keep <- c("datazone_2001","Council", "hs.r33 2012")

# Subset the data to match the selected indicator, no need to change anything
mstr_map_dta <- dzs01_dta[,(names(dzs01_dta) %in% cols_keep)]
# Merge map file with the indicator and data file
mstr_map_dta <- merge(x = dzs01_map_fort, y = mstr_map_dta,
                      by.x = "id", by.y = "datazone_2001", all.x = TRUE)

# Subset data  to get LOCALAUTHORITY
mstr_map_dta_cln <- subset(x = mstr_map_dta, subset = Council == "LOCALAUTHORITY")

##get nice breaks
mstr_map_dta_cln<-mstr_map_dta_cln[order(mstr_map_dta_cln[["hs.r33 2012"]]),]
mstr_map_dta_cln$brks<-nice.cuts2(mstr_map_dta_cln[["hs.r33 2012"]], 5)

par(mar = c(0,0,0,0))
clrs<-c(rev(brewer.pal(length(levels(mstr_map_dta_cln$brks)), "RdYlGn")))


map_cncl_dzs_b <- backgr_map_zoom +
  geom_polygon(data = mstr_map_dta_cln, aes(x = lng, y = lat,
                                            fill = brks,
                                            group = group),
               alpha = 0.8, colour = 'grey18', size = 0.15) +
  scale_fill_manual(name = "Disease Rate", values = clrs) +
  ggtitle("Coronary Heart Disease Rate 2012") +
  guides(fill = guide_legend(override.aes = list(colour = NULL))) +
  theme_map() +
  theme(legend.background = element_rect(colour = 'black'),
        plot.title = element_text(face = 'bold', size = 11),
        legend.position = c(0.71,0.28),
        legend.key.size = unit(2.5, "mm"),
        #   legend.text = element_text(size = 5),
        legend.title = element_text(face = 'bold'),
        legend.background = element_rect(size = 0.05, colour = 'black',
                                         linetype = 'solid'),
        plot.margin = unit(rep(0,4),"mm"),
        panel.margin = unit(rep(1,4),"mm"))

map_cncl_dzs_b

ggsave(filename = paste(LAmapsfolder, "HeartDis.png", sep = "/"),
       plot = map_cncl_dzs_b,  width=8, height=6.5, 
       units= "in", dpi = 450, scale = 0.75)