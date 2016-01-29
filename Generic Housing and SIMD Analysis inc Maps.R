# The following script sources the housing data and derives set of tabular and
# visual summaries
#This is the generic code. Replace LOCALAUTHORITY with the Council of choice!
#In addition you will need to find the correct centre of map and zoom for the LA
#this is on line 193 and 195
#You will also need to change the legend position or size - SEE LINE 219
#Be careful with Spaces in the LA name certain lines will not work if there is a space
#the code will also create a folder and add graphs to it
#Also I haven't saved the coucil tax bands maps as these are mad in the map syntax

# Clean and set up --------------------------------------------------------

# Clean env.
rm(list = ls())

# Libs
require(ggplot2)  # For graphs
require(reshape2) # For melting the data
require(ggthemes) # Extra themes
require(ggmap)    # For adding Google map
require(stringr)  # To trim empty spaces
require(plyr)     # To compute statistics by group
require(dplyr)    # To transform variable into deciles
require(psych)    # To generate summary by group
require(tables)   # To filter variables by type
require(RColorBrewer)
require(gtools)
#load pretty.cuts function
source('S:/G - Governance & Performance Mngmt/Research Team/Generic R Tools/Function - cuts - improved.R')

# Folders
fldr_dta <- "//wlhsan-cluster/improvement service/G - Governance & Performance Mngmt/Research Team/Fire Research/Assessments-Rproject/data"
##Create the graphs folder if it does not exist (make sure to replace LOCALAUTHORITY!!)
LAfolder<-paste("S:/G - Governance & Performance Mngmt/Research Team/Fire Research/Assessments-Rproject/Additional Reports", "LOCALAUTHORITY", sep="/")
dir.create(LAfolder)
LAgraphsfolder<-paste(LAfolder, "Graphs", sep = "/")
dir.create(LAgraphsfolder)
##And Maps folder
LAmapsfolder<-paste(LAfolder, "Maps", sep = "/")
dir.create(LAmapsfolder)

# Source data -------------------------------------------------------------

# Read CSV file
dta_sg_dwells <- read.csv(file =
                            paste(fldr_dta,
                                  "Chargeable Dwellings 1st September 2014.csv",
                                  sep = "/"))

# Change column to numeric, deleting all comas
cols = c(2:10);
dta_sg_dwells[,cols] = apply(dta_sg_dwells[,cols],
                             2, function(x)
                               as.numeric(as.character((gsub(",", "", x)))))

# Drop column with totals
dta_sg_dwells[,10] <- NULL

# Compute percentage of each column
dta_sg_dwells_per <- cbind(id = dta_sg_dwells[, 1],
                           (dta_sg_dwells[, -1]/rowSums(dta_sg_dwells[, -1]))
                           * 100)

# Sort by band D
dta_sg_dwells_per <- dta_sg_dwells_per[order(dta_sg_dwells_per$Band.D),]

# Melt the data
dta_sg_dwells_per_mlt <- melt(data = dta_sg_dwells_per, id.vars = "id")

# Delete "band" from column name
dta_sg_dwells_per_mlt$variable <- sub(pattern = "Band.",
                                      x = dta_sg_dwells_per_mlt$variable,
                                      replacement = "")

# Graph stacked bar chart-------------------------------------------------

ggplot(data = dta_sg_dwells_per_mlt, aes(x = id)) +
  geom_bar(aes(y = value, fill = variable), position = "stack",
           stat = "identity") +
  xlab("Council") +
  ylab("% of dwellings") +
  scale_fill_discrete(name = "Band") +
  ggtitle("Council Tax Bands") +
  theme(legend.key = element_blank(),
        legend.background = element_rect(colour = 'black'),
        legend.position = "bottom",
        plot.title = element_text(lineheight = .8, face = "bold", vjust = 1),
        axis.text.x = element_text(size = 9, angle = 90, vjust = 0.5,
                                   hjust = 1, colour = "black"),
        axis.text.y = element_text(size = 11, colour = 'black'),
        axis.title = element_text(size = 10, face = 'bold'),
        axis.line = element_line(colour = "black"),
        plot.background = element_rect(colour = 'black', size = 1),
        panel.background = element_blank())

## Save graph
ggsave(filename = paste(LAgraphsfolder, "council_band_las.png",sep = "/"),
       width = 12, height = 6, units = 'in', dpi = 600, scale = 0.75)

# Combine categories ------------------------------------------------------

dta_sg_dwells_per_mlt_cmb <- dta_sg_dwells_per_mlt
dta_sg_dwells_per_mlt_cmb$variable[
  dta_sg_dwells_per_mlt_cmb$variable == "A"] <- "A-C"
dta_sg_dwells_per_mlt_cmb$variable[
  dta_sg_dwells_per_mlt_cmb$variable == "B"] <- "A-C"
dta_sg_dwells_per_mlt_cmb$variable[
  dta_sg_dwells_per_mlt_cmb$variable == "C"] <- "A-C"
dta_sg_dwells_per_mlt_cmb$variable[
  dta_sg_dwells_per_mlt_cmb$variable == "D"] <- "D-E"
dta_sg_dwells_per_mlt_cmb$variable[
  dta_sg_dwells_per_mlt_cmb$variable == "E"] <- "D-E"
dta_sg_dwells_per_mlt_cmb$variable[
  dta_sg_dwells_per_mlt_cmb$variable == "F"] <- "F-H"
dta_sg_dwells_per_mlt_cmb$variable[
  dta_sg_dwells_per_mlt_cmb$variable == "G"] <- "F-H"
dta_sg_dwells_per_mlt_cmb$variable[
  dta_sg_dwells_per_mlt_cmb$variable == "H"] <- "F-H"

# Graph for categories ----------------------------------------------------

ggplot(data = dta_sg_dwells_per_mlt_cmb, aes(x = id)) +
  geom_bar(aes(y = value, fill = variable), position = "stack",
           stat = "identity") +
  xlab("Council") +
  ylab("% of dwellings") +
  scale_fill_discrete(name = "Band") +
  ggtitle("Council Tax Bands") +
  theme(legend.key = element_blank(),
        legend.background = element_rect(colour = 'black'),
        legend.position = "bottom",
        plot.title = element_text(lineheight = .8, face = "bold", vjust = 1),
        axis.text.x = element_text(size = 9, angle = 90, vjust = 0.5,
                                   hjust = 1, colour = "black"),
        axis.text.y = element_text(size = 11, colour = 'black'),
        axis.title = element_text(size = 10, face = 'bold'),
        axis.line = element_line(colour = "black"),
        plot.background = element_rect(colour = 'black', size = 1),
        panel.background = element_blank())

## Save graph
ggsave(filename = paste(LAgraphsfolder, "council_band_las_cat.png",sep = "/"),
       width = 12, height = 6, units = 'in', dpi = 600, scale = 0.75)

# Source data zone Council Tax --------------------------------------------

# Source data zone level Council Tax data
dta_dzs_tax_per <- read.csv(file = paste(fldr_dta, "dzs_dwells_per.csv",
                                         sep = "/"))

# Replace dot in column names
names(dta_dzs_tax_per) <- sub(pattern = "\\.", replacement = "-",
                              x = names(dta_dzs_tax_per))

# List rows where values are not equal 100
dta_dzs_tax_per[rowSums(dta_dzs_tax_per[,2:4]) < 100,]

# Merge data for the maps syntax ------------------------------------------

# Draws the map for the A-C band
# Load prepared data zone shapefiles
dzs01_map_fort <- readRDS(file = paste(fldr_dta, "dzs01_fort.rds",
                                       sep = "/"))
# Prepare the master data frame for the map
mstr_map_dta <- merge(x = dzs01_map_fort, y = dta_dzs_tax_per,
                      by.x = "id", by.y = "datazone", all.x = TRUE, sort = FALSE)
## Add column with local authority names
dta_geo <- read.csv(paste(fldr_dta,"datazones2001.csv",
                          sep = "/"))
## Change column to factor and trim white space
dta_geo$DataZone <- str_trim(as.character(dta_geo$Datazone2001))
mstr_map_dta$id <- str_trim(as.character(mstr_map_dta$id))
dta_geo$Council <- str_trim(as.character(dta_geo$Council.Name))
## Merge two data tables
mstr_map_dta <- merge(x = mstr_map_dta,
                      y = dta_geo[ , c("Datazone2001", "Council")],
                      by.x = "id", by.y = "Datazone2001", all.x = TRUE, sort = FALSE)
## Subset this gadget for Selected Local Authority
mstr_map_dta_cln <- subset(x = mstr_map_dta, subset = Council == "LOCALAUTHORITY")
#get pretty breaks for this indicator
##NB, because pretty breaks counts 0 as Na I had to revalue NA to 0, then
##relevel so that 0 was the first level in that "A-C_brks" factor
##then sort the column from high to low
mstr_map_dta_cln<-mstr_map_dta_cln[order(mstr_map_dta_cln$`A-C`),]
mstr_map_dta_cln$`A-C_brks`<-nice.cuts2(mstr_map_dta_cln$`A-C`)
mstr_map_dta_cln$`A-C_brks`<-revalue(mstr_map_dta_cln$`A-C_brks`, c("NA"="0"))
mstr_map_dta_cln$`A-C_brks`<-relevel(mstr_map_dta_cln$`A-C_brks`, "0")
mstr_map_dta_cln$`A-C_brks`<-sort(mstr_map_dta_cln$`A-C_brks`)
# Produce map -------------------------------------------------------------

# Get Google background map
##Need to Change both location anf zoom
CentreOfMap <- geocode(location = "CENTRE, UK")             #CHANGE THIS!!!!!!!!
LOCALAUTHORITYMap = get_map(c(lon = CentreOfMap$lon, lat = CentreOfMap$lat), 
                           zoom = ,                         #SET THIS!!!!!!!!!!!!!
                           maptype = "terrain", source = "google") 
# Apply ggmap
LOCALAUTHORITYMap <- ggmap(LOCALAUTHORITYMap, extent = "normal")

##create colour scheme
#map_colours <- brewer.pal(length(mixedsort(unique(
 # mstr_map_dta_cln$`A-C_brks`))), "YlOrRd")
        map_colours <- c("grey", brewer.pal(5, "YlOrRd"))
        
# Make basic map
ggmap(LOCALAUTHORITYMap, extent = 'panel') +
  geom_polygon(data = mstr_map_dta_cln, aes(x = lng, y = lat,
                                            group = group,
                                            fill = `A-C_brks`),
               alpha = 0.8,
               color = "black",
               size = 0.1) +
  scale_fill_manual(name = "% Bands A-C", values=map_colours, labels=c("0", "1-20","21-40","41-60","61-80","81-100") ) +
  ggtitle("Council Tax - Bands A-C") +
  guides(fill = guide_legend(override.aes = list(colour = NULL))) +
  theme_map() +
  theme(legend.background = element_rect(colour = 'black'),
        plot.title = element_text(face = 'bold', size = 11),
        legend.position = c(0.7,0.28),    #CHANGE THIS
        legend.key.size = unit(2.5, "mm"),
        #   legend.text = element_text(size = 5),
        legend.title = element_text(face = 'bold'),
        legend.background = element_rect(size = 0.05, colour = 'black',
                                         linetype = 'solid'),
        plot.margin = unit(rep(0,4),"mm"),
        panel.margin = unit(rep(1,4),"mm"))
# Save map
#ggsave(filename = paste(LAmapsfolder,"bandAC.png", sep = "/"),
#       dpi = 300)

# Map for bands C-D -------------------------------------------------------

# Make basic map
LOCALAUTHORITYMap +
  geom_polygon(data = mstr_map_dta_cln, aes(x = lng, y = lat,
                                            group = group, fill = `D-E`),
               alpha = 0.8, color = "black", size = 0.1) +
  scale_fill_manual(name = "% Bands D-E", values=map_colours, labels=c("0", "1-20","21-40","41-60","61-80","81-100") ) +
  ggtitle("Council Tax - Bands D-E") +
  guides(fill = guide_legend(override.aes = list(colour = NULL))) +
  theme_map() +
  theme(legend.background = element_rect(colour = 'black'),
        plot.title = element_text(face = 'bold', size = 11),
        legend.position = c(0.7,0.28),
        legend.key.size = unit(2.5, "mm"),
        #   legend.text = element_text(size = 5),
        legend.title = element_text(face = 'bold'),
        legend.background = element_rect(size = 0.05, colour = 'black',
                                         linetype = 'solid'),
        plot.margin = unit(rep(0,4),"mm"),
        panel.margin = unit(rep(1,4),"mm"))

# Save map
#ggsave(filename = paste(LAmapsfolder,"bandDE.png", sep = "/"),
#       dpi = 300)

# Maps last bands ---------------------------------------------------------

# Make basic map
LOCALAUTHORITYMap +
  geom_polygon(data = mstr_map_dta_cln, aes(x = lng, y = lat,
                                            group = group, fill = `D-E`),
               alpha = 0.8, color = "black", size = 0.1) +
  scale_fill_manual(name = "% Bands F-H", values=map_colours, labels=c("0", "1-20","21-40","41-60","61-80","81-100") ) +
  ggtitle("Council Tax - Bands F-H") +
  guides(fill = guide_legend(override.aes = list(colour = NULL))) +
  theme_map() +
  theme(legend.background = element_rect(colour = 'black'),
        plot.title = element_text(face = 'bold', size = 11),
        legend.position = c(0.7,0.28),
        legend.key.size = unit(2.5, "mm"),
        #   legend.text = element_text(size = 5),
        legend.title = element_text(face = 'bold'),
        legend.background = element_rect(size = 0.05, colour = 'black',
                                         linetype = 'solid'),
        plot.margin = unit(rep(0,4),"mm"),
        panel.margin = unit(rep(1,4),"mm"))
# Save map
#ggsave(filename = paste(LAmapsfolder,"bandFH.png", sep = "/"),
#       dpi = 300)

# Recode majority and make map --------------------------------------------

# Provide column with majority of dwellings
mstr_map_dta_cln$majority <- colnames(
  mstr_map_dta_cln[ ,c("A-C","D-E","F-H")])[apply(
    mstr_map_dta_cln[,c("A-C","D-E","F-H")],1,which.max)]

# Make map with majority of dwellings
LOCALAUTHORITYMap +
  geom_polygon(data = mstr_map_dta_cln, aes(x = lng, y = lat,
                                            group = group, fill = majority),
               alpha = 0.8, color = "black", size = 0.1) +
  scale_fill_discrete(name = "Majority Dwellings") +
  ggtitle("Prevalence - Council Tax Bands") +
  guides(fill = guide_legend(override.aes = list(colour = NULL))) +
  theme_map() +
  theme(legend.background = element_rect(colour = 'black'),
        plot.title = element_text(face = 'bold', size = 11),
        legend.position = c(0.7,0.28),
        legend.key.size = unit(2.5, "mm"),
        #   legend.text = element_text(size = 5),
        legend.title = element_text(face = 'bold'),
        legend.background = element_rect(size = 0.05, colour = 'black',
                                         linetype = 'solid'),
        plot.margin = unit(rep(0,4),"mm"),
        panel.margin = unit(rep(1,4),"mm"))

# Save map
ggsave(filename = paste(LAmapsfolder,"prevdwells.png", sep = "/"),
       dpi = 300)

# Source LA dwellings data and chart --------------------------------------

# Source local authority dwellings data
dta_la_dwells <- read.csv(paste(fldr_dta,"la_dwells.csv",sep = "/"))

# Melt the dwellings data
dta_la_dwells_mlt <- melt(data = dta_la_dwells,
                          id.vars = c("Local.Authority","OpenLayersMapCode"))
## Show head
head(dta_la_dwells_mlt)

## Clean column after was melted
dta_la_dwells_mlt$variable <- gsub(".2013","",dta_la_dwells_mlt$variable)
dta_la_dwells_mlt$variable <- gsub("Dwellings","",dta_la_dwells_mlt$variable)
dta_la_dwells_mlt$variable <- gsub("\\."," ",dta_la_dwells_mlt$variable)
dta_la_dwells_mlt$variable <- gsub("Semi detached",
                                   "Semi-detached",
                                   dta_la_dwells_mlt$variable)
dta_la_dwells_mlt$variable <- str_trim(dta_la_dwells_mlt$variable,
                                       side = c("both"))
unique(dta_la_dwells_mlt$variable)

## Make column thousands
dta_la_dwells_mlt$value <- dta_la_dwells_mlt$value / 1000

# Graph stacked bar chart
ggplot(data = dta_la_dwells_mlt, aes(x = Local.Authority)) +
  geom_bar(aes(y = value, fill = variable), position = "stack",
           stat = "identity") +
  xlab("Council") +
  ylab("Dwellings (thousands)") +
  scale_fill_discrete(name = "Band") +
  ggtitle("Total Dwellings") +
  theme(legend.key = element_blank(),
        legend.background = element_rect(colour = 'black'),
        legend.position = "bottom",
        plot.title = element_text(lineheight = .8, face = "bold", vjust = 1),
        axis.text.x = element_text(size = 11, angle = 90, vjust = 0.5,
                                   hjust = 1, colour = 'black'),
        axis.text.y = element_text(size = 11, colour = 'black'),
        axis.title = element_text(size = 10, face = 'bold'),
        axis.line = element_line(colour = "black"),
        plot.background = element_rect(colour = 'black', size = 1),
        panel.background = element_blank())

## Save graph
ggsave(filename = paste(LAgraphsfolder, "council_dwells_las.png",sep = "/"),
       width = 12, height = 7, units = 'in', dpi = 600, scale = 0.75)

# Make percentage LA dwellings data ---------------------------------------
## Compute percentage for group
dta_la_dwells_mlt_pers <- ddply(
  .data = dta_la_dwells_mlt, .(Local.Authority),
  mutate,
  perc_var = ((value / sum(value))*100)
)
# Draw percentage graph
ggplot(data = dta_la_dwells_mlt_pers, aes(x = Local.Authority)) +
  geom_bar(aes(y = perc_var, fill = variable), position = "stack",
           stat = "identity") +
  xlab("Council") +
  ylab("% of total dwellings") +
  scale_fill_discrete(name = "Band") +
  ggtitle("Dwellings") +
  theme(legend.key = element_blank(),
        legend.background = element_rect(colour = 'black'),
        legend.position = "bottom",
        plot.title = element_text(lineheight = .8, face = "bold", vjust = 1),
        axis.text.x = element_text(size = 11, angle = 90, vjust = 0.5,
                                   hjust = 1, colour = 'black'),
        axis.text.y = element_text(size = 11, colour = 'black'),
        axis.title = element_text(size = 10, face = 'bold'),
        axis.line = element_line(colour = "black"),
        plot.background = element_rect(colour = 'black', size = 1),
        panel.background = element_blank())

## Export file
ggsave(filename = paste(LAgraphsfolder, "council_dwells_las_pers.png",sep = "/"),
       width = 12, height = 7, units = 'in', dpi = 600, scale = 0.75)

# Source SIMD data and make map -------------------------------------------

# Read SIMD CSV file
dta_simd12_dz <- read.csv(file = "http://www.isdscotland.org/Products-and-Services/GPD-Support/Deprivation/SIMD/_docs/SIMD_2012/datazone_simd2012.csv")

# Group SIMD housing rank into deciles
dta_simd12_dz$simd2012_house_rank_dec <- ntile(x =
                                                 dta_simd12_dz$simd2012_house_rank,
                                               n = 10)

# Merge the data table with the map data
map_dta_simd12_dz <- merge(x = mstr_map_dta_cln,
                           y = dta_simd12_dz,
                           by.x = "id", by.y = "Datazone", all.x = TRUE,
                           all.y = FALSE, sort = FALSE)

# Make factor to work with discrete scale
map_dta_simd12_dz$simd2012_house_rank_dec <- as.factor(
  map_dta_simd12_dz$simd2012_house_rank_dec)

#some nice colours
clrs<-brewer.pal(length(levels(map_dta_simd12_dz$simd2012_house_rank_dec)), "RdYlGn")

# Make map with simd housing deciles
LOCALAUTHORITYMap +
  geom_polygon(data = map_dta_simd12_dz, aes(x = lng, y = lat,
                                             group = group,
                                             fill = simd2012_house_rank_dec),
               alpha = 0.8, color = "black", size = 0.1) +
  scale_fill_manual(name="Decile", values= clrs)+
  ggtitle("SIMD'12 Housing") +
  guides(fill = guide_legend(override.aes = list(colour = NULL))) +
  theme_map() +
  theme(legend.background = element_rect(colour = 'black'),
        plot.title = element_text(face = 'bold', size = 11),
        legend.position = c(0.7,0.28),
        legend.key.size = unit(2.5, "mm"),
        #   legend.text = element_text(size = 5),
        legend.title = element_text(face = 'bold'),
        legend.background = element_rect(size = 0.05, colour = 'black',
                                         linetype = 'solid'),
        plot.margin = unit(rep(0,4),"mm"),
        panel.margin = unit(rep(1,4),"mm"))

# Save map
ggsave(filename = paste(LAmapsfolder,"housing_simd.png", sep = "/"),
       dpi = 300)

##Map for SIMD Rank and Income Domain
# Group SIMD rank into deciles
dta_simd12_dz$simd2012_rank_dec <- ntile(x = dta_simd12_dz$simd2012rank,
                                               n = 10)
# Merge the data table with the map data
map_dta_simd12_dz <- merge(x = mstr_map_dta_cln,
                           y = dta_simd12_dz,
                           by.x = "id", by.y = "Datazone", all.x = TRUE,
                           all.y = FALSE, sort = FALSE)
# Make factor to work with discrete scale
map_dta_simd12_dz$simd2012_rank_dec <- as.factor(
  map_dta_simd12_dz$simd2012_rank_dec)
##some colours
clrs<-brewer.pal(length(levels(map_dta_simd12_dz$simd2012_rank_dec)), "RdYlGn")
##Make map for SIMD Deciles
LOCALAUTHORITYMap +
  geom_polygon(data = map_dta_simd12_dz, aes(x = lng, y = lat,
                                             group = group,
                                             fill = simd2012_rank_dec),
               alpha = 0.8, color = "black", size = 0.1) +
  scale_fill_manual(name="Decile", values= clrs)+
  ggtitle("SIMD'12") +
  guides(fill = guide_legend(override.aes = list(colour = NULL))) +
  theme_map() +
  theme(legend.background = element_rect(colour = 'black'),
        plot.title = element_text(face = 'bold', size = 11),
        legend.position = c(0.7,0.28),
        legend.key.size = unit(2.5, "mm"),
        #   legend.text = element_text(size = 5),
        legend.title = element_text(face = 'bold'),
        legend.background = element_rect(size = 0.05, colour = 'black',
                                         linetype = 'solid'),
        plot.margin = unit(rep(0,4),"mm"),
        panel.margin = unit(rep(1,4),"mm"))

ggsave(filename = paste(LAmapsfolder,"simd_dec.png", sep = "/"),
       dpi = 300)

##now for Income Domain
# Group SIMD income rank into deciles
dta_simd12_dz$simd2012_inc_rank_dec <- ntile(x = dta_simd12_dz$simd2012_inc_rank,
                                         n = 10)
# Merge the data table with the map data
map_dta_simd12_dz <- merge(x = mstr_map_dta_cln,
                           y = dta_simd12_dz,
                           by.x = "id", by.y = "Datazone", all.x = TRUE,
                           all.y = FALSE, sort = FALSE)
# Make factor to work with discrete scale
map_dta_simd12_dz$simd2012_inc_rank_dec <- as.factor(
  map_dta_simd12_dz$simd2012_inc_rank_dec)
##some colours
clrs<-brewer.pal(length(levels(map_dta_simd12_dz$simd2012_inc_rank_dec)), "RdYlGn")
##Make map for SIMD Deciles
LOCALAUTHORITYMap +
  geom_polygon(data = map_dta_simd12_dz, aes(x = lng, y = lat,
                                             group = group,
                                             fill = simd2012_inc_rank_dec),
               alpha = 0.8, color = "black", size = 0.1) +
  scale_fill_manual(name="Decile", values= clrs)+
  ggtitle("SIMD Income '12") +
  guides(fill = guide_legend(override.aes = list(colour = NULL))) +
  theme_map() +
  theme(legend.background = element_rect(colour = 'black'),
        plot.title = element_text(face = 'bold', size = 11),
        legend.position = c(0.7,0.28),
        legend.key.size = unit(2.5, "mm"),
        #   legend.text = element_text(size = 5),
        legend.title = element_text(face = 'bold'),
        legend.background = element_rect(size = 0.05, colour = 'black',
                                         linetype = 'solid'),
        plot.margin = unit(rep(0,4),"mm"),
        panel.margin = unit(rep(1,4),"mm"))

ggsave(filename = paste(LAmapsfolder,"income_simd_dec.png", sep = "/"),
       dpi = 300)
# Housing by Local Authority ----------------------------------------------

# Add local authority identifier to the simd housing data
dta_simd12_dz_wgeo <- merge(x = dta_simd12_dz,
                            y = dta_geo[ , c("Datazone2001", "Council")],
                            by.x = "Datazone", by.y = "Datazone2001",
                            all.x = TRUE, sort = FALSE)

# Melt it to prepare for graphs
dta_simd12_dz_wgeo_mlt <- melt(data =
                                 dta_simd12_dz_wgeo[ ,
                                                     c("Datazone", "Council",
                                                       "simd2012_house_rank_dec")],
                               id.vars = c("Datazone","Council"))

# To numeric
dta_simd12_dz_wgeo_mlt$value <- as.numeric(dta_simd12_dz_wgeo_mlt$value)

# Aggregate the data on a local authority level<- For tables
#dta_simd12_la_agg <- aggregate(x = dta_simd12_dz_wgeo_mlt["value"],
#                               by = list(dta_simd12_dz_wgeo_mlt$Council),
#                               FUN = sum)


# Create percentage summary table by group
# Do not run on slow computers
# dta_simd12_dz_wgeo_mlt_per <- ddply(
#   .data = dta_simd12_dz_wgeo_mlt, .(Council),
#   mutate,
#   perc_var = ((value / sum(value))*100))

# Create Dwellings Prevalence Map -----------------------------------------

# Source data zone level dwellings data
dta_dz_dwells <- read.csv(paste(fldr_dta,"Batch1_2013_ZNC0R0_30_6_2015.csv",
                                sep = "/"))
## Clean names
names(dta_dz_dwells)[names(dta_dz_dwells) == "X"] <- "datazone"
## Delete first row
dta_dz_dwells = dta_dz_dwells[-1,]
## Delete column with the total number of dwellings
dta_dz_dwells$HO.Dwellings <- NULL
## Melt the data frame for plotting
dta_dz_dwells_mlt <- melt(data = dta_dz_dwells, id.vars = "datazone")
## Remove unwanted letters
dta_dz_dwells_mlt$variable <- gsub(pattern = "HO.", replacement = "",
                                   x = dta_dz_dwells_mlt$variable)
## Calculate percentage summary by group
# Do not run as eats too much memory !!!!! won't run on 4GB ram or less
# dta_dz_dwells_mlt_per <- ddply(
#   .data = dta_dz_dwells_mlt, .(datazone),
#   mutate,
#   perc_var = ((value / sum(value))*100))

# Show head of this data table
head(dta_dz_dwells_mlt)

# Create percentage rows
dta_dz_dwells_mlt_per <- prop.table(tapply(dta_dz_dwells_mlt$value,
                                       dta_dz_dwells_mlt[1:2], sum), 1)
## Convert back to data frame
dta_dz_dwells_mlt_per <- data.frame(dta_dz_dwells_mlt_per)
head(dta_dz_dwells_mlt_per)
dta_dz_dwells_mlt_per <- dta_dz_dwells_mlt_per[-1,]
## Create majority variable
dta_dz_dwells_mlt_per$majority <- apply(
    dta_dz_dwells_mlt_per[,c("AttachUnknown","Detached","Flat",
                             "Semidetached", "Terraced")],1,which.max)
### Replace created number with a word
str_names <- names(dta_dz_dwells_mlt_per)
dta_dz_dwells_mlt_per$majority <- as.numeric(dta_dz_dwells_mlt_per$majority)
dta_dz_dwells_mlt_per$majority <- str_names[dta_dz_dwells_mlt_per$majority]
head(dta_dz_dwells_mlt_per)
## Make percentage figures

## Create aggregated data frame with maximum value
# dta_dz_dwells_mlt_per_agg <- ddply(.data = dta_dz_dwells_mlt_per,
#                                    .variables = .(datazone, variable),
#                                    subset,
#                                    value == max(value))

## Make data zone column
dta_dz_dwells_mlt_per$datazone <- rownames(dta_dz_dwells_mlt_per)

# Merge the data table with the map data
map_dta_dwells_perc <- merge(x = mstr_map_dta_cln,
                           y = dta_dz_dwells_mlt_per,
                           by.x = "id", by.y = "datazone", all.x = TRUE,
                           all.y = FALSE, sort = FALSE)

## Clean variable names after merging
names(map_dta_dwells_perc)[names(map_dta_dwells_perc)
                           == "majority.x"] <- "majority_band"
names(map_dta_dwells_perc)[names(map_dta_dwells_perc)
                           == "majority.y"] <- "majority_dwellings"

# Generate map for majority of dwellings per data zone
LOCALAUTHORITYMap +
  geom_polygon(data = map_dta_dwells_perc, aes(x = lng, y = lat,
                                             group = group,
                                             fill = majority_dwellings),
               alpha = 0.8, color = "black", size = 0.1) +
  scale_fill_discrete(name = "Majority of Dwellings") +
  ggtitle("Dwellings") +
  guides(fill = guide_legend(override.aes = list(colour = NULL))) +
  theme_map() +
  theme(legend.background = element_rect(colour = 'black'),
        plot.title = element_text(face = 'bold', size = 11),
        legend.position = c(0.7,0.28),
        legend.key.size = unit(2.5, "mm"),
        #   legend.text = element_text(size = 5),
        legend.title = element_text(face = 'bold'),
        legend.background = element_rect(size = 0.05, colour = 'black',
                                         linetype = 'solid'),
        plot.margin = unit(rep(0,4),"mm"),
        panel.margin = unit(rep(1,4),"mm"))
# Save map
ggsave(filename = paste(LAmapsfolder,"maj_dwells.png", sep = "/"),
       dpi = 300)