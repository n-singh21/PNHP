#---------------------------------------------------------------------------------------------
# Name:     bonap_colorgetter.R
# Purpose:  uses RGB values from bonap species range map images to determine species presence
#           in all US counties. Calculates Pennsylvania responsibility values for all species
#           based on the sum of species presence area within pennsylvania versus total species
#           presence area.
# Author:   Molly Moore
# Created:  2018-06-12
# Updates:  Updated to include Bonap image downloader
#
# To Do List/Future Ideas:
#           - copy rasters into geodatabase in R - currently has to be done in Arc
#           - develop method/math for determining edge of range species
#---------------------------------------------------------------------------------------------


# install and load necessary packages
if (!requireNamespace("arcgisbinding", quietly=TRUE)) install.packages("arcgisbinding")
require(arcgisbinding)
if (!requireNamespace("raster", quietly=TRUE)) install.packages("raster")
require(raster)
if (!requireNamespace("sf", quietly=TRUE)) install.packages("sf")
require(sf)
if (!requireNamespace("dplyr", quietly=TRUE)) install.packages("dplyr")
require(dplyr)
if (!requireNamespace("RCurl", quietly=TRUE)) install.packages("RCurl")
require(RCurl)
if (!requireNamespace("data.table", quietly=TRUE)) install.packages("data.table")
require(data.table)
if (!requireNamespace("rgdal", quietly=TRUE)) install.packages("rgdal")
require(rgdal)
if (!requireNamespace("raster", quietly=TRUE)) install.packages("raster")
require(raster)


# set path to bonap image folder, image geodatabase, county centroids.
image_folder <- "C:/Users/nsingh/Documents/PNHP/BONAP files/bonap_originals"
#image_gdb <- 'W:/Heritage/Heritage_Projects/1495_PlantConservationPlan/bonap/bonap_rasters.gdb'
county_centroids <- "C:/Users/nsingh/Documents/ArcGIS/Projects/iNaturalistData/bonap.gdb/county_centroids"
url_base <- 'http://bonap.net/MapGallery/County/'
projection_file <- "C:/Users/nsingh/Documents/PNHP/BONAP files/bonap_projections"
snames = c("Asplenium bradleyi","Asplenium pinnatifidum",
           "Baptisia australis var. australis","Cryptogramma stelleri",
           "Equisetum variegatum","Platanthera dilatata var. dilatata",
           "Dodecatheon meadia","Woodwardia areolata")

#####################################################################################################################
## Downloading files ################################################################################################
#####################################################################################################################

not_downloaded <- data.frame(species=character())
for(s in snames){
  u <- paste(paste0(url_base, s), "png", sep=".")
  u <- gsub(' ','%20',u)
  if(url.exists(u)==TRUE){
    download.file(u, paste0(paste(image_folder,s, sep='/'),".tif"), mode='wb')
  }
  else{
    print(paste0(s, " was not available for download"))
    nd <- data.frame(species=s)
    not_downloaded <- rbind(not_downloaded, nd)
  }
}

#not_down <- 'W:/Heritage/Heritage_Projects/1495_PlantConservationPlan/bonap/maps_not_downloaded.csv'
#write.csv(not_downloaded, not_down)
not_downloaded

# create list of species from file names. sub in underscores and exclude extension to match file name in GDB.
bonap_maps <- list.files(path = image_folder, pattern='\\.tif$')
bonap_maps <- gsub('\\.tif$', '', bonap_maps)
for(s in bonap_maps){
  file.copy(paste0(projection_file,".tfwx"), paste0(image_folder,"/",s,".tfwx"), overwrite=TRUE)
  file.copy(paste0(projection_file,".tif.aux.xml"), paste0(image_folder,"/",s,".tif.aux.xml"), overwrite=TRUE)
  file.copy(paste0(projection_file,".tif.xml"), paste0(image_folder,"/",s,".tif.xml"), overwrite=TRUE)
}

#####################################################################################################################
## Color Getting ####################################################################################################
#####################################################################################################################

# print start time for color loop
print(paste0("color gettin': ", Sys.time())) 

# open r-arc connection, load county centroid layer and change to sp
arc.check_product()
counties <- arc.open(county_centroids)
county_tbl <- arc.select(counties)
counties_shape <- arc.data2sp(county_tbl)

# start loop
for(bonap in bonap_maps){
  # load in species raster from GDB
  # map <- arc.open(paste(image_gdb,bonap,sep="/"))
  # map <- arc.raster(map)
  # map <- as.raster(map)
  
  map <- readGDAL(paste0(image_folder,"/",bonap,".tif"))
  proj4string(map) <- CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs")
  extent_mat <- t(matrix(c(-2375340, 3008685, -2562062, 742518),nrow=2))
  map@bbox = extent_mat
  gridded(map) <- TRUE
  map1 = raster(map,layer=1)
  map2 = raster(map,layer=2)
  map3 = raster(map,layer=3)
  map4 = raster(map,layer=4)
  
  # extract RGB values to county centroid points
  Band_1 <- extract(map1, counties_shape)
  Band_2 <- extract(map2, counties_shape)
  Band_3 <- extract(map3, counties_shape)
  Band_4 <- extract(map4, counties_shape)
  merge <- cbind(county_tbl,Band_1,Band_2,Band_3,Band_4)
  
 
  
  # create column and fill with species presence value based on RGB values
  merge$bonap <- ifelse(merge$Band_1 == 173 & merge$Band_2 == 142 & merge$Band_3 == 0, 'SNP',
                        ifelse(merge$Band_1 == 0 & merge$Band_2 == 128 & merge$Band_3 == 0, 'SP',
                               ifelse(merge$Band_1 == 0 & merge$Band_2 == 255 & merge$Band_3 == 0, 'PNR',
                                      ifelse(merge$Band_1 == 255 & merge$Band_2 == 255 & merge$Band_3 == 0, 'PR',
                                             ifelse(merge$Band_1 == 0 & merge$Band_2 == 0 & merge$Band_3 == 235, 'SPE',
                                                    ifelse(merge$Band_1 == 0 & merge$Band_2 == 255 & merge$Band_3 == 255, 'PE',
                                                           ifelse(merge$Band_1 == 255 & merge$Band_2 == 0 & merge$Band_3 == 255, 'NOX',
                                                                  ifelse(merge$Band_1 == 66 & merge$Band_2 == 66 & merge$Band_3 == 66, 'RAD',
                                                                         ifelse(merge$Band_1 == 0 & merge$Band_2 == 221 & merge$Band_3 == 145, 'SNA',
                                                                                ifelse(merge$Band_1 == 255 & merge$Band_2 == 165 & merge$Band_3 == 0, 'EXT', 'color not found'
                                                                                ))))))))))
  # change column name and join to county table
  colnames(merge)[colnames(merge)=='bonap'] <- bonap
  county_tbl <- cbind(county_tbl, merge[ncol(merge)])
}

# print end time for color getter
print(paste0("color gettin' finished: ", Sys.time())) 


if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
require(ggplot2)
if (!requireNamespace("ggmap", quietly = TRUE)) install.packages("ggmap")
require(ggmap)
if (!requireNamespace("arcgisbinding", quietly = TRUE)) install.packages("arcgisbinding")
require(arcgisbinding)
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
require(sf)
if (!requireNamespace("dplyr", quietly=TRUE)) install.packages("dplyr")
require(dplyr)

#set paths
counties <- "C:/Users/nsingh/Documents/ArcGIS/Projects/iNaturalistData/bonap.gdb/counties_us"
states <- "C:/Users/nsingh/Documents/ArcGIS/Projects/iNaturalistData/bonap.gdb/states_us_clip"
output_folder <- "C:/Users/nsingh/Documents/PNHP/BONAP files/Maps/"

#open states with arcbridge
arc.check_product()
state_open <- arc.open(states)
state_select <- arc.select(state_open)
state_lyr <- arc.data2sf(state_select)

#open counties with arcbridge
county_open <- arc.open(counties)
county_select <- arc.select(county_open)
county_lyr <- arc.data2sf(county_select)
county_lyr$COUNTYNS <- as.integer(county_lyr$COUNTYNS)

bonap <- county_tbl
bonap$COUNTYNS = as.integer(bonap$COUNTYNS)

county_bonap <- merge(county_lyr, bonap, by='COUNTYNS')

bonap_maps <- list.files(path = image_folder, pattern='\\.tif$')
bonap_maps <- gsub('\\.tif$', '', bonap_maps)
bonap_maps <- gsub(' ', '_', bonap_maps)
bonap_maps <- gsub('-', '_', bonap_maps)
bonap_maps <- gsub('var.', 'var', bonap_maps, fixed=TRUE)
bonap_maps <- gsub('ssp.', 'ssp', bonap_maps, fixed=TRUE)
names(county_bonap)[17:23] = bonap_maps

cols <- c("Not present in state"="yellow4",
          "Present in state"="green4",
          "Present, not rare"="green1",
          "Present, rare"="yellow",
          "Present in state, exotic"="blue",
          "Exotic and present"="cyan2",
          "Noxious"="magenta",
          "Eradicated"="grey15",
          "Native, adventive"="aquamarine",
          "Extirpated (historic)"="goldenrod1",
          "Unknown"="grey70")

# start loop for all species columns
for(b in bonap_maps){
  # select species column and only include if species is present not rare (PNR) or present rare (PR)
  column <- county_bonap %>% dplyr::select(b)
  column[[b]] <- gsub("SPE","Present in state, exotic", column[[b]])
  column[[b]] <- gsub("SNP","Not present in state", column[[b]])
  column[[b]] <- gsub("SP","Present in state", column[[b]])
  column[[b]] <- gsub("PNR","Present, not rare", column[[b]])
  column[[b]] <- gsub("PR","Present, rare", column[[b]])
  column[[b]] <- gsub("PE","Exotic and present", column[[b]])
  column[[b]] <- gsub("NOX","Noxious", column[[b]])
  column[[b]] <- gsub("RAD","Eradicated", column[[b]])
  column[[b]] <- gsub("SNA","Native, adventive", column[[b]])
  column[[b]] <- gsub("EXT","Extirpated (historic)", column[[b]])
  column[[b]] <- gsub("color not found", "Unknown", column[[b]])
  
  ggplot() +
    geom_sf(data=column, mapping=aes(fill=column[[b]]),lwd=0.25,color="grey40") +
    geom_sf(data=state_lyr, fill=NA, lwd=0.3, color="grey30") +
    scale_colour_manual(values=cols, aesthetics = c("fill")) +
    labs(title=gsub('_',' ',b),
         subtitle="Range within Contigious United States")+
    theme(axis.line = element_blank(),
          panel.grid.major = element_line(colour='transparent'),
          panel.grid.minor = element_line(colour='transparent'),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank(),
          legend.title=element_blank(),
          legend.box.margin=margin(0,0,-25,-50),
          legend.text=element_text(size=9),
          legend.key.size=unit(.8,"line"),
          plot.title=element_text(size=12,hjust=0.5,face=c("bold.italic")),
          plot.subtitle=element_text(size=8,hjust=0.5))+
    guides(fill = guide_legend(reverse=TRUE))
  
  ggsave(paste0(output_folder,b,".png"), width=7.5, height=4,dpi=500)
}


