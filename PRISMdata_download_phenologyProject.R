##Created by EML on January 1 2024
##For Erin's phenology project
##Script goal is to download PRISM data for Mount Blue Sky and Pikes Peak

#load library
library(dplyr)
library(ggplot2)
library(sf)
library(terra)
library(raster)
library(prism)

#Is the original phenology data loaded into your environment?
head(phen) 

#If not, load it in:
phen <- read.csv("/Users/elizabethlombardi/Desktop/Research/UNM/Erin phenology project/Master_Dataframe_colorsEML.csv", header=TRUE) #This is the most up to date as of Jan 1 2024

#convert the dataframe to an sf object
phen_sf <- phen %>%
  filter(!latitude=="") %>%
  filter(!longitude =="") %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326)


#We also need polygons that define the areas of interest (PP and MBS)
pp.aoi <- st_read("/Users/elizabethlombardi/Desktop/Research/UNM/Erin phenology project/Site Polygons/PikesPeak_Poly.shp") %>%
  st_transform(4326)

mbs.aoi <- st_read("/Users/elizabethlombardi/Desktop/Research/UNM/Erin phenology project/Site Polygons/MountEvans_Poly.shp") %>%
  st_transform(4326)




###To use the PRISM API and download via R, you need to define the directories the first time you use it:
#Specify where raster layers will go
options(prism.path="/Users/elizabethlombardi/Desktop/PRISM/Erin_PhenologyProject")
#Set folder for where PRISM layer downloads go
prism_set_dl_dir("/Users/elizabethlombardi/Desktop/PRISM/Erin_PhenologyProject") 

#After that first time, you can check to see what data you have in your folders already:
#check out the list of rasters I now have
prism_archive_ls()
prism_get_dl_dir()


#IF you don't have any data in your archive folder already 
# Download the 30-year annual average precip and annual average temperature
get_prism_monthlys(type = "ppt", years = 2019:2020, mon = 1:12, keepZip = TRUE)

get_prism_normals("ppt", "4km", annual = TRUE, keepZip = FALSE)
get_prism_normals("tmean", "4km", annual = TRUE, keepZip = FALSE)

###
get_prism_normals(
  type = "tmean", 
  resolution = "4km", 
  mon = 1:12, 
  annual = TRUE,
  keepZip = FALSE
)

# Grab the prism data and compile the files into a raster stack (precip and temp) for pikes peak (PP) and mount blue sky (MBS)
climate_data <- prism_archive_ls() %>%  
  pd_stack(.)  

climate_data_pp<-crop(climate_data, pp.aoi) #clip to just Pikes Peak
climate_data_mbs <- crop(climate_data, MBS) #clip to just Mount Blue Sky








# Extract project coordinates from raster stack
climate_crs <- climate_data@crs@projargs

# Convert collection locations to format and projection that matches prism data
# make spatial
env.v <- env.v %>% 
  mutate_at(c("decimalLongitude", "decimalLatitude", "id"), as.numeric) %>%
  st_as_sf(coords=c("decimalLongitude","decimalLatitude"), crs=CRS(climate_crs), remove=FALSE) %>%
  as.data.frame(.) 

coordinates(env.v) <- c('decimalLongitude', 'decimalLatitude')
proj4string(env.v) <- CRS(climate_crs)


# Extract the data from the raster stack for those sites 
prism.df <- data.frame(coordinates(env.v), 
                       env.v$id,
                       extract(climate_data, env.v))



#rename columns with annual mean precip and temp values for each row
prism.df <- prism.df %>% #includes provisional 2022 data
  #  mutate_at(c("env.coords.id"), as.numeric) %>%
  rename("precip30yr" = "PRISM_ppt_30yr_normal_4kmM4_annual_bil") %>% 
  rename("temp30yr" = "PRISM_tmean_30yr_normal_4kmM4_annual_bil") %>%
  rename("prismLong" = "decimalLongitude") %>%
  rename("prismLat" = "decimalLatitude")


#convert env.v back to dataframe
env.v <- as.data.frame(env.v)

#join prism.means with env.v using id columns DUPLICATES!! 3972 duplicated records because of many-to-many match here. 
env.v <- left_join(env.v, prism.df, by=(c("id"="env.v.id"))) #this joins PRISM 30-year normals to env.v dataframe, and also two new lat/long columns. Useful to make sure the abiotic data line up with the right collections.
