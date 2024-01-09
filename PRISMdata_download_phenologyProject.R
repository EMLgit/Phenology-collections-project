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
  filter(!is.na(longitude)) %>%
  filter(!is.na(latitude)) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326)


#We also need polygons that define the areas of interest (PP and MBS)
pp.aoi <- st_read("/Users/elizabethlombardi/Desktop/Research/UNM/Erin phenology project/Site Polygons/PikesPeak_Poly.shp") %>%
  st_transform(4326)

mbs.aoi <- st_read("/Users/elizabethlombardi/Desktop/Research/UNM/Erin phenology project/Site Polygons/MountEvans_Poly.shp") %>%
  st_transform(4326)



####Load PRISM data that was downloaded from online site.
#these data frame are yearly and monthly averages across the three gridded rasters Erin focused on


#montly precipitation since 1895

mbs.ppt <- read.csv("/Users/elizabethlombardi/Desktop/Research/UNM/Erin phenology project/MBS_ppt_monthlyMeans.csv", header = TRUE)
pp.ppt <- read.csv("/Users/elizabethlombardi/Desktop/Research/UNM/Erin phenology project/PP_ppt_monthlyMeans.csv", header = TRUE)

comb.ppt <- rbind(mbs.ppt, pp.ppt) #this is the combined precipitation date for the study areas

ggplot(comb.ppt, aes(x = month, y = ppt.mean)) +
  geom_point(alpha=0.3, color="darkslategrey") +
  geom_smooth(method="gam", color="darkslategrey") + #general additive model to fit the data...seems fine for now?
  labs(title = "Annual precipitation trends",
       x = "Month",
       y = "Precipitation (inches)") +
  theme_minimal() +
  facet_wrap(~ site, scales = "free_x")



##Annual averages (provided by Erin)
#annual precipitation since 1895
mbs.annppt <- read.csv("/Users/elizabethlombardi/Desktop/Research/UNM/Erin phenology project/MBS_ppt_annual.csv", header = TRUE)
pp.annppt <- read.csv("/Users/elizabethlombardi/Desktop/Research/UNM/Erin phenology project/PP_ppt_annual.csv", header = TRUE)

comb.annppt <- rbind(mbs.annppt, pp.annppt)

#plot of annual data from Erin for each site over time
ggplot(comb.annppt, aes(x = year, y = mean.ppt)) +
  geom_point(alpha=0.3, color="darkslategrey") +
  geom_smooth(method="loess", color="darkslategrey") + 
  labs(title = "Annual precipitation trends",
       x = "Year",
       y = "Precipitation (inches)") +
  theme_minimal() +
  facet_wrap(~ site, scales = "free_x")


#break it down by raster grid
ggplot(comb.annppt, aes(x = year, y = summit.ppt)) +
  geom_point(alpha=0.3, color="darkslategrey") +
  geom_smooth(method="loess", color="darkslategrey") + 
  geom_point(aes(x=year, y= mid.ppt), alpha=0.5, color="darkblue") +
  geom_smooth(aes(x = year, y = mid.ppt), method = "loess", color = "darkblue")+
  geom_point(aes(x=year, y= sub.ppt), alpha=0.5, color="darkgreen") +
  geom_smooth(aes(x = year, y = sub.ppt), method = "loess", color = "darkgreen")+
  labs(title = "Annual precipitation trends",
       x = "Year",
       y = "Precipitation (inches)") +
  theme_minimal() +
  facet_wrap(~ site, scales = "free_x")



####Put abiotic and biotic data together
####Join the prism data to the flowering data from Exploratory Data Analysis script.
#remember that earlyPhen has all the data for records made the earliest day (ordinal_date) of each year. There are multiple per year sometimes
earlyPhen <- read.csv("/Users/elizabethlombardi/Desktop/Research/UNM/Erin phenology project/earlyPhen.csv", header = TRUE) 

#join
try <- earlyPhen %>% # I don't think this is working correctly
  inner_join(comb.annppt, by="year")

ggplot(fullDF, aes(x = mean.ppt, y = ordinal_date)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color="darkslategrey") +
  labs(title = "Correlation between observed earliest dates and minimum annual ppt",
       x = "Annual Precipitation (inches)",
       y = "Earliest Ordinal Date") +
  theme_minimal()


# Run linear regression and print summary
pptlm<-lm(ordinal_date ~ mean.ppt, data = fullDF)
pptsumm1 <- tidy(pptlm)

print(pptlm)
print(pptsumm1)





#Temperature since 1895

#montly mean temperature since 1895
mbs.tmean <- read.csv("/Users/elizabethlombardi/Desktop/Research/UNM/Erin phenology project/MBS_tmean_monthly.csv", header = TRUE)
pp.tmean <- read.csv("/Users/elizabethlombardi/Desktop/Research/UNM/Erin phenology project/PP_tmean_monthly.csv", header = TRUE)

comb.tmean <- rbind(mbs.tmean, pp.tmean) #this is the combined precipitation data for monthly analyses for the study areas

ggplot(comb.tmean, aes(x = month, y = tmean.avg)) +
  geom_point(alpha=0.3, color="goldenrod4") +
  geom_smooth(method="gam", color="goldenrod4") + #general additive model to fit the data...seems fine for now?
  labs(title = "Annual mean temperature trends",
       x = "Month",
       y = "Temperature") +
  theme_minimal() +
  facet_wrap(~ site, scales = "free_x")


#annual temperature since 1895
mbs.anntmean <- read.csv("/Users/elizabethlombardi/Desktop/Research/UNM/Erin phenology project/MBS_tmean_annual.csv", header = TRUE)
pp.anntmean <- read.csv("/Users/elizabethlombardi/Desktop/Research/UNM/Erin phenology project/PP_tmean_annual.csv", header = TRUE)

comb.anntmean <- rbind(mbs.anntmean, pp.anntmean)

#plot of annual data from Erin for each site over time
ggplot(comb.anntmean, aes(x = year, y = mean.tmean)) +
  geom_point(alpha=0.3, color="goldenrod4") +
  geom_smooth(method="loess", color="goldenrod4") + 
  labs(title = "Annual mean temperature trends",
       x = "Year",
       y = "Temperature (F") +
  theme_minimal() +
  facet_wrap(~ site, scales = "free_x")


#break it down by raster grid
ggplot(comb.anntmean, aes(x = year, y = summit.tmean)) +
  geom_point(alpha=0.3, color="goldenrod4") +
  geom_smooth(method="loess", color="goldenrod4") + 
  geom_point(aes(x=year, y= mid.tmean), alpha=0.5, color="lightblue") +
  geom_smooth(aes(x = year, y = mid.tmean), method = "loess", color = "lightblue")+
  geom_point(aes(x=year, y= sub.tmean), alpha=0.5, color="lightgreen") +
  geom_smooth(aes(x = year, y = sub.tmean), method = "loess", color = "lightgreen")+
  labs(title = "Annual temperature trends",
       x = "Year",
       y = "Temperature (F)") +
  theme_minimal() +
  facet_wrap(~ site, scales = "free_x")



####Join precip and temperature into a dataframe for annual (prismANN) and monthly(prismMNTH) data
prismANN <- comb.annppt %>%
  cbind(comb.anntmean) %>%
  dplyr::select(-"site", -"year")
prismANN$site = toupper(prismANN$site) #upper case to match the other datasheets with phenology


prismMNTH<- comb.ppt %>%
  cbind(comb.tmean) %>%
  dplyr::select(-"month", -"site", -"year")

#join earliest flowering date for each site/year combination
flwsANN <- earlyPhen %>%
  distinct(year, site, ordinal_date) %>%
  inner_join( prismANN, 
            by=c('site','year'))



####Put abiotic and biotic data together
####Join the prism data to the flowering data from Exploratory Data Analysis script.
#remember that earlyPhen has all the data for records made the earliest day (ordinal_date) of each year. There are multiple per year sometimes


#Precipitation impacts on earliest flowering at each site over annual time
ggplot(flwsANN, aes(x = mean.ppt, y = ordinal_date)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color="darkslategrey") +
  labs(title = "Correlation between observed earliest dates and minimum annual ppt",
       x = "Annual Precipitation (inches)",
       y = "Earliest Ordinal Date") +
  theme_minimal()


# Run linear regression and print summary
pptlm<-lm(ordinal_date ~ mean.ppt, data = flwsANN)
pptsumm1 <- tidy(pptlm)

print(pptlm)
print(pptsumm1)


#Temperature impacts on earliest flowering at each site over annual time
ggplot(flwsANN, aes(x = mean.tmean, y = ordinal_date)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color="goldenrod4") +
  labs(title = "Correlation between observed earliest dates and minimum annual ppt",
       x = "Annual Temperature (F)",
       y = "Earliest Ordinal Date") +
  theme_minimal()


# Run linear regression and print summary
tmeanlm<-lm(ordinal_date ~ mean.tmean, data = flwsANN)
tmeansumm1 <- tidy(tmeanlm)

print(tmeanlm)
print(tmeansumm1)









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
