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
library(readxl)

#Is the original phenology data loaded into your environment?
head(phen) 

#################################LOAD DATA
#If not, load it in:
phen <- read.csv("/Users/elizabethlombardi/Desktop/Research/UNM/Erin phenology project/Master_Dataframe_colorsEML.csv", header=TRUE) #This is the most up to date as of Jan 1 2024

#Also load the earliest dataframes from the Excel workbook that Erin made:
E.mbs.spec <- read_excel("~/Desktop/Research/UNM/Erin phenology project/Earliest_taxa.xlsx", 
                         sheet = "Earliest MBS specimen ")
E.pp.spec <- read_excel("~/Desktop/Research/UNM/Erin phenology project/Earliest_taxa.xlsx", 
                           sheet = "Earliest PP specimen")
E.mbs.obs <- read_excel("~/Desktop/Research/UNM/Erin phenology project/Earliest_taxa.xlsx", 
                        sheet = "Earliest MBS observation")
E.pp.obs <- read_excel("~/Desktop/Research/UNM/Erin phenology project/Earliest_taxa.xlsx", 
                       sheet = "Earliest PP observation")

#We also need polygons that define the areas of interest (PP and MBS)
pp.aoi <- st_read("/Users/elizabethlombardi/Desktop/Research/UNM/Erin phenology project/Site Polygons/PikesPeak_Poly.shp") %>%
  st_transform(4326)

mbs.aoi <- st_read("/Users/elizabethlombardi/Desktop/Research/UNM/Erin phenology project/Site Polygons/MountEvans_Poly.shp") %>%
  st_transform(4326)
#################################



################################# CLEAN DATA IF YOU WANT/NEED TO
#convert the dataframe to an sf object
phen_sf <- phen %>%
  filter(!is.na(longitude)) %>%
  filter(!is.na(latitude)) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326)



#define color palettes
siteCols = c("#4477AA", "#EE6677") #MBS is blue, PP is red
dataCols = c("#AA3377", "#CCBB44")



################################# Load PRISM data that was downloaded from online site.
#these data frame are yearly and monthly averages across the three gridded rasters Erin focused on

#monthly precipitation since 1895
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



#
#
#Temperature since 1895
#
#
#
#monthly mean temperature since 1895
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



################################# JOINING ABIOTIC AND OCCURRENCE DATA
#Note that there are not annual data from PRISM for year 2023, but there are some monthly data for 2023 (months 1 through 9)

####Join precip and temperature into a dataframe for annual (prism.annual) and monthly(prism.month) data to occurrence data

#annual
prism.annual <- merge(comb.anntmean, comb.annppt, by = c("year", "site"))
prism.annual$site = toupper(prism.annual$site) #upper case to match the other datasheets with phenology


#monthly
prism.month<- merge(comb.tmean, comb.ppt, by = c("month", "year", "site"))
prism.month$site = toupper(prism.month$site)

# i. join to just the earlyPhen data
#Create full datasets of all earlyPhen occurrences with associated annual and monthly abiotic data
early.annual <-merge(earlyPhen, prism.annual, by=c("year", "site"))
#check to see what didn't merge
check<-anti_join(earlyPhen, early.annual) #these are the records from too recent or too long ago to have associated PRISM data. Could add 2023

#monthly
early.monthly <- merge(earlyPhen, prism.month, by=c("year", "site")) #this is the earlyPhen data and each of the earliest records has 12 rows of abiotic data from that year


# ii. join abiotic data to full phenology data
#annual 
full.annual<- merge(phen2, prism.annual, by=c("year", "site"))
full.annual <- merge(phen, abio.df, by=c("year", "month")) #this joins phen to the seasonal abiotic dataframe; out of order may change


check<-anti_join(phen2, full.annual) #all are records that fall outside of the 1895-2022 PRISM time frame. 559 of the 592 are from 2023. 

#monthly
full.monthly <- merge(phen2, prism.month, by=c("year", "site")) 


#iii. Earliest phenology by species, site and data type
#Create full datasets of all E.site.datatype sheets occurrences with associated annual and monthly abiotic data
E.mbs.obs <-merge(E.mbs.obs, prism.annual, by=c("year", "site"))
E.mbs.spec <-merge(E.mbs.spec, prism.annual, by=c("year", "site"))

E.pp.obs <-merge(E.pp.obs, prism.annual, by=c("year", "site"))
E.pp.spec <-merge(E.pp.spec, prism.annual, by=c("year", "site"))


#check to see what didn't merge
check<-anti_join(E.mbs.obs, prism.annual, by=c("year", "site")) #these are the records from too recent or too long ago to have associated PRISM data. Could add 2023



#PLOT

# impacts on flowering over annual time

#i. plot full dataset
ggplot(full.annual, aes(x = ppt.mean, y = ordinal_date, color=season)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  #scale_color_manual(values=dataCols) +
  labs(title = "Correlation between observed ALL phenology dates and mean annual ppt",
       x = "Annual Precipitation (inches)",
       y = "All Ordinal Date") +
  theme_minimal() +
  facet_wrap(~ season, scales = "free_x")

ggplot(full.annual, aes(x = mean.tmean, y = ordinal_date, color=data_type)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values=dataCols) +
  labs(title = "Correlation between observed ALL phenology dates and mean temperature",
       x = "Annual temperature (F)",
       y = "All Ordinal Date") +
  theme_minimal() +
  facet_wrap(~ data_type, scales = "free_x")

#ii. plot earliest flowering dataset

#Part A: original plots that don't totally make sense (it's all earliest phenology data without separating by species)
ggplot(early.annual, aes(x = mean.ppt, y = ordinal_date)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color="darkslategrey") +
  scale_color_manual(values=dataCols) +
  labs(title = "Correlation between observed Earliest phenology dates and mean annual ppt",
       x = "Annual Precipitation (inches)",
       y = "Earliest Ordinal Date") +
  theme_minimal() 

ggplot(early.annual, aes(x = mean.tmean, y = ordinal_date)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color="darkslategrey") +
  scale_color_manual(values=dataCols) +
  labs(title = "Correlation between observed Earliest phenology dates and mean annual temperature",
       x = "Annual Temperature (f)",
       y = "Earliest Ordinal Date") +
  theme_minimal()

#annual ppt for earliest flowering data
ggplot(early.annual, aes(x = mean.ppt, y = ordinal_date, color=data_type)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values=dataCols) +
  labs(title = "Correlation between observed ALL phenology dates and mean annual ppt",
       x = "Annual Precipitation (inches)",
       y = "Earliest Ordinal Date") +
  theme_minimal() +
  facet_wrap(~ data_type, scales = "free_x")

#annual tmean
ggplot(early.annual, aes(x = mean.tmean, y = ordinal_date, color=data_type)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values=dataCols) +
  labs(title = "Correlation between observed ALL phenology dates and mean annual temperature",
       x = "Annual Temperature (F)",
       y = "Earliest Ordinal Date") +
  theme_minimal() +
  facet_wrap(~ data_type, scales = "free_x")


##################PLOT Earliest data separated by species, site and data type

####PRECIPITATION
#earliest by species at MBS (observational data)
eMBSobs <-ggplot(E.mbs.obs, aes(x = mean.ppt, y = ordinal_date, color=data_type)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values=dataCols[1]) +
  labs(x = "Annual Precipitation (inches)",
       y = "Earliest Ordinal Date") +
  theme_minimal() +
  theme(panel.background = element_blank(),
          legend.position = "none")

#earliest by species at PP (observational data)
ePPobs<-ggplot(E.pp.obs, aes(x = mean.ppt, y = ordinal_date, color=data_type)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values=dataCols[1]) +
  labs(x = "Annual Precipitation (inches)",
       y = "Earliest Ordinal Date") +
  theme_minimal() +
  theme(panel.background = element_blank(),
       legend.position = "none")

#earliest by species at MBS (herbarium data)
eMBSspec <-ggplot(E.mbs.spec, aes(x = mean.ppt, y = ordinal_date, color=data_type)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values=dataCols[2]) +
  labs(x = "Annual Precipitation (inches)",
       y = "Earliest Ordinal Date") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        legend.position = "none")

#earliest by species at PP (herbarium data)
ePPspec<-ggplot(E.pp.spec, aes(x = mean.ppt, y = ordinal_date, color=data_type)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values=dataCols[2]) +
  labs(x = "Annual Precipitation (inches)",
       y = "Earliest Ordinal Date") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        legend.position = "none")


####TEMPERATURE
#earliest by species at MBS (observational data)
eMBSobs2 <-ggplot(E.mbs.obs, aes(x = mean.tmean, y = ordinal_date, color=data_type)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values=dataCols[1]) +
  labs(x = "Annual Temperature (degrees F)",
       y = "Earliest Ordinal Date") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        legend.position = "none")

#earliest by species at PP (observational data)
ePPobs2 <-ggplot(E.pp.obs, aes(x = mean.tmean, y = ordinal_date, color=data_type)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values=dataCols[1]) +
  labs(x = "Annual Temperature (degrees F)",
       y = "Earliest Ordinal Date") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        legend.position = "none")

#earliest by species at MBS (herbarium data)
eMBSspec2 <-ggplot(E.mbs.spec, aes(x = mean.tmean, y = ordinal_date, color=data_type)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values=dataCols[2]) +
  labs(x = "Annual Temperature (degrees F)",
       y = "Earliest Ordinal Date") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        legend.position = "none")

#earliest by species at PP (herbarium data)
ePPspec2 <-ggplot(E.pp.spec, aes(x = mean.tmean, y = ordinal_date, color=data_type)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values=dataCols[2]) +
  labs(x = "Annual Temperature (degrees F)",
       y = "Earliest Ordinal Date") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        legend.position = "none")



###########################SAVE THE PLOTS AS PNG FILES

#PRECIP PLOTS
ggsave("/Users/elizabethlombardi/Desktop/Research/UNM/Erin phenology project/Figures/eMBSobs.png", eMBSobs, width = 8, height = 6, units="in", dpi = 300)
ggsave("/Users/elizabethlombardi/Desktop/Research/UNM/Erin phenology project/Figures/ePPobs.png", ePPobs, width = 8, height = 6, units="in", dpi = 300)
ggsave("/Users/elizabethlombardi/Desktop/Research/UNM/Erin phenology project/Figures/eMBSspec.png", eMBSspec, width = 8, height = 6, units="in", dpi = 300)
ggsave("/Users/elizabethlombardi/Desktop/Research/UNM/Erin phenology project/Figures/ePPspec.png", ePPspec, width = 8, height = 6, units="in", dpi = 300)

#TEMPERATURE PLOTS
ggsave("/Users/elizabethlombardi/Desktop/Research/UNM/Erin phenology project/Figures/eMBSobs2.png", eMBSobs2, width = 8, height = 6, units="in", dpi = 300)
ggsave("/Users/elizabethlombardi/Desktop/Research/UNM/Erin phenology project/Figures/ePPobs2.png", ePPobs2, width = 8, height = 6, units="in", dpi = 300)
ggsave("/Users/elizabethlombardi/Desktop/Research/UNM/Erin phenology project/Figures/eMBSspec2.png", eMBSspec2, width = 8, height = 6, units="in", dpi = 300)
ggsave("/Users/elizabethlombardi/Desktop/Research/UNM/Erin phenology project/Figures/ePPspec2.png", ePPspec2, width = 8, height = 6, units="in", dpi = 300)




# Run linear regression and print summaries

#i. Earliest data only
##precipitation
pptlm<-lm(ordinal_date ~ mean.ppt, data = early.annual)
pptsumm1 <- tidy(pptlm)
print(pptlm)
print(pptsumm1)

pptlm.obs <- lm(ordinal_date ~ mean.ppt, data = early.annual, subset = (data_type == "observation"))
pptlm.herb <- lm(ordinal_date ~ mean.ppt, data = early.annual, subset = (data_type == "specimen"))

summary(pptlm.obs)
summary(pptlm.herb)


##temperature
tmeanlm<-lm(ordinal_date ~ mean.tmean, data = early.annual)
tmeansumm1 <- tidy(tmeanlm)
print(tmeanlm)
print(tmeansumm1)

tmeanlm.obs <- lm(ordinal_date ~ mean.tmean, data = early.annual, subset = (data_type == "observation"))
tmeanlm.herb <- lm(ordinal_date ~ mean.tmean, data = early.annual, subset = (data_type == "specimen"))

summary(tmeanlm.obs)
summary(tmeanlm.herb)



#i. FULL data
##precipitation
pptlm2<-lm(ordinal_date ~ mean.ppt, data = full.annual)
pptsumm2 <- tidy(pptlm2)
print(pptlm2)
print(pptsumm2)

pptlm2.obs <- lm(ordinal_date ~ mean.ppt, data = full.annual, subset = (data_type == "observation"))
pptlm2.herb <- lm(ordinal_date ~ mean.ppt, data = full.annual, subset = (data_type == "specimen"))

summary(pptlm2.obs)
summary(pptlm2.herb)


##temperature
tmeanlm2<-lm(ordinal_date ~ mean.tmean, data = full.annual)
tmeansumm2 <- tidy(tmeanlm2)
print(tmeanlm2)
print(tmeansumm2)

tmeanlm2.obs <- lm(ordinal_date ~ mean.tmean, data = full.annual, subset = (data_type == "observation"))
tmeanlm2.herb <- lm(ordinal_date ~ mean.tmean, data = full.annual, subset = (data_type == "specimen"))

summary(tmeanlm2.obs)
summary(tmeanlm2.herb)







#Are there correlations between collections and time generally; this probably belongs in the Exploratory Data Analysis script instead of here!

#1. herbarium
ggplot(earlyPhen.herb, aes(x = year, y = ordinal_date)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color="darkslategrey") +
  #scale_color_manual(values=siteCols) +
  labs(title = "Earliest herbarium dates over time",
       x = "Year",
       y = "Earliest Ordinal Date") +
  theme_minimal() 

ggplot(earlyPhen.herb, aes(x = year, y = ordinal_date, color=site)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values=siteCols) +
  labs(title = "Earliest herbarium dates over time",
       x = "Year",
       y = "Earliest Ordinal Date") +
  theme_minimal() +
  facet_wrap(~ site, scales = "free_x")

#check for significance

herb.mbslm2 <- lm(ordinal_date ~ year, data = earlyPhen.herb, subset = (site == "MBS"))
herb.pplm2 <- lm(ordinal_date ~ year, data = earlyPhen.herb, subset = (site == "PP"))

summary(herb.mbslm2)
summary(herb.pplm2)


###ALL herbarium data over time
ggplot(specimen, aes(x = year, y = ordinal_date, color=site)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values=siteCols) +
  labs(title = "All herbarium dates over time",
       x = "Year",
       y = "Ordinal Date") +
  theme_minimal() +
  facet_wrap(~ site, scales = "free_x")

#check for significance

herb.mbslm <- lm(ordinal_date ~ year, data = specimen, subset = (site == "MBS"))
herb.pplm <- lm(ordinal_date ~ year, data = specimen, subset = (site == "PP"))

summary(herb.mbslm)
summary(herb.pplm)




#2. observation
ggplot(earlyPhen.obs, aes(x = year, y = ordinal_date)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color="darkslategrey") +
  #scale_color_manual(values=siteCols) +
  labs(title = "Earliest iNat dates over time",
       x = "Year",
       y = "Earliest Ordinal Date") +
  theme_minimal() 

ggplot(earlyPhen.obs, aes(x = year, y = ordinal_date, color=site)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values=siteCols) +
  labs(title = "Earliest iNat dates over time",
       x = "Year",
       y = "Earliest Ordinal Date") +
  theme_minimal() +
  facet_wrap(~ site, scales = "free_x")

obs.mbslm2 <- lm(ordinal_date ~ year, data = earlyPhen.obs, subset = (site == "MBS"))
obs.pplm2 <- lm(ordinal_date ~ year, data = earlyPhen.obs, subset = (site == "PP"))

summary(obs.mbslm2)
summary(obs.pplm2)

###All specimen data
ggplot(inat, aes(x = year, y = ordinal_date, color=site)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values=siteCols) +
  labs(title = "All iNat dates over time",
       x = "Year",
       y = "All Ordinal Dates") +
  theme_minimal() +
  facet_wrap(~ site, scales = "free_x")

obs.mbslm <- lm(ordinal_date ~ year, data = inat, subset = (site == "MBS"))
obs.pplm <- lm(ordinal_date ~ year, data = inat, subset = (site == "PP"))

summary(obs.mbslm)
summary(obs.pplm)


#stats
lm.herb <- lm(ordinal_date ~ year, data = earlyPhen, subset = (data_type == "specimen"))
lm.obs <- lm(ordinal_date ~ year, data = earlyPhen, subset = (data_type == "observation"))

summary(lm.herb)
summary(lm.obs)





