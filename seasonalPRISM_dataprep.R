##PRISM data manipulation

#In this script, I'm going to load the PRISM data from Erin, merge different abiotic variables by year and site, then define seasonal weather objects to be saved in a datastack

#Libraries
library(ggpubr)

#Load
pp.temp <- read.csv("~/Desktop/Research/UNM/Erin phenology project/PP_tmean_monthly.csv")
mbs.temp <- read.csv("~/Desktop/Research/UNM/Erin phenology project/MBS_tmean_monthly.csv")

pp.ppt <- read.csv("~/Desktop/Research/UNM/Erin phenology project/PP_ppt_monthlyMeans.csv")
mbs.ppt <- read.csv("~/Desktop/Research/UNM/Erin phenology project/MBS_ppt_monthlyMeans.csv")

names(pp.temp)
names(mbs.temp)

#Also load phenology data for later
phen <- read.csv("/Users/elizabethlombardi/Desktop/Research/UNM/Erin phenology project/Master_Dataframe_sorted.csv", header=TRUE) #I added a new column ("data_type") and renamed the CSV



#Merge for each variable
temp <- rbind(pp.temp, mbs.temp)
ppt <- rbind(pp.ppt, mbs.ppt)


#combine columns between the datasets
abio.df <- left_join(temp, ppt, by=c("site", "year", "month"))


## Seasonal dataframes

abio.df <- abio.df %>%
  mutate(season = case_when(
    month %in% c(12, 1, 2) ~ "winter",
    month %in% c(3, 4, 5) ~ "spring",
    month %in% c(6, 7, 8) ~ "summer",
    month %in% c(9, 10, 11) ~ "fall",
    TRUE ~ NA_character_
  )) %>%
  arrange(year, month) %>%
  mutate(season.year = ifelse(month == 12, year + 1, year)) %>%
  group_by(season) %>%
  mutate(ticker = dense_rank(season.year)) %>%
  ungroup() %>%
  mutate(season.label = paste0(season, ticker),
         seasonYear = ticker) %>%
  select(-season.year, -ticker)



#Merge abiotic seasonal data and records

####first, are the shared columns matching?
#phen <- phen %>%
  #mutate(across(c(month, year, site), as.character)) %>%
  #mutate(across(c(month, year, site), ~ trimws(as.character(.))))


abio.df <- abio.df %>%
 # mutate(across(c(month, year, site), as.character)) %>%
  mutate(site = toupper(site)) %>% 
  mutate(site = toupper(gsub("[[:punct:]\\s]", "", site))) 
#%>%  #the charactersin abio.df are originally lowercase and weren't matching
  #mutate(across(c(month, year, site), ~ trimws(as.character(.))))


###THIS is the merged dataset (phenology plus abiotic and seasonal data). Note that there are NA values if records are missing year/month, or if they predate 1895 (because PRISM data doesn't go back any farther)
phen.abio <- left_join(phen, abio.df, by = c("month", "year", "site"))
write.csv(phen.abio, "/Users/elizabethlombardi/Desktop/Research/UNM/Erin phenology project/Phenology collections project/phen.abio.csv")


###Check distribution of observations by site through time
#confirmed uneven sampling at MBS and PP pre-iNat data. Problem? Hopefully not...Just keep site as a factor variable...

ggplot(phen.abio, aes(x = year, y = tmean.avg, color=site)) + 
  geom_point(alpha=0.5)   

             
##### NEXT TASK
# I want to add ppt and temp data from preceding winter season (season_label) to the mixed effects linear models. For this, move over to the BayesianRegMods_alpPhenology.Rmd script

abio.seasonal <- abio.df %>%
  group_by(site, season.label) %>%  # Group by site, seasonal year, and season
  summarize(
    seas.mean.precip = mean(ppt.mean, na.rm = TRUE), #mean seasonal precipitation
    seas.total.precip = sum(ppt.mean, na.rm = TRUE), # Total seasonal precipitation
    seas.mean.temp = mean(tmean.avg, na.rm = TRUE), #Mean seasonal temperature
    seas.min.temp = min(tmean.avg, na.rm =TRUE), # Min seasonal temperature
    .groups = "drop"                               # Ungroup after summarizing
  )

phen.abio <- phen.abio %>%
  left_join(abio.seasonal, by = c("site", "season.label"))

save(phen.abio, abio.df, file="/Users/elizabethlombardi/Desktop/Research/UNM/Erin phenology project/Phenology collections project/phenAbio.RData")
load(file="/Users/elizabethlombardi/Desktop/Research/UNM/Erin phenology project/Phenology collections project/phenAbio.RData")


###QUICK descriptive stats about how seasonal values have changed over time 

abio.grp1 <- abio.df %>%
  group_by(season, season_label, site, year) %>%
  summarize(mean(ppt.mean)) 

abio.grp2 <- abio.df %>%
  group_by(season, season_label, site, year) %>%
  summarize(mean(tmean.avg)) 
  
abio.grp <- abio.grp1 %>%
  left_join(abio.grp2, by=c("season", "season_label", "site", "year")) %>%
  rename('pptSeasonMean'='mean(ppt.mean)') %>%
  rename('tmeanSeasonMean' = 'mean(tmean.avg)')

###Add seasonal min and max temp and ppt values to abio.grp (for fun...I don't think this is especially useful with only three values each season)
minPPT <- min(abio.df$ppt.mean)
maxPPT <- max(abio.df$ppt.mean)
minTemp <- min(abio.df$tmean.avg)
maxTemp <- max(abio.df$tmean.avg)  



#Generally have things changed seasonally over time? 

#Seasonal precipitation (all monthly data included, facet wrapped by season)
ggplot(abio.df, aes(x = year, y = ppt.mean)) +
  geom_point(alpha=0.5) +
  geom_smooth(method = "loess", se = FALSE, color="turquoise3") +
  labs(
    title = "Monthly precipitation over time",
    x = "Year",
    y = "Value"
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  facet_wrap(~season)


#Seasonal precipitation (averaged ppt by season)
ggplot(abio.grp, aes(x = year, y = pptSeasonMean)) +
  geom_point(alpha=0.5) +
  geom_smooth(method = "loess", se = TRUE, color="turquoise3") +
  theme_bw() +
  facet_wrap(~season) 




#Seasonal mean temperature (all monthly data included, facet wrapped by season)
ggplot(abio.df, aes(x = year, y = tmean.avg)) +
  geom_point(alpha=0.5) +
  geom_smooth(method = "loess", se = FALSE, color="coral") +
  labs(
    title = "Monthly average temperature over time",
    x = "Year",
    y = "Value"
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  facet_wrap(~season)


#Seasonal mean temperature (averaged ppt by season)
ggplot(abio.grp, aes(x = year, y = tmeanSeasonMean)) +
  geom_point(alpha=0.5) +
  geom_smooth(method = "loess", se = TRUE, color="coral") +
  theme_bw() +
  facet_wrap(~season) 


#Chop it a slightly different way: Season by season
winterTemp <- abio.df %>%
  filter(season=="winter") %>%
  ggplot(aes(x = year, y = tmean.avg)) +
  geom_point(alpha=0.5) +
  geom_smooth(method = "loess", se = FALSE, color="coral") +
  labs(
    title = "Monthly average temperature over time",
    x = "Year",
    y = "Value"
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  facet_wrap(~season)

winterPPT <- abio.df %>%
  filter(season=="winter") %>%
  ggplot(aes(x = year, y = ppt.mean)) +
  geom_point(alpha=0.5) +
  geom_smooth(method = "loess", se = FALSE, color="turquoise3") +
  labs(
    title = "Monthly average precipitation over time",
    x = "Year",
    y = "Value"
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  facet_wrap(~season)


#Panel plot for winter
winter.panel <- winterTemp + winterPPT




######OLD and NOT USEFUL BELOW THIS LINE (DELETE LATER)

#--------OLD
#PRECIP
pptshifted <- abio.df %>%
  group_by(season.label, site, seasonYear) %>%
  summarise(mean.ppt.seasonal = mean(ppt.mean, na.rm = TRUE)) 

pptshifted2 <- abio.df %>%
  group_by(season.label, site, seasonYear) %>%
  summarise(tot.ppt.seasonal = sum(ppt.mean, na.rm = TRUE)) 

df.shift<-df.shift %>%
  select(-seasonYear.y, -season_label.y)

# I. mean winter precipitation
pptshifted <- abio.df %>%
  group_by(season.label, site, seasonYear) %>%
  summarise(mean_ppt_mean = mean(ppt.mean, na.rm = TRUE)) %>%
  #mutate(seasonYear.shifted = seasonYear + 1)
  mutate(seasonYear.shifted = seasonYear)

pptshifted.winter <- pptshifted %>%
  filter(str_detect(season.label, "^winter\\d+"))

df.shift <- phen.abio %>% 
  left_join(pptshifted.winter, by = c("seasonYear"="seasonYear.shifted", "site")) %>%
  rename(prevWintPPT.mean = mean_ppt_mean)

df.shift<-df.shift %>%
  select(-seasonYear.y, -season_label.y)

# II. total winter precipitation
pptshifted <- abio.df %>%
  group_by(season.label, site, seasonYear) %>%
  summarise(tot_wintPPT = sum(ppt.mean, na.rm = TRUE)) %>%
  mutate(seasonYear.shifted = seasonYear + 1)

pptshifted.winter <- pptshifted %>%
  filter(str_detect(season.label, "^winter\\d+"))

df.shift <- df.shift %>% 
  left_join(pptshifted.winter, by = c("seasonYear"="seasonYear.shifted", "site")) %>%
  rename(tot.wintPPT = tot_wintPPT)

df.shift<-df.shift %>%
  select(-seasonYear.y.y)



#TEMP
tempshifted <- abio.df %>%
  group_by(season.label, site, seasonYear) %>%
  summarise(mean.temp.mean = mean(tmean.avg, na.rm = TRUE)) %>%
  mutate(seasonYear.shifted = seasonYear + 1)

tempshifted2 <- abio.df %>%
  group_by(season.label, site, seasonYear) %>%
  summarise(min.temp = min(tmean.avg, na.rm = TRUE)) %>%
  mutate(seasonYear.shifted = seasonYear + 1)

tempshifted.winter <- tempshifted %>%
  filter(str_detect(season_label, "^winter\\d+"))

df.shift <- df.shift %>% 
  left_join(tempshifted.winter, by = c("seasonYear"="seasonYear.shifted", "site")) %>%
  rename(prevWintTemp = mean.temp.mean)

df.shift<-df.shift %>%
  select(-seasonYear.y, -season.label)

#TEMP of spring of collection year? NOT WORKING YET
tempshifted <- df.shift %>%
  group_by(season.label.x, site, seasonYear) %>%
  summarise(min.temp.mean = min(tmean.avg, na.rm = TRUE)) 

#%>% mutate(seasonYear.shifted = seasonYear + 1)

tempshifted.spring <- tempshifted %>%
  filter(str_detect(season.label.x.y, "^spring\\d+"))

df.shift <- df.shift %>% 
  left_join(tempshifted.spring, by = c("seasonYear"="seasonYear.shifted", "site")) %>%
  rename(prevSpringTemp = mean.temp.mean)

df.shift<-df.shift %>%
  select(-seasonYear.y, -season.label)


###Add minimum spring temperature column (shifted because most records are from summer months)
#Idea after Al Kovaleski's talk: maybe I should also calculate an estimate for dormancy period (duration, min temp, accumulated snow)



#UPDATE main dataframe to include abiotic columsn from previous year: THIS IS WHAT GOES TO THE BAYESIAN MODELING SCRIPT
phen.abio <- df.shift
