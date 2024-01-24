##Exploratory Analyses
##Erin's Phenology project
##EML started this script October 2023

#libraries
library(dplyr)
library(ggplot2)
library(ggridges)
library(viridis)
library(khroma)



#Load data in CSV format. Check to see if it's the most up to date version with Erin. 
phen <- read.csv("/Users/elizabethlombardi/Desktop/Research/UNM/Erin phenology project/Master_Dataframe_sorted.csv", header=TRUE) 
phen <- read.csv("/Users/elizabethlombardi/Desktop/Research/UNM/Erin phenology project/Master_Dataframe_colorsEML.csv", header=TRUE) #This is the most up to date as of Jan 1 2024
View(phen)
names(phen)

#Or load the data stack
load("/Users/elizabethlombardi/Desktop/Research/UNM/Erin phenology project/Phenology collections project/phenologyCollProj_workspace.RData")


#define color palettes
siteCols = c("#4477AA", "#EE6677") #MBS is blue, PP is red
dataCols = c("#AA3377", "#CCBB44") #observation is maroon, specimen is gold


#some of the values are integers but should be numeric
#phen<- phen %>%
  mutate_if(is.integer, as.numeric) %>%
  mutate(latitude = as.numeric(latitude))


###### FIRST: TAKE A LOOK AT MEAN ORDINAL.DATE VALUES ACROSS CATEGORIES #######
#minimum date recorded for the three data sources
d2 <- phen %>%
  filter(!ordinal_date=="") %>%
  group_by(data_type) %>%
  summarize(meanDate = mean(ordinal_date)) 

d3 <-  phen %>%
  filter(!ordinal_date=="") %>%
  group_by(data_type) %>%
  summarize(minDate = min(ordinal_date)) 

#Join the tables that describe minimum and mean ordinal date by data type
dTab<- left_join(d2, d3, by="data_type")

#write the table with basic information about minimum and mean flowering ordinal date info by data type (specimen versus observation)
write.csv(dTab, "/Users/elizabethlombardi/Desktop/Research/UNM/Erin phenology project/Tables/basicTable_datatype.csv", row.names = TRUE)


#are the min and mean ordinal date data different from each other?
t.test(dTab$meanDate) #BARELY statistically significant but still different
t.test(dTab$minDate) #statistically significant

# I think the table is the best way to show the observation versus specimen data type differences (not the following ggplot)
ggplot(dTab, aes(y=meanDate, x=data_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Value by Category",
       x = "Category",
       y = "Mean Value") +
  theme_minimal() +
  facet_wrap(~ data_type, scales = "free_x")


#Mean date recorded across the species in question 
spp1 <- phen %>%
  #filter(!scientific.name=="") %>%
  na.omit() %>%
  group_by(scientific_name) %>%
  summarize(meanDate = mean(ordinal_date))

spp2 <- phen %>%
  na.omit() %>%
  group_by(scientific_name) %>%
  summarize(minDate = min(ordinal_date))

#Left join the mean and minimum information by species (across sites)
sppTab<- left_join(spp1, spp2, by="scientific_name")

#save the species mean/min table
write.csv(sppTab, "/Users/elizabethlombardi/Desktop/Research/UNM/Erin phenology project/Tables/speciesMeanMin.csv", row.names = TRUE)



#temporal variation by site and species (all recorded dates)
siteFig2<-ggplot(phen, aes(x = ordinal_date, y = subsite)) + #change between subsite and site depending on what you want to look at
  geom_boxplot(aes(fill = site), outlier.shape = NA) +
  labs(
    title = "Box Plot of Values by Category",
    x = "Date",
    y = "Value"
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_fill_manual(values=siteCols) +
  facet_wrap(~ data_type, scales = "free_x")



siteFig2.spp <- ggplot(phen, aes(x = ordinal_date, y = site)) +
  geom_boxplot(aes(fill = site), outlier.shape = NA) +
  labs(
    title = "Box Plot of Values by Category",
    x = "Date",
    y = "Value"
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_fill_manual(values=siteCols) +
  facet_wrap(~scientific_name)

ggsave("/Users/elizabethlombardi/Desktop/Research/UNM/Erin phenology project/Figures/siteFig2.png", siteFig2, width = 10, height = 5, dpi = 300)



#######
#geomridges comparing mean flowering time across species over time
sppRidges<-ggplot(phen, aes(x = ordinal_date, y = scientific_name, fill=factor(data_type))) + 
  geom_density_ridges(alpha=0.5) +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_fill_manual(values = dataCols)

ggsave("/Users/elizabethlombardi/Desktop/Research/UNM/Erin phenology project/Figures/sppRidges.png", sppRidges, width = 10, height = 5, dpi = 300)



###############################################
#are there statistical differences? 
#In order to say anything about phenology or phenological shifts, we need quantitative tests (statistical tests)

#1.Basic ANOVA for mean flowering time across sites
anova1 <- aov(ordinal_date ~ site, data=phen)
cat("One-way ANOVA for Mean Flowering Time across two sites:\n")
print(summary(anova1))

#2.Basic ANOVA for mean flowering time across subsites
anova2 <- aov(ordinal_date ~ subsite, data=phen)
cat("One-way ANOVA for Mean Flowering Time across all Sub-sites:\n")
print(summary(anova2))

#3. Basic ANOVA for mean flowering time across species. This compares the average ordinal.date values recorded for each species to other species. 
anova3 <- aov(ordinal_date ~ scientific_name, data=phen)
cat("One-way ANOVA for Mean Flowering Time across all species:\n")
print(summary(anova3))






###### NEXT: TAKE A LOOK AT MINIMUM ORDINAL.DATE (I.E. EARLIEST FLOWERING DATES RECORDED) VALUES ACROSS CATEGORIES #######

early_phen <- phen %>% #This isn't working properly! Not sure why...? minimum dates for a bunch of species is missing...
  group_by(site, scientific_name) %>%
  summarize(earliestDate = min(ordinal_date))

ggplot(phen, aes(x = ordinal_date, y = site)) +
  geom_boxplot(aes(color = site), outlier.shape = NA) +
  geom_jitter(aes(color=site), alpha=0.1) +
  labs(
    title = "Date of earliest flowering by Species and Site",
    x = "Date",
    y = "Value"
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_color_manual(values=colors)

ggplot(early_phen, aes(x = earliestDate, y = site)) +
  geom_point(aes(color = site), outlier.shape = NA) +
  labs(
    title = "Date of earliest flowering by Species and Site",
    x = "Date",
    y = "Value"
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_color_manual(values=colors) +
  facet_wrap(~scientific.name)


#statistical difference in mean first flowering date recorded by site?
anova4 <- aov(earliestDate ~ site, data=early_phen)
cat("One-way ANOVA for Mean Earliest Flowering Time by Site:\n")
print(summary(anova4))

anova5 <- aov(earliestDate ~ scientific.name, data=early_phen)
cat("One-way ANOVA for Mean Earliest Flowering Time by Species:\n")
print(summary(anova5)) #this confirms that different species initiate flowering at different times



#####Temporal change in earliest flowering dates??

#I. Generally, are flowers producing buds earlier?  'earlyPhen' is when all data, regardless of type are combined.
earlyPhen <- phen %>%
  group_by(year) %>%
  filter(ordinal_date == min(ordinal_date))
table(earlyPhen$data_type) #most are from herbarium specimens

write.csv(earlyPhen, "/Users/elizabethlombardi/Desktop/Research/UNM/Erin phenology project/earlyPhen.csv", row.names = TRUE)

# Plot the correlation
plot(earlyPhen$ordinal_date~earlyPhen$year) #very basic 

#i. All records in dataset (observations and herbarium specimen)
corrPlot1 <- ggplot(earlyPhen, aes(x = year, y = ordinal_date)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color="darkslategrey") +
  labs(title = "Correlation between Earliest Ordinal Date and Year for all records",
       x = "Year",
       y = "Earliest Ordinal Date") +
  theme_minimal() 

# Run linear regression and print summary
rm1 <- lm(ordinal_date ~ year, data = earlyPhen)
rsumm1 <- tidy(rm1)

print(corrPlot1)
print(rsumm1)




#ii. Only specimen data
earlyPhen.herb <- earlyPhen %>%
  filter(data_type=="specimen") #keep only the records from herbarium (in)

corrPlot2 <- ggplot(earlyPhen.herb, aes(x = year, y = ordinal_date)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color="goldenrod4") +
  labs(title = "Correlation between Earliest Ordinal Date and Year (Specimen data only)",
       x = "Year",
       y = "Earliest Ordinal Date") +
  theme_minimal() +
  facet_wrap(~site)

# Run linear regression and print summary
rm2 <- lm(ordinal_date ~ year, data = earlyPhen.herb)
rsumm2 <- tidy(rm2)

print(corrPlot2)
print(rsumm2)


#Run mixed effects model to test for significance in herbarium flowering with site as a random effect
hist(earlyPhen.herb$ordinal_date) #this is fine; the only continuous variable, really
hist(earlyPhen.herb$year) #this is...a bit skewed. Maybe transform but maybe it'll be okay for specimen data


#iii. Only specimen data
earlyPhen.obs <- earlyPhen %>%
  filter(data_type == "observation")

earlyPhen.obs2 <- earlyPhen.obs %>% #should we remove that single very-early 1994 date? Nah. Erin confirmed that it's real
  filter(!year<"2000")

corrPlot3 <- ggplot(earlyPhen.obs, aes(x = year, y = ordinal_date)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color="turquoise4") +
  labs(title = "Correlation between Earliest Ordinal Date and Year (Observation data only)",
       x = "Year",
       y = "Earliest Ordinal Date") +
  theme_minimal()

# Run linear regression and print summary
rm3 <- lm(ordinal_date ~ year, data = earlyPhen.obs)
rsumm3 <- tidy(rm3)

print(corrPlot3)
print(rsumm3)

#iii.B Check if the trend is statistically different when you exclude the 1994 point
corrPlot3B <- ggplot(earlyPhen.obs2, aes(x = year, y = ordinal_date)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color="turquoise4") +
  labs(title = "Correlation between Earliest Ordinal Date and Year (Observation data after 2000)",
       x = "Year",
       y = "Earliest Ordinal Date") +
  theme_minimal()

# Run linear regression and print summary
rm3B <- lm(ordinal_date ~ year, data = earlyPhen.obs)
rsumm3B <- tidy(rm3B)

print(corrPlot3B)
print(rsumm3B)




