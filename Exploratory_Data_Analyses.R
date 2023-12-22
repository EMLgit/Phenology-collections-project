##Exploratory Analyses
##Erin's Phenology project
##EML started this script October 2023

#libraries
library(dplyr)
library(ggplot2)
library(ggridges)
library(viridis)

#Load data in CSV format. Check to see if it's the most up to date version with Erin. 
phen <- read.csv("/Users/elizabethlombardi/Desktop/Research/UNM/Erin phenology project/Master_Dataframe_sorted.csv", header=TRUE) #I added a new column ("data_type") and renamed the CSV
View(phen)
names(phen)

#define color palette, as discussed with Erin. Green indicates Pikes Peak, Purple indicates Mt. Blue Sky
colors = (c("#D86FEF", "#0DA907"))


#some of the values are integers but should be numeric
phen<- phen %>%
  mutate_if(is.integer, as.numeric)


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
dTab<- left_join(d2, d3, by="dat")

#write the table with basic information about minimum and mean flowering ordinal date info by data type (specimen versus observation)
write.csv(dTab, "/Users/elizabethlombardi/Desktop/Research/UNM/Erin phenology project/Tables/basicTable_datatype.csv", row.names = TRUE)


#are the min and mean ordinal date data different from each other?
t.test(dTab$meanDate) #BARELY statistically significant but still different
t.test(dTab$minDate) #statistically significant


ggplot(dTab, aes(x = data_type)) +
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



#temporal variation by site and species (all recorded dates)
ggplot(phen, aes(x = ordinal_date, y = subsite)) +
  geom_boxplot(aes(fill = site), outlier.shape = NA) +
  labs(
    title = "Box Plot of Values by Category",
    x = "Date",
    y = "Value"
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_fill_manual(values=colors) 


ggplot(phen, aes(x = ordinal.date, y = subsite)) +
  geom_boxplot(aes(fill = site), outlier.shape = NA) +
  labs(
    title = "Box Plot of Values by Category",
    x = "Date",
    y = "Value"
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_fill_manual(values=colors) +
  facet_wrap(~scientific.name)


#geomridges comparing mean flowering time across species over time
ggplot(phen, aes(x = ordinal.date, y = scientific.name, fill=factor(site))) + 
  geom_density_ridges(alpha=0.4) +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_fill_manual(values = colors)


#are there statistical differences? 
#In order to say anything about phenology or phenological shifts, we need quantitiative tests (statistical tests)

#1.Basic ANOVA for mean flowering time across sites
anova1 <- aov(ordinal.date ~ site, data=phen)
cat("One-way ANOVA for Mean Flowering Time across two sites:\n")
print(summary(anova))

#2.Basic ANOVA for mean flowering time across subsites
anova2 <- aov(ordinal.date ~ subsite, data=phen)
cat("One-way ANOVA for Mean Flowering Time across all Sub-sites:\n")
print(summary(anova2))

#3. Basic ANOVA for mean flowering time across species. This compares the average ordinal.date values recorded for each species to other species. 
anova3 <- aov(ordinal.date ~ scientific.name, data=phen)
cat("One-way ANOVA for Mean Flowering Time across all species:\n")
print(summary(anova3))






###### NEXT: TAKE A LOOK AT MINIMUM ORDINAL.DATE (I.E. EARLIEST FLOWERING DATES RECORDED) VALUES ACROSS CATEGORIES #######

early_phen <- phen %>%
  group_by(site, scientific.name) %>%
  summarize(earliestDate = min(ordinal.date))

ggplot(early_phen, aes(x = earliestDate, y = site)) +
  geom_boxplot(aes(color = site), outlier.shape = NA) +
  geom_point(aes(color=site)) +
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






