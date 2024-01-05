###Phenology Estimation script
###Based on Pearse et al 2017
###phest package


#load data
phen <- read.csv("/Users/elizabethlombardi/Desktop/Research/UNM/Erin phenology project/Master_Dataframe_sorted.csv", header=TRUE) #I added a new column ("data_type") and renamed the CSV

#libraries
library(devtools)
#install_github("willpearse/phest")
library(phest)
library(dplyr)
library(ggplot2)


###clean the dataset
phen2 <- phen %>% 
  filter(!ordinal_date==" ") 


#create vector of dates for weibull distribution across species
v1 <- c(phen2$ordinal_date)
mean(v1) #203.3899
min(v1) #135

weib.basic <- weib.limit(x=v1, k=30, upper=FALSE, alpha = 0.05) #this seems to kinda work? 
raw.min = min(phen2$ordinal_date) #compare to the earliest flowering date recorded


######## How does your chosen k value impact the estimate value?
# Create an empty vector to store the estimates
estimates <- numeric()

# Loop through k values from 10 to 30
for (k_value in 10:50) {
  # Run the function and store the estimate
  result <- weib.limit(x = v1, k = k_value, upper = FALSE, alpha = 0.05)
  estimate <- result[1]
  estimates <- c(estimates, estimate)
}

plot(10:50, estimates, type = "l", xlab = "k values", ylab = "Estimates", main = "Estimates vs k values")



####Compare estimated earliest flowering dates for different groups of your data


##1. Guilds (i.e. Pikes Peak guild versus Mt Blue Sky guild)

#subste data
pp.guild <- phen2 %>% 
  filter(site=="PP")
bs.guild <- phen2 %>%
  filter(site == "MBS")

#Estimate earliest flowering interval for site guilds
weibPP <- unname(weib.limit(x=c(pp.guild$ordinal_date), k=16, upper=FALSE, alpha = 0.05))#why won't anything greater than k=16 work?
weibBS <- unname(weib.limit(x=c(bs.guild$ordinal_date), k=30, upper=FALSE, alpha = 0.05))

#Create a dataframe showing estimated and observed values for guilds
weibGuild <- rbind(weibPP, weibBS)
colnames(weibGuild) <- c("estimate", "est.min", "est.max")

min(pp.guild$ordinal_date)
min(bs.guild$ordinal_date) #there are way more observations from mt blue sky
mins <-data.frame(obs.mins=c(min(pp.guild$ordinal_date), min(bs.guild$ordinal_date)))

rows <- data.frame(guild=c("pp", "bs"))
weibGuild <- cbind(rows,weibGuild, mins) #Use this later


#Check distributions of pp and bs guilds to see if CI issues are from that
pp.dens <- density(pp.guild$ordinal_date)
plot(pp.dens)
bs.dens <- density(bs.guild$ordinal_date)
plot(bs.dens)

#compare in same plot
sm.density.compare(phen2$ordinal_date, phen2$site, xlab="guild densities") #they're basically the same

#compare number of records? This could be an issue...
ggplot(phen2, aes(x=subsite, fill=site)) +
  geom_bar()


#Basic Guild comparison plot; observed versus estimated

# Reshape data from wide to long format
weibGuild_long<- tidyr::gather(weibGuild, "Metric", "Value", -guild, -obs.mins)

ggplot(weibGuild_long, aes(x = Value, y = guild, color = guild, shape = Metric)) +
  geom_line(aes(group = guild), size = 1) +  # Line plot
  geom_point(aes(size = Metric), shape=18, alpha=0.5)  +  # Dot plot
  geom_point(aes(x = obs.mins, y = guild), shape = 20, size = 2.5, color = "red", alpha=0.5) +  # Red circles for obs.min
  scale_color_manual(values = c("darkslategrey", "darkslategrey")) +
  #scale_shape_manual(values = c(1, 1, 1, 1, 5)) +  # Customize point shapes
  #geom_point(aes(x=Value, y=guild, size = factor(obs.mins)), shape = 5, color = "darkslategrey") +  # Additional point shape with SingleValue
  #geom_point( aes(size = factor(obs.mins)), color = "black") +  # Additional point shape with SingleValue
  labs(x = "Flowering Date", y = "Guild", color = "Guild", shape = "Metric") +  # Set axis labels and legends
  ggtitle("Estimated vs. Observed Values") + # Set plot title
  theme_bw()





#####NOW: estimates by species
###To get around the named numeric variable thing more quickly, I just created a table in CSV format
#UPDATE: I created this below (weib.spp) as well

weibDF <- read.csv("/Users/elizabethlombardi/Desktop/Research/UNM/Erin phenology project/WeibullEstimations_species.csv", header=TRUE)

ggplot(weibDF, aes(x = estimate, y = species, color = species)) +
  geom_point() +  # Scatter plot
  labs(x = "Numeric Variable 1", y = "Numeric Variable 2", color = "Category") +  # Set axis labels and color legend title
  ggtitle("Scatter Plot of Numeric Variable 1 vs. Numeric Variable 2 by Category")  # Set plot title


inat <-phen2 %>%
  filter(data_type=="obs")
herb <- phen2 %>%
  filter(data_type =="herb")
erin <- phen2 %>%
  filter(source == "Berkowitz")

weib.limit(x=c(inat$ordinal.date), k=30, upper=FALSE, alpha = 0.05) 
weib.limit(x=c(herb$ordinal.date), k=30, upper=FALSE, alpha = 0.05) 
weib.limit(x=c(erin$ordinal.date), k=30, upper=FALSE, alpha = 0.05) 


min(inat$ordinal.date)
min(herb$ordinal.date)
min(erin$ordinal.date)


###Can I just group and calculate?
weibDF1 <- phen2 %>%
  group_by(scientific_name) %>%
  mutate(weib.spp = weib.limit(x=c(ordinal_date), k=10, upper=FALSE, alpha=0.05)[1])  #k only 10
weibDF2 <- phen2 %>%
  group_by(scientific_name) %>%
  mutate(weib.spp = weib.limit(x=c(ordinal_date), k=10, upper=FALSE, alpha=0.05)[2])  #k only 10
weibDF3 <- phen2 %>%
  group_by(scientific_name) %>%
  mutate(weib.spp = weib.limit(x=c(ordinal_date), k=10, upper=FALSE, alpha=0.05)[3])  #k only 10


spp1<- weibDF1 %>%
  group_by(scientific_name, weib.spp) %>%
  summarize()
spp2 <- weibDF2 %>%
  group_by(scientific_name, weib.spp) %>%
  summarize()
spp3 <- weibDF3 %>%
  group_by(scientific_name, weib.spp) %>%
  summarize()
spp4<-weibDF %>%
  group_by(scientific_name) %>%
  summarise(obs.min = min(ordinal_date))

  
#This is the species table with estimated and observed earliest dates
weib.spp <- spp1 %>%
  left_join(spp2, by="scientific_name") %>%
  left_join(spp3, by="scientific_name") %>%
  rename(c("estimate"="weib.spp.x","est.min"="weib.spp.y", "est.max" = "weib.spp")) %>%
  left_join(spp4, by="scientific_name")

write.csv(weib.spp, "/Users/elizabethlombardi/Desktop/Research/UNM/Erin phenology project/WeibullEstimations_speciesTable.csv", row.names = TRUE)
         

# Reshape data from wide to long format
weibSpp_long<- tidyr::gather(weib.spp, "Metric", "Value", -scientific_name, -obs.min)

ggplot(weibSpp_long, aes(x = Value, y = scientific_name, color = Metric, shape = Metric)) +
  geom_line(aes(group = scientific_name), size = 1) +  # Line plot
  geom_point(aes(size = Metric), shape=18, alpha=0.7)  +  # Dot plot
  geom_point(aes(x = obs.min, y = scientific_name), shape = 20, size = 2.5, color = "red", alpha=0.5) +  # Red circles for obs.min
  scale_shape_manual(values = c(1, 1, 1, 1, 5)) +  # Customize point shapes
  labs(x = "Flowering Date", y = "Species", color = "Metric") +  # Set axis labels and legends
  ggtitle("Estimated vs. Observed Values") + # Set plot title
  theme_bw()  





####Now compare by data types
##herbarium versus observational
#subset data
specimen <- phen2 %>% 
  filter(data_type=="specimen")
inat <- phen2 %>%
  filter(data_type == "observation")

#Estimate earliest flowering interval for site guilds
weibHerb <- unname(weib.limit(x=c(specimen$ordinal_date), k=30, upper=FALSE, alpha = 0.05))
weibiNat <- unname(weib.limit(x=c(inat$ordinal_date), k=30, upper=FALSE, alpha = 0.05))

#Create a dataframe showing estimated and observed values for guilds
weibDataType <- rbind(weibHerb, weibiNat)
colnames(weibDataType) <- c("estimate", "est.min", "est.max")

min(specimen$ordinal_date)
min(inat$ordinal_date)
mins <-data.frame(obs.mins=c(min(specimen$ordinal_date), min(inat$ordinal_date)))

rows <- data.frame(type=c("herbarium", "inaturalist"))

weibDataType <- cbind(rows, weibDataType, mins) #Use this later



# Reshape data from wide to long format
weibDataType_long<- tidyr::gather(weibDataType, "Metric", "Value", -type, -obs.mins)

ggplot(weibDataType_long, aes(x = Value, y = type, color = type, shape = Metric)) +
  geom_line(aes(group = type), size = 1) +  # Line plot
  geom_point(aes(size = Metric), shape=18, alpha=0.5)  +  # Dot plot
  geom_point(aes(x = obs.mins, y = type), shape = 20, size = 2.5, color = "red", alpha=0.5) +  # Red circles for obs.min
  scale_color_manual(values = c("darkslategrey", "darkslategrey")) +
  #scale_shape_manual(values = c(1, 1, 1, 1, 5)) +  # Customize point shapes
  #geom_point(aes(x=Value, y=guild, size = factor(obs.mins)), shape = 5, color = "darkslategrey") +  # Additional point shape with SingleValue
  #geom_point( aes(size = factor(obs.mins)), color = "black") +  # Additional point shape with SingleValue
  labs(x = "Flowering Date", y = "Data Type", shape = "Metric") +  # Set axis labels and legends
  ggtitle("Estimated vs. Observed Values") + # Set plot title
  theme_bw()




######OVERALL ADVANCEMENT IN FLOWERING?
# to check this, I think we need to subset the full dataset to keep only years with sufficient data (N=5 for now)

your_data <- phen2 %>%
  group_by(year) %>%
  filter(n() >= 5) %>%
  ungroup()

# Get unique years
unique_years <- unique(your_data$year)

# Initialize an empty list to store results
weibull_results_list <- list()

# Loop through unique years
for (year_value in unique_years) {
  # Subset the data for the current year
  subset_data <- your_data[your_data$year == year_value, ]
  
  # Apply weib.limit function
  weibull_result <- weib.limit(x = subset_data$ordinal_date) #naive estimation using the function without alpha or k value
  
  # Create a dataframe with the results
  result_df <- data.frame(
    year = year_value,
    estimate = weibull_result[[1]],
    lower_ci = weibull_result[[2]],
    upper_ci = weibull_result[[3]]
  )
  
  # Append to the list
  weibull_results_list <- c(weibull_results_list, list(result_df))
}

# Convert the list to a dataframe
weibAnnualDF <- do.call(rbind, weibull_results_list)

#Join the weibAnnualDF estimations to observed earliest flowering date for the yeras that have sufficient data

weibMin <- weibAnnualDF %>%
  filter(!estimate=="") 

obs.min <- phen2 %>%
  group_by(year) %>%
  summarize(obs.min=min(ordinal_date))

annualMins<- left_join(weibMin, obs.min, by="year")


ann1<-ggplot(annualMins, aes(x = year, y = obs.min)) +
  geom_point(alpha=0.4, color="turquoise3") +
  geom_smooth(method = "lm", se = FALSE, color="turquoise4") +
  labs(title = "Observed and estimated earliest flowering dates by year",
       x = "Year",
       y = "Earliest Ordinal Date") +
  theme_minimal()

ann2 <- ann1 + 
  geom_point(aes(x=year, y= estimate), alpha=0.5, color="darkslategrey") +
  geom_smooth(aes(x = year, y = estimate), method = "lm", se = FALSE, color = "darkslategrey", linetype = "dashed")






