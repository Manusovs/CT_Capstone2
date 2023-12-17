#add expected libraries (based off of previous capstone)
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("knitr")
library(dplyr)
library(ggplot2)
library(tidyverse)
library(knitr)


#FOR REPORT(1)
#Hypothesis:
print("Hypothesis 1: Most emissions come from the US and 2nd China, with India relatively high, other first world countries are a little lower, 3rd world countries are the least")
print("Hypothesis 2: US, China and first world countries have reduced their share of emissions over time")



#import data (this time I grabbed 2 sets, per country use and cumulative)
d_cap <- read.csv('co-emissions-per-capita.csv')
head(d_cap) #confirmation of data import
d_cum <- read.csv('share-of-cumulative-co2.csv')
head(d_cum) #confirmation of data import
#data sets are very similar at first look the only difference seems to be the unit.  I imagine the the constant conversion from year to year is different



#Begin looking at what the data looks like:
#Generic data shape
str(d_cap) #4 columns (Entity (which looks to be country), Code (relabel of country), year (integer), & value (float))
str(d_cum) #4 columns (Entity (which looks to be country), Code (relabel of country), year (integer), & value (float)) - has a few more values

#what do the values look like (basic statistics)
summary(d_cap) #1750 -2022, 0 - 772, median = 0.9333/ mean = 3.7, 3rd Q = 4.1504 (very strong skew a few countries have vastly more Co2 than the rest)
summary(d_cum) #1750 -2022 (note 1Q, median, etc are at different points), 0 - 100, median = 0.02/ mean = 5.29, 3rd Q = 0.36 (even stronger skew a few countries have vastly more cumulative Co2 than the rest)

#missing value review
sum(is.na(d_cap)) #no missing values!! (This is the first set I've worked with, that didn't have missing values)
sum(is.na(d_cum)) #no missing values!!

#Find unique values only interested for countries
unique(d_cap$Entity) #231 countries, looks like a good cross section (note this was a similar goal with the last capstone, except lots of wholes in the data made it nearly unworkable)
unique(d_cum$Entity) #235 countries, odd that it's not the same list of countries (I guess counties have formed and changed identity over 272 years)

#determine differences in counties (didn't expect a different number of values)
unique_ent_cap <- unique(d_cap$Entity)
unique_ent_cum <- unique(d_cum$Entity)

unique_to_cap <- setdiff(unique_ent_cap, unique_ent_cum) #expecting low to no answers 
cat("Entities unique to 'd_cap':\n", unique_to_cap, "\n") #nothing showed up, it might be because there are no values, confirm on reverse


unique_to_cum <- setdiff(unique_ent_cum, unique_ent_cap) # expecting 4 entities 
cat("Entities unique to 'd_cap':\n", unique_to_cum, "\n") #Antarctica, Christmas Island, International aviation, International shipping

#The data seems pretty comparable and doesn't seem to need to be 2 data sets, merge to one, to make graphing easier.  
merged <- merge(d_cap, d_cum, by = c("Entity", "Code", "Year"))
#confirm it worked
head(merged)



#Start graphing to get a better understanding of differences in years and countries
#Clean up the â sumbol
colnames(merged) <- gsub("â", "a", colnames(merged))
colnames(merged) #confirm it worked

# scatter plot to get a first visualization of how the actual and share compare
#This failed I used the wrong column names 
ggplot(merged, aes(x = 'Annual.CO..emissions..per.capita.', y = 'Share.of.global.cumulative.CO..emissions')) +
  geom_point() + 
  labs(title = "annual per capita vs cumulative share of CO2",
      x = "Annual per capita CO2 emissions",
      y = "Cumulative share of CO2") 

str(merged)
colnames(merged)

#Try "#2"
ggplot(merged, aes(x = Annual.CO..emissions..per.capita., y = Share.of.global.cumulative.CO..emissions)) +
  geom_point() + 
  labs(title = "annual per capita vs cumulative share of CO2",
       x = "Annual per capita CO2 emissions",
       y = "Cumulative share of CO2") 
# no correlation between amount CO2 per capita and cumulative share of CO2 used.  


#Add in color by year, to see if that gives us any additional information
ggplot(merged, aes(x = Annual.CO..emissions..per.capita., y = Share.of.global.cumulative.CO..emissions, color = as.factor(Year))) +
  geom_point() + 
  labs(title = "annual per capita vs cumulative share of CO2",
       x = "Annual per capita CO2 emissions",
       y = "Cumulative share of CO2") +
  scale_color_discrete(name = "Year")
#got an error

#Troubleshooting
str(merged) # Year is present with values : int 1949 1950 1951 . . . 
class(merged$Year) # [1] "integer"
sum(is.na(merged$Year)) #[1] 0

#step by step:
base_plot <- ggplot(merged, aes(x = Annual.CO..emissions..per.capita., y = Share.of.global.cumulative.CO..emissions)) +
  geom_point() + 
  labs(title = "annual per capita vs cumulative share of CO2",
       x = "Annual per capita CO2 emissions",
       y = "Cumulative share of CO2") 

print(base_plot) #confirmed it works

color_plot <- base_plot + aes(color = as.factor(Year)) +
  scale_color_discrete(name = "Year")
#FOR REPORT(2)
print(color_plot)
#There do seem to be trends in the years, although hard to determine what they are.  

#I want to zoom in on the annual per capita less than 40.  
d_40 <- filter(merged, 'Annual.CO..emissions..per.capita.' <= 40)
head(d_40) # no values
head(merged) # 5 values on every column
#used wrong symbol in filter

d_40 <- filter(merged, `Annual.CO..emissions..per.capita.` <= 40)
head(d_40) #works now

#copy paste graphs and change source.   
base_plot <- ggplot(d_40, aes(x = Annual.CO..emissions..per.capita., y = Share.of.global.cumulative.CO..emissions)) +
  geom_point() + 
  labs(title = "annual per capita vs cumulative share of CO2",
       x = "Annual per capita CO2 emissions",
       y = "Cumulative share of CO2") 

print(base_plot) #confirmed it works

#redo limiting to 25:
d_25 <- filter(merged, `Annual.CO..emissions..per.capita.` <= 25)
head(d_25) #confirmed it works

#copy paste graphs and change source.   
base_plot <- ggplot(d_25, aes(x = Annual.CO..emissions..per.capita., y = Share.of.global.cumulative.CO..emissions)) +
  geom_point() + 
  labs(title = "annual per capita vs cumulative share of CO2",
       x = "Annual per capita CO2 emissions",
       y = "Cumulative share of CO2")

print(base_plot) #confirmed it works
#noticed there are some multimodal trends largest cumulative has several peaks:
#largest peak is at 35% share at 23 annual per capita
# then other peaks 10% share at 17 annual per capita
# Around 65-80% share at around 12 annula per capita

color_plot <- base_plot + aes(color = as.factor(Year)) +
  scale_color_discrete(name = "Year")

print(color_plot) #confirmed it works
#It looks like there are various trends, probalby different for different countries

#group decades together to make the data more manageable
decades <- mutate(merged, Decade = floor(Year / 10) * 10) %>%
  group_by(Entity, Code, Decade) %>%
  summarize(
    Decade_Start = min(Year),
    Decade_Per_Capita = sum(`Annual.CO..emissions..per.capita.`),
    Share_Cum = mean(`Share.of.global.cumulative.CO..emissions`)
  ) %>%
  ungroup()
head(decades) #confirmed it works


base_plot <- ggplot(decades, aes(x = Decade_Per_Capita, y = Share_Cum)) +
  geom_point() + 
  labs(title = "decade per capita vs cumulative share of CO2",
       x = "Decade per capita CO2 emissions",
       y = "Cumulative share of CO2")

print(base_plot) #confirmed it works

d_250 <- filter(decades, `Decade_Per_Capita` <= 250)
head(d_250) #confirmed it works

#copy paste graphs and change source.   
base_plot <- ggplot(d_250, aes(x = Decade_Per_Capita, y = Share_Cum)) +
  geom_point() + 
  labs(title = "decade per capita vs cumulative share of CO2",
       x = "Decade per capita CO2 emissions",
       y = "Cumulative share of CO2")

print(base_plot) #confirmed it works

color_plot <- base_plot + aes(color = as.factor(Decade)) +
  scale_color_discrete(name = "Decade")

#FOR REPORT(3)
print(color_plot) #confirmed it works
#By reviewing by decades we can see that overall emissions over the decades are increasing.


#FOR REPORT(4)
#redraw graph emission vs time.  
base_plot <- ggplot(d_250, aes(x = Decade, y = Decade_Per_Capita)) +
  geom_point() + 
  labs(title = "decade per capita vs cumulative share of CO2",
       x = "Decade per capita CO2 emissions",
       y = "Per capita release of CO2")
print(base_plot)

#FOR REPORT(5)
print("Conclusion 0: Overall increase in emissions, no significant drop with climate agreements, although maybe a stalling of increases")


#Work on Hypothesis 1 check who are the largest emitters (both per capita and by share)
top_10_cap <- merged %>%
  group_by(Entity) %>%
  summarize(Total_Per_Capita = mean(`Annual.CO..emissions..per.capita.`)) %>%
  top_n(10, Total_Per_Capita) %>%
  arrange(desc(Total_Per_Capita))

#FOR REPORT(6)
print(top_10_cap) # confirmed it works
#US, China, India, and Western Europe didn't make it, very wealthy smaller countries did (this makes sense when accounting for population)

#repeat by share:
top_10_cum <- merged %>%
  group_by(Entity) %>%
  summarize(Total_Cum = mean(`Share.of.global.cumulative.CO..emissions`)) %>%
  top_n(10, Total_Cum) %>%
  arrange(desc(Total_Cum))

print(top_10_cum) # confirmed it works
# lots of groups included, this explains the count totaling way over 100%
# High income countries are the majority, lots of groups included

#Retry with 20 entities to make sure the large groups aren't hiding surprises.  
top_20_cum <- merged %>%
  group_by(Entity) %>%
  summarize(Total_Cum = mean(`Share.of.global.cumulative.CO..emissions`)) %>%
  top_n(20, Total_Cum) %>%
  arrange(desc(Total_Cum))

#FOR REPORT(7)
print(top_20_cum) # confirmed it works
# The UK has the highest share of emissions, followed by the US, Germany, France, China, then Russia.
# Note: India did not land among this group as I expected.  
# Most of the emissions are from high income countries (90%), upper middle had < 7%

target <- 'India'
rank_india <- merged %>%
  filter(Entity == target)  %>%
  summarize(Rank_Cum = mean(rank(desc(`Share.of.global.cumulative.CO..emissions`))))
 
#FOR REPORT(8)
print(paste(rank_india))
#India ranked about 78/235 entities, a handful of those are groups, so while India is higher than most it is far from a top emitter.  

#Now to check on which countries are at the bottom of the list (start with per capita).  
bot_20_per_capita <- merged %>%
  group_by(Entity) %>%
  summarize(Avg_per_cap = mean(`Annual.CO..emissions..per.capita.`)) %>%
  arrange(desc(Avg_per_cap)) %>%
  top_n(-20, Avg_per_cap)

#FOR REPORT(9)
print(bot_20_per_capita) # Confirms it works
#Most 3rd world countries in Africa

#repeat with cumulative values.  
bot_20_cum <- merged %>%
  group_by(Entity) %>%
  summarize(Avg_Cum = mean(`Share.of.global.cumulative.CO..emissions`)) %>%
  arrange(desc(Avg_Cum)) %>%
  top_n(-20, Avg_Cum)

#FOR REPORT(10)
print(bot_20_cum) # Confirms it works
#Mostly very small countries, many islands, some are touristy, some are not

#FOR REPORT(11)
cat("Conclusion 1: US is 2nd not first (UK is the highest),\n",
"China is a little further behind in 5th, w/ several western 1st world countries leading the pack \n",
"India is at about the 1/3 mark of all countries emitting, well below the top 10 as I expected.) \n",
"The least emitters are small island nations, some of which are very touristy\n",
"When accounting cumulative, wealthy small nations make up most of the emissions,\n",
"and 3rd world countries have the least per capita, most of which were also in Africa")



#Begin work on hypothesis 2:
#4 graphs: top per capita, top cumulative, bot per capita, bot cumulative (include India?)
# data  for top per capita
trend_top5_pc_countries <- c("Sint Maarten (Dutch part)", "Curacao", "Qatar", "United Arab Emirates", "Kuwait")
trend_top5_pc <- decades %>%  #used decades to make the results more readable
  filter(Entity %in% trend_top5_pc_countries)

# FOR REPORT(12)graph
ggplot(trend_top5_pc, aes(x = Decade, y = `Decade_Per_Capita`, color = Entity)) +
  geom_line() +
  labs(title = "Top 5 Countries per capita over Time", 
       x = "Decade",
       y = "Emissions per capita",
       color = "Country") +
  theme_minimal()


# data  for top per capita
trend_top5_pc_countries <- c("United Kingdom", "United States", "Germany", "France", "China")
trend_top5_pc <- decades %>%  #used decades to make the results more readable
  filter(Entity %in% trend_top5_pc_countries)

# FOR REPORT(13) graph top 5 countries over decades
ggplot(trend_top5_pc, aes(x = Decade, y = `Decade_Per_Capita`, color = Entity)) +
  geom_line() +
  labs(title = "Top 5 Countries per capita over Time", 
       x = "Decade",
       y = "Emissions per capita (sum of the decade)",
       color = "Country") +
  theme_minimal()

# data  for top per capita (+India)
trend_top6_pc_countries <- c("United Kingdom", "United States", "Germany", "France", "China","India")
trend_top6_pc <- decades %>%  #used decades to make the results more readable
  filter(Entity %in% trend_top6_pc_countries)

# FOR REPORT(14) graph top 5 countries + India over decades
ggplot(trend_top6_pc, aes(x = Decade, y = `Decade_Per_Capita`, color = Entity)) +
  geom_line() +
  labs(title = "Top 5 Countries per capita over Time", 
       x = "Decade",
       y = "Emissions per capita (sum of the decade)",
       color = "Country") +
  theme_minimal()
#India looks like an increasing normal curve, an order of magnitude less than the rest

# FOR REPORT(15) Graph top 5+ India with share of cumulative
ggplot(trend_top6_pc, aes(x = Decade, y = `Share_Cum`, color = Entity)) +
  geom_line() +
  labs(title = "Top 5 Countries (by share)", 
       x = "Decade",
       y = "Cumulative share (sum of the decade)",
       color = "Country") +
  theme_minimal()

# data  for bottom per capita 
trend_bot10_pc_countries <- c("Burundi","Sierra Leone", "Ethiopia","Rwanda", "Central Africann Republic", "Guinea", "Chad", "Niger", "Somalia", "Uganda")
trend_bot10_pc <- decades %>%  #used decades to make the results more readable
  filter(Entity %in% trend_bot10_pc_countries)

# FOR REPORT(16) graph bottom 10 countries over decades
ggplot(trend_bot10_pc, aes(x = Decade, y = `Decade_Per_Capita`, color = Entity)) +
  geom_line() +
  labs(title = "Bottom 10 Countries per capita over Time", 
       x = "Decade",
       y = "Emissions per capita (sum of the decade)",
       color = "Country") +
  theme_minimal() +
  #They don't start until 1930ish
  scale_x_continuous(limits = c(1930, max(trend_bot10_pc$Decade)))
#Bimodal most countries have a peak at 1975 and then start increasing again around 1990 -2010


#FOR REPORT(17) Graph bottom 10 with share of cumulative
ggplot(trend_bot10_pc, aes(x = Decade, y = `Share_Cum`, color = Entity)) +
  geom_line() +
  labs(title = "Bottom 10 Countries share over Time", 
       x = "Decade",
       y = "Cumulative share (sum of the decade)",
       color = "Country") +
  theme_minimal() +
  scale_x_continuous(limits = c(1930, max(trend_bot10_pc$Decade)))
#Most increasing, Somalia is slightly decreasing, and Sierra Leone is almost holding steady

#Review graphs and edit the below!!!!!!!!
cat("Conclusion 2: Most of the 1st world lead emitters have reduced their share over time, despite still being the lead\n",
    "In share they have been in a long downward trend, in per capita it has been more recent \n",
    "China (and India) have both been increasing their share and per capita previous to the current decade\n",
    "The least emitters are increasing their share of emissions\n",
    "The lowest emitters have a very noisy emissions per capita, and over the next few decades a stronger trend may emerge\n",
    "The gap between the highest and lowest emitters is shrinking (although still vast)")