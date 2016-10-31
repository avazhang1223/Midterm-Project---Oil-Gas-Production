# County-level oil and gas production
# Group Member: Mengze Yin, Tianchi Zhang, Yilin Li
# Objective of this project : 
# Discover the relationship between population level and oil and gas production

library(tidyr)
library(dplyr)
library(readr)
library(data.table)
library(doBy)
library(ggplot2)

# Part 1. Tidy Data

# Read file "Oilgascounty.csv" (Reference: www.ers.usda.gov)
raw <- read.csv("oilgascounty.csv",stringsAsFactors = FALSE)
raw <- raw[, c("FIPS","geoid","Stabr","County_Name","Rural_Urban_Continuum_Code_2013","Urban_Influence_2013","Metro_Nonmetro_2013","Metro_Micro_Noncore_2013","oil2000","oil2001","oil2002","oil2003","oil2004","oil2005","oil2006","oil2007","oil2008","oil2009","oil2010","oil2011","gas2000","gas2001","gas2002","gas2003","gas2004","gas2005","gas2006","gas2007","gas2008","gas2009","gas2010","gas2011","oil_change_group","gas_change_group","oil_gas_change_group")]

# Extract column names in order to revise them 
names(raw)[3] <- "State"
names(raw)[4] <- "County"
names(raw)[5] <- "Population_Level"
names(raw)[6] <- "Urban_Influence"
names(raw)[7] <- "If_Metro"
names(raw)[8] <- "Metro_Micro_Noncore"

raw


# Split the table into two because there are multiple types in one table: County & Oil and Gas Production
# Create two tables

# Select the information of County to form a new table  
county <- raw %>% 
  select(FIPS, State, County,Population_Level,Urban_Influence, If_Metro,Metro_Micro_Noncore) %>%
  unique() %>%
  mutate(county_id = row_number())

# To reduce repetition and enhance readability, eliminate the word "County" from the County column
county$County <- sub("County","",county$County)
county


# Select the information of Oil and Gas Production for each county to form a new table
oil_gas0 <- raw %>% 
  select(FIPS, oil2000,oil2001,oil2002,oil2003,oil2004,oil2005,oil2006,oil2007,oil2008,oil2009,oil2010,oil2011,gas2000,gas2001,gas2002,gas2003,gas2004,gas2005,gas2006,gas2007,gas2008,gas2009,gas2010,gas2011) %>%
  unique() %>%
  mutate()

# Because column headers include values (oil/gas + year, eg: oil2003), we melted the column heads into rows 
# First, gather up the non-variable columns
oil_gas1 <- oil_gas0 %>% 
  gather(demo, n, -FIPS, na.rm = TRUE)

# Second, seperate column headers
oil_gas_tidy <- oil_gas1 %>% 
  separate(demo, c("production", "year"), 3)




# Part 2. Data Summary

# (1) average annual oil and gas production for each county
raw$oil_production <- rowSums(raw[9:20])/12
raw$gas_production <- rowSums(raw[21:32])/12

mean_county_production <- raw %>% 
  select(FIPS,Population_Level, oil_production, gas_production) %>%
  unique() %>%
  mutate(county_id = row_number())


# (2) nationalwide mean production (per county) in each year to compare nationwide oil and gas production
# nationalwide oil production
mean_oil <- colMeans(raw[9:20], na.rm = FALSE, dims =1L)
mean_oil <- data_frame(year=c(2000:2011),mean_oil=mean_oil)

# nationalwide gas production
mean_gas <- colMeans(raw[21:32], na.rm = FALSE, dims =1L)
mean_gas <- data_frame(year=c(2000:2011),mean_gas=mean_gas)

# Merge two tables into one for future visulization
national_mean_production <- merge(mean_oil,mean_gas,by="year")
names(national_mean_production)[2] <- "typeoil"
names(national_mean_production)[3] <- "typegas"
national_mean_production <- national_mean_production %>%
  gather(type,amount,typeoil:typegas) %>%
  mutate(type = gsub("type","",type)) %>%
  arrange(year, type)


# (3) average oil and gas production for counties by each population level 
# Select the information of population level,Oil and Gas Production for each county to form a new table
County_Level_Mean <- raw %>% 
  select(Population_Level, oil2000,oil2001,oil2002,oil2003,oil2004,oil2005,oil2006,oil2007,oil2008,oil2009,oil2010,oil2011,gas2000,gas2001,gas2002,gas2003,gas2004,gas2005,gas2006,gas2007,gas2008,gas2009,gas2010,gas2011) %>%
  mutate()

# Get the average county oil and gas production for each population level in each year
County_Level_Mean <- aggregate(County_Level_Mean[,2:25],list(County_Level_Mean$Population_Level), mean)
names(County_Level_Mean)[1] <- paste("County_Level")
County_Level_Mean

# Tidy the table 
County_Level_Mean0 <- County_Level_Mean %>%
  gather(demo, amount, -County_Level, na.rm = TRUE)

County_Level_Mean_tidy <- County_Level_Mean0 %>% 
  separate(demo, c("production", "year"), 3)

# Create a table with county, county level defined by population, sum of oil production in 12 years and sum of gas production in 12 years
county_production <- raw %>% 
  select(FIPS,Population_Level, oil_production, gas_production) %>%
  unique() %>%
  mutate(county_id = row_number())
county_production

#
County_Level_Mean_Oil <- County_Level_Mean %>%
  gather(oil,oil_production_mean, oil2000:oil2011, na.rm = TRUE) %>%
  mutate(year = parse_number(oil))%>%
  select(County_Level, year, oil_production_mean) %>%
  unique() %>%
  mutate()

County_Level_Mean_Gas <- County_Level_Mean %>%
  gather(gas,gas_production_mean, gas2000:gas2011, na.rm = TRUE) %>%
  mutate(year = parse_number(gas))%>%
  select(County_Level, year, gas_production_mean) %>%
  unique() %>%
  mutate()

# Part 3. Visualization
require(ggplot2) 


# 1
# ggplot for "Relationship between Oil Production and Gas Production from 2000-2011" 
ggplot(data=national_mean_production, aes(x=factor(year), y=amount, fill=type)) +
  geom_bar(stat="identity", position =position_dodge())+
  xlab("Year")+
  ylab("Production")+
  ggtitle("Oil Production vs. Gas Production")+
  scale_y_continuous(breaks=seq(0, 8000000, 1000000))

# 2
# ggplot for "Oil Production for each County Level"
ggplot(data = county_production, aes(factor(Population_Level),oil_production,fill=factor(Population_Level)))  +
  geom_bar(stat = "identity")+
  xlab("County Level")+
  ylab("Mean Oil Production")+
  labs(fill="County Level")+
  scale_y_continuous(breaks=seq(0, 1200000000, 50000000))+
  ggtitle("Oil Production By County Level")

# ggplot for "Gas Production for each County Level"
ggplot(data = county_production, aes(factor(Population_Level),gas_production,fill=factor(Population_Level)))  +
  geom_bar(stat = "identity")+
  xlab("County Level")+
  ylab("Mean Gas Production")+
  labs(fill="County Level")+
  scale_y_continuous(breaks=seq(0, 12000000000, 500000000))+
  ggtitle("Gas Production By County Level")


# 3
# Stacked Column
#ggplot(moil_countylevel, aes(year, fill=countylevel)) + geom_bar()

# 4
# ggplot for "Annual Oil Production for each County Level"
ggplot(data = County_Level_Mean_Oil, aes(x = factor(year), y = oil_production_mean,colour = County_Level)) +       
  xlab("Year")+
  ylab("Mean Oil Production")+
  ggtitle("Oil Production By Year")+
  geom_line(aes(group = County_Level)) + geom_point()+
  scale_y_continuous(breaks=seq(0, 12000000, 200000))+
  scale_colour_gradient(low = 'lightblue', high = 'darkblue', breaks=c(1:9))

# ggplot for "Annual Oil Production for each County Level"
ggplot(data = County_Level_Mean_Gas, aes(x = factor(year), y = gas_production_mean,colour = County_Level)) +       
  xlab("Year")+
  ylab("Mean Gas Production")+
  ggtitle("Gas Production By Year")+
  geom_line(aes(group = County_Level)) + geom_point()+
  scale_y_continuous(breaks=seq(0, 120000000, 2000000))+
  scale_colour_gradient(low = 'lightblue', high = 'darkblue', breaks=c(1:9))



##Add:

oilstart <- which(colnames(raw)=="oil2000")
oilend <- which(colnames(raw)=="oil2011")
gasstart <- which(colnames(raw)=="gas2000")
gasend <- which(colnames(raw)=="gas2011")

# Stacked bar plot: total oil/gas production for each county in each year
#Oil
County_Level_Mean_year_oil <- County_Level_Mean %>% 
  select(oil2000,oil2001,oil2002,oil2003,oil2004,oil2005,oil2006,oil2007,oil2008,oil2009,oil2010,oil2011) %>%
  unique() %>%
  mutate()
County_Level_Mean_year_oil <- data.frame(t(County_Level_Mean_year_oil))
setDT(County_Level_Mean_year_oil, keep.rownames = TRUE)
names(County_Level_Mean_year_oil)[2:10]<-paste("County",1:9)
names(County_Level_Mean_year_oil)[1] <- "Year"
County_Level_Mean_year_oil$Year <- sub("oil","",County_Level_Mean_year_oil$Year)

#Gas
County_Level_Mean_year_gas <- County_Level_Mean %>% 
  select(gas2000,gas2001,gas2002,gas2003,gas2004,gas2005,gas2006,gas2007,gas2008,gas2009,gas2010,gas2011) %>%
  unique() %>%
  mutate()
County_Level_Mean_year_gas <- data.frame(t(County_Level_Mean_year_gas))
setDT(County_Level_Mean_year_gas, keep.rownames = TRUE)
names(County_Level_Mean_year_gas)[2:10]<-paste("County",1:9)
names(County_Level_Mean_year_gas)[1] <- "Year"
County_Level_Mean_year_gas$Year <- sub("gas","",County_Level_Mean_year_gas$Year)

#Stacked chart
counts <- table(County_Level_Mean_year_oil$County1,County_Level_Mean_year_oil$County2,County_Level_Mean_year_oil$County3,County_Level_Mean_year_oil$County4,County_Level_Mean_year_oil$County5,County_Level_Mean_year_oil$County6,County_Level_Mean_year_oil$County7,County_Level_Mean_year_oil$County8,County_Level_Mean_year_oil$County9)
barplot(counts, main="Car Distribution by Gears and VS",
        xlab="Number of Gears", col=c("darkblue","red"),
        legend = rownames(counts))


#fuck..lets start over

#Table: Mean Oil Production By County
County_Level_Mean_Oil_year <- County_Level_Mean %>% 
  select(oil2000,oil2001,oil2002,oil2003,oil2004,oil2005,oil2006,oil2007,oil2008,oil2009,oil2010,oil2011) %>%
  unique() %>%
  mutate()
names(County_Level_Mean_Oil_year)[1:12] <- (2000:2011)

#Stacked Barplot: Mean Oil Production By County
barplot(as.matrix(County_Level_Mean_Oil_year),xlab="Year",ylab="Mean Oil Production",main="Mean Oil Production By County",ylim=c(0,5e+6),col=heat.colors(9))+
  legend(title="County","right",c("9","8","7","6","5","4","3","2","1"),cex=0.9,inset=c(-0.08,0),bty="n",xpd=TRUE,fill=rev(heat.colors(9)))



#Table: Mean Gas Production By County
County_Level_Mean_Gas_year <- County_Level_Mean %>% 
  select(gas2000,gas2001,gas2002,gas2003,gas2004,gas2005,gas2006,gas2007,gas2008,gas2009,gas2010,gas2011) %>%
  unique() %>%
  mutate()
names(County_Level_Mean_Gas_year)[1:12] <- (2000:2011)

#Stacked Barplot: Mean Gas Production By County
barplot(as.matrix(County_Level_Mean_Gas_year),xlab="Year",ylab="Mean Gas Production",main="Mean Gas Production By County",ylim=c(0,8e+7),col=heat.colors(9))+
legend(title="County","right",c("9","8","7","6","5","4","3","2","1"),cex=0.9,inset=c(-0.08,0),bty="n",xpd=TRUE,fill=rev(heat.colors(9)))

