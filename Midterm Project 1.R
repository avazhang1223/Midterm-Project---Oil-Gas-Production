# County-level oil and gas production
# Group Member: Mengze Yin, Tianchi Zhang, Yilin Li
# Objective of this project : 
# Discover the relationship between urbanization level and oil and gas production


library(tidyr)
library(dplyr)
library(readr)
library(data.table)
library(doBy)
library(RColorBrewer)

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


# Select the information of Oil and Gas Production to form a new table
oil_gas0 <- raw %>% 
  select(FIPS, oil2000,oil2001,oil2002,oil2003,oil2004,oil2005,oil2006,oil2007,oil2008,oil2009,oil2010,oil2011,gas2000,gas2001,gas2002,gas2003,gas2004,gas2005,gas2006,gas2007,gas2008,gas2009,gas2010,gas2011) %>%
  unique() %>%
  mutate(production_id = row_number())

# Because column headers include values (oil/gas + year, eg: oil2003), we melted the column heads into rows 
# First, melt down the oil info into two columns: oil + oil_production
oil_gas1 <- oil_gas0 %>% 
  gather(oil,oil_production, oil2000:oil2011, na.rm = TRUE)

# Second, melt down the gas info into two columns: gas + gas_production
oil_gas2 <- oil_gas1 %>%   
  gather(gas, gas_production, gas2000:gas2011, na.rm = TRUE)

# Extract the year information from "oil" or "gas" column (eg. oil2001) and created a "year" column
oil_gas3 <- oil_gas2 %>%
  mutate(
    year = parse_number(oil)
  )

# Omit "oil" and "gas" columns by selecting the rest information and create a new data table called "oil gas"
oil_gas_tidy <- oil_gas3 %>% 
  select(FIPS, year,oil_production,gas_production) %>%
  unique() %>%
  mutate()
oil_gas_tidy

oil_gas_final <- oil_gas_tidy %>%
  gather(type,amount,oil_production:gas_production)%>%
  mutate(type = gsub("_production","",type)) %>%
  arrange(FIPS,year,type)







# From here, we give some basic summarized statistics of the dataset as a preparation for futher exploration

# average annual oil and gas production by each county
dframe = data.frame(county = oil_gas_tidy$FIPS, oil=oil_gas_tidy$oil_production, gas=oil_gas_tidy$gas_production, year=oil_gas_tidy$year)

# mean annual oil production for each county
moil_county <- summaryBy(oil~county, data=dframe, FUN = mean)

# mean annual gas production for each county
mgas_county <- summaryBy(gas~county, data=dframe, FUN = mean)


# nationalwide mean production (per county) in each year to compare nationwide oil and gas production
# nationalwide oil production
moil_year <- colMeans(raw[9:20], na.rm = FALSE, dims =1L)
moil_year <- data_frame(year=c(2000:2011),moil_year=moil_year)

# nationalwide gas production
mgas_year <- colMeans(raw[21:32], na.rm = FALSE, dims =1L)
mgas_year <- data_frame(year=c(2000:2011),mgas_year=mgas_year)

# Merge two tables into one for future visulization
m_year <- merge(moil_year,mgas_year,by="year")
names(m_year)[2] <- "typeoil"
names(m_year)[3] <- "typegas"
m_year <- m_year %>%
  gather(type,amount,typeoil:typegas) %>%
  mutate(type = gsub("type","",type)) %>%
  arrange(year, type)


# Form a new table including population level to compare oil and gas prodution between different county levels
# Sum all production through 2000 to 2011 for each county
raw$oil_production <- rowSums(raw[9:20])
raw$gas_production <- rowSums(raw[21:32])

# Create a table with county, county level defined by population, sum of oil production in 12 years and sum of gas production in 12 years
county_production <- raw %>% 
  select(FIPS,Population_Level, oil_production, gas_production) %>%
  unique() %>%
  mutate(county_id = row_number())
county_production

# average total oil and gas production in 12 years for counties by each population level 
dframe2 = data.frame(countylevel = county_production$Population_Level, oil=county_production$oil_production, gas=county_production$gas_production)
# mean total oil prouction in 12 years by each county level
soil_level <- summaryBy(oil~countylevel, data=dframe2, FUN = mean)
# mean total gas production in 12 years by each county level 
sgas_level <- summaryBy(gas~countylevel, data=dframe2, FUN = mean)



# mean oil and gas production for counties by each population level in each year
County_Level_Mean <- raw %>% 
  select(Population_Level, oil2000,oil2001,oil2002,oil2003,oil2004,oil2005,oil2006,oil2007,oil2008,oil2009,oil2010,oil2011,gas2000,gas2001,gas2002,gas2003,gas2004,gas2005,gas2006,gas2007,gas2008,gas2009,gas2010,gas2011) %>%
  mutate(county_id = row_number())
County_Level_Mean <- aggregate(County_Level_Mean[,2:25],list(County_Level_Mean$Population_Level), mean)
names(County_Level_Mean)[1] <- paste("County_Level")
County_Level_Mean

County_Level_Mean_tidyoil <- County_Level_Mean %>%
  gather(oil,oil_production_mean, oil2000:oil2011, na.rm = TRUE)
County_Level_Mean_tidyoil

County_Level_Mean_tidygas <- County_Level_Mean_tidyoil %>%
  gather(gas, gas_production_mean, gas2000:gas2011, na.rm = TRUE)
County_Level_Mean_tidygas

# Extract the year information from "oil" or "gas" column (eg. oil2001) and created a "year" column
County_Level_Mean_omit <- County_Level_Mean_tidygas %>%
  mutate(
    year = parse_number(oil)
  )

# Omit "oil" and "gas" columns by selecting the rest information and create a new data table called "oil gas"
County_Level_Mean_Final <- County_Level_Mean_omit %>% 
  select(County_Level, year, oil_production_mean,gas_production_mean) %>%
  unique() %>%
  mutate()
County_Level_Mean_Final

# mean oil for counties by each population level in each year
moil_countylevel <- raw %>% 
  select(Population_Level, oil2000,oil2001,oil2002,oil2003,oil2004,oil2005,oil2006,oil2007,oil2008,oil2009,oil2010,oil2011) %>%
  mutate(county_id = row_number())
moil_countylevel <- aggregate(moil_countylevel[,2:13],list(moil_countylevel$Population_Level), mean)
names(moil_countylevel)[1] <- paste("County_Level")
colnames(moil_countylevel)[2:13] <- c(2000:2011)
moil_countylevel

# mean gas for counties by each population level in each year
mgas_countylevel <- raw %>% 
  select(Population_Level, gas2000,gas2001,gas2002,gas2003,gas2004,gas2005,gas2006,gas2007,gas2008,gas2009,gas2010,gas2011) %>%
  mutate(county_id = row_number())
mgas_countylevel <- aggregate(mgas_countylevel[,2:13],list(mgas_countylevel$Population_Level), mean)
names(mgas_countylevel)[1] <- paste("County_Level")
colnames(mgas_countylevel)[2:13] <- c(2000:2011)
mgas_countylevel


require(ggplot2) 

# 1
# ggplot for "Relationship between Oil Production and Gas Production from 2000-2011" 
ggplot(data=m_year, aes(x=factor(year), y=amount, fill=type)) +
  geom_bar(stat="identity", position =position_dodge())+
  xlab("Year")+
  ylab("Production")+
  ggtitle("Oil Production vs. Gas Production")+
  scale_y_continuous(breaks=seq(0, 8000000, 1000000))

# 2
# ggplot for "Oil Production for each County Level"
family <- as.factor(dframe2$countylevel)
ggplot(data = dframe2, aes(factor(countylevel),oil,fill:="red") +
  geom_bar(stat = "identity")+
  xlab("County Level")+
  ylab("Mean Oil Production")+
  scale_y_continuous(breaks=seq(0, 12000000000, 500000000))+
  ggtitle("Oil Production By County Level"))



# ggplot for "Gas Production for each County Level"
ggplot(data = dframe2, aes(factor(countylevel),gas,fill=~factor(am)))+
  geom_bar(stat = "identity") +
  xlab("County Level")+
  ylab("Mean Gas Production")+
  scale_y_continuous(breaks=seq(0, 120000000000, 5000000000))+
  ggtitle("Gas Production By County Level")
# ggplot for "Oil Production for each County Level"
ggplot(data = dframe2, aes(countylevel,gas))+
  geom_bar(stat = "identity")

# 3
ggplot(data = County_Level_Mean_Final, aes(x = factor(year), y = oil_production_mean, colour = County_Level)) +       
  xlab("Year")+
  ylab("Mean Oil Production")+
  ggtitle("Oil Production By Year")+
  geom_line(aes(group = County_Level)) + geom_point()+
  scale_colour_gradient(low = 'lightblue', high = 'darkblue', breaks=c(1:9))

