library(tidyr)
library(readr)
library(data.table)
library(plyr)
library(dplyr)


# County-level oil and gas production
# Group Member: Mengze Yin, Tianchi Zhang, Yilin Li
# Objective of this project : 
# Discover the relationship between urbanization level and oil and gas production

# Read file "Oilgascounty.csv" (Reference: www.ers.usda.gov)
raw <- read.csv("oilgascounty.csv",stringsAsFactors = FALSE)
raw <- raw[, c("FIPS","geoid","Stabr","County_Name","Rural_Urban_Continuum_Code_2013","Urban_Influence_2013","Metro_Nonmetro_2013","Metro_Micro_Noncore_2013","oil2000","oil2001","oil2002","oil2003","oil2004","oil2005","oil2006","oil2007","oil2008","oil2009","oil2010","oil2011","gas2000","gas2001","gas2002","gas2003","gas2004","gas2005","gas2006","gas2007","gas2008","gas2009","gas2010","gas2011","oil_change_group","gas_change_group","oil_gas_change_group")]

# extract column names in order to revise them 
names(raw)[3] <- "State"
names(raw)[4] <- "County"
names(raw)[5] <- "Population_Level"
names(raw)[6] <- "Urban_Influence"
names(raw)[7] <- "If_Metro"
names(raw)[8] <- "Metro_Micro_Noncore"

raw

# Split the table into two because there are multiple types in one table: County & Oil and Gas Production
# Create two tables

# select the information of County to form a new table 
county <- raw %>% 
  select(FIPS, State, County,Population_Level,Urban_Influence, If_Metro,Metro_Micro_Noncore,oil_change_group,gas_change_group,oil_gas_change_group) %>%
  unique() %>%
  mutate(county_id = row_number())
county

# To reduce repetition and enhance readability, eliminate the word "County" from the County column
county$County <- sub("County","",county$County)
county

# select the information of Oil and Gas Production to form a new table
oil_gas <- raw %>% 
  select(FIPS, oil2000,oil2001,oil2002,oil2003,oil2004,oil2005,oil2006,oil2007,oil2008,oil2009,oil2010,oil2011,gas2000,gas2001,gas2002,gas2003,gas2004,gas2005,gas2006,gas2007,gas2008,gas2009,gas2010,gas2011) %>%
  unique() %>%
  mutate(production_id = row_number())
oil_gas
# Because column headers include values (oil/gas + year, eg: oil2003), we melted the column heads into rows 
# first, melt down the oil info into two columns: oil + oil_production
oil_adjusted <- oil_gas %>% 
  gather(oil,oil_production, oil2000:oil2011, na.rm = TRUE)
oil_adjusted

# second, melt down the gas info into two columns: gas + gas_production
oilgas_adjusted <- oil_adjusted %>%   
  gather(gas, gas_production, gas2000:gas2011, na.rm = TRUE)
oilgas_adjusted

# extract the year information from "oil" or "gas" column (eg. oil2001) and created a "year" column
oil_gas_adjusted <- oilgas_adjusted %>%
  mutate(
    year = parse_number(oil)
  )
oil_gas_adjusted

# omit "oil" and "gas" columns by selecting the rest information and create a new data table called "oil gas"
oilgas <- oil_gas_adjusted %>% 
  select(FIPS, year,oil_production,gas_production) %>%
  unique() %>%
  mutate(id = row_number())
oilgas

# Replace 0's with NA, for the convenience of future data updates
oilgas$oil_production <- replace(oilgas$oil_production,oilgas$oil_production==0,NA)
oilgas$gas_production <- replace(oilgas$gas_production,oilgas$gas_production==0,NA)
head(oilgas,10)

df <- data.table(oilgas)
df %>% group_by(oilgas$FIPS) %>% 
  summarize(mean=mean(dt), sum=sum(dt))
