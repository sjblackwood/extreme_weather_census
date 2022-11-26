####### CENSUS DATA MANIPULATION (at the tract level; Connecticut used as example)
########## for documentation, see https://docs.google.com/spreadsheets/d/1m47hMk3yb_87AN1ZyEm_TwPDX6h7PHfAPnjSFNX0nXk/edit#gid=1608846354

#### LOADING RELEVANT PACKAGES
library("tigris")
library('devtools')
library('zipcodeR')
library("tidyverse")
library("glue")
library("readxl")
library(ggplot2)
library("writexl")
library(lubridate)
library(haven)
library(censusapi)
library(tidycensus)
library(ggrepel)
library("scales")
library(colortools)



#####################
## GETTING THE DATA
#####################


#### LOADING THE VARIABLES
acs_20_vars <- load_variables(year = 2020, "acs5", cache = TRUE)


#### EXTRACTING ALL RELEVANT VARIABLES
# NOTE: for place of birth, had to use a different variable for Puerto Rico
# others use "PLACE OF BIRTH FOR THE FOREIGN-BORN POPULATION IN THE UNITED STATES"
# Puerto Rico uses "PLACE OF BIRTH BY NATIVITY AND CITIZENSHIP STATUS"
total <- acs_20_vars %>% filter(grepl("B01001_001", acs_20_vars$name))
place_of_birth <- acs_20_vars %>% filter(name %in% c("B05006_048", "B05006_134", "B05006_150", "B05002_010", "B05006_154", "B05006_078", "B05006_091", "B05006_105"))
native <- acs_20_vars %>% filter(grepl("B01001C_001", acs_20_vars$name))
tenure <- acs_20_vars %>% filter(name %in% c("B25003_001", "B25003_002"))
disability <- acs_20_vars %>% filter(grepl("B18101_", acs_20_vars$name))
internet <- acs_20_vars %>% filter(grepl("B28011_", acs_20_vars$name))


#### GETTING THE DATA FOR ALL VARAIBLES OF INTEREST
total_var <- total$name
pob_vars <- place_of_birth$name
nat_vars <- native$name
ten_vars <- tenure$name
disab_vars <- disability$name[as.logical(c(1, grepl("With a disability", disability$label[2:length(disability$label)])))]
int_vars <- c("B28011_001", "B28011_008")
select_vars <- c(total_var, pob_vars, nat_vars, ten_vars, int_vars, disab_vars)
data_of_interest_tract <- get_acs(geography = "tract", variables = select_vars, year = 2020, state = "CT")



######################
## CLEANING THE DATA
######################


data_of_interest_tract <- data_of_interest_tract[, -5]
data_of_interest_tract <- data_of_interest_tract %>%
  spread(variable, estimate) %>%
  filter(B01001_001 != 0) # remove census tracts with population zero
## for examples of census tracts with population zero, see https://www.melissa.com/v2/lookups/mapblock/geoid to verify
## some include: Bradley International Airport (Census Tract 9800.02, Hartford County, Connecticut), Long Island Sound (Census Tract 9900, Fairfield County, Connecticut), etc.

## place of birth
data_of_interest_tract <- data_of_interest_tract %>% 
  mutate(prop.EAsia = B05006_048 / B01001_001) %>%
  mutate(prop.Cuba = B05006_134 / B01001_001) %>%
  mutate(prop.Mexico = B05006_150 / B01001_001) %>%
  mutate(prop.PR = B05002_010 / B01001_001) %>%
  mutate(prop.SAmer = B05006_154 / B01001_001) %>%
  mutate(prop.WAsia = B05006_078 / B01001_001) %>%
  mutate(prop.SubSahAfr = (B05006_091 - B05006_105) / B01001_001) %>%
  select(-(all_of(pob_vars)))
## Native Americans
data_of_interest_tract <- data_of_interest_tract %>%
  mutate(prop.Native = B01001C_001 / B01001_001) %>%
  select(-(all_of(nat_vars)))
## tenure
data_of_interest_tract <- data_of_interest_tract %>% 
  mutate(prop.ten_own = B25003_002 / B25003_001) %>%
  select(-(all_of(ten_vars)))
## disability
data_of_interest_tract <- data_of_interest_tract %>% 
  mutate(prop.disab = (B18101_004 + B18101_007 + B18101_010 + B18101_013 + B18101_016 + B18101_019 + B18101_023 + B18101_026 + B18101_029 + B18101_032 + B18101_035 + B18101_038) / B18101_001) %>%
  select(-(all_of(disab_vars)))
## internet subscription
data_of_interest_tract <- data_of_interest_tract %>% 
  mutate(prop.no_int = B28011_008 / B28011_001) %>%
  select(-(all_of(int_vars)))
## remove total population variable
data_of_interest_tract <- data_of_interest_tract %>% 
  select(-(B01001_001))



######################
## PLOTTING THE DATA
######################


hist(data_of_interest_tract$prop.EAsia)
hist(data_of_interest_tract$prop.Cuba)
hist(data_of_interest_tract$prop.Mexico)
hist(data_of_interest_tract$prop.PR)
hist(data_of_interest_tract$prop.SAmer)
hist(data_of_interest_tract$prop.WAsia)
hist(data_of_interest_tract$prop.SubSahAfr)
hist(data_of_interest_tract$prop.Native)
hist(data_of_interest_tract$prop.ten_own)
hist(data_of_interest_tract$prop.disab)
hist(data_of_interest_tract$prop.no_int)



















