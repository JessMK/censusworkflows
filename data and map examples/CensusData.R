#install packages (if necessary)
#install.packages(c("tidycensus", "tidyverse", "sf", "tigris"))

#load libraries

library(tidycensus)
library(tidyverse)
library(sf)
library(tigris)

## Use this code to get Census data from Census.gov with the API

## load your api key

# Get a Census API key at https://api.census.gov/data/key_signup.html 

#census_api_key("INSERT YOUR API KEY HERE", install = TRUE)


## disable scientific notation

options(scipen = 999)


## search available variables, for Decennial and ACS

decennial_vars_20 <- load_variables(2020, "pl")

View(decennial_vars_20)


acs_detailed_22 <- load_variables(2022, "acs5")

View(acs_detailed_22)


## Decennial population, at the state level

pop20 <- get_decennial(
  geography = "state",
  variables = "P1_001N",
  geometry = TRUE,
  year = 2020) 


## view and explore Decennial population

pop20

table(pop20$NAME)

glimpse(pop20)

str(pop20)


## US Decennial population, by county

county_pop_2020 <- get_decennial(
  geography = "county",
  variables = "P1_001N",
  geometry = TRUE,
  year = 2020) 


## view US Decennial population, by county

head(county_pop_2020)

glimpse(county_pop_2020)

str(county_pop_2020)


## MD Decennial population, by county

md_pop_2020 <- get_decennial(
  geography = "county",
  variables = "P1_001N",
  geometry = TRUE,
  state = "MD",
  year = 2020)


## view MD county population

table(md_pop_2020$NAME)

glimpse(md_pop_2020)

str(md_pop_2020)


## ACS examples

median_income_1yr_state <- get_acs(
  geography = "state",
  variables = "B19013_001",
  year = 2022,
  survey = "acs1")

head(median_income_1yr_state)


median_income_1yr_county <- get_acs(
  geography = "county",
  variables = "B19013_001",
  year = 2022,
  survey = "acs1")

str(median_income_1yr_county)


md_education <- get_acs(
  geography = "county",
  state = "MD",
  variables = c(percent_high_school = "DP02_0062P",
                percent_bachelors = "DP02_0065P",
                percent_graduate = "DP02_0066P"),
  year = 2022)

str(md_education)

View(md_education)


## group quarters institutionalized population data, by State

gq_pop_2020 <- get_decennial(
  geography = "state",
  geometry = TRUE,
  output = "wide",
  year = 2020,
  variables = c(Population = "P1_001N",
                Correctional = "P5_003N",
                Juvenile = "P5_004N",
                Nursing = "P5_005N",
                Other = "P5_006N")) %>%
  select(NAME, Population:Other) %>%   
  pivot_longer(cols = Population:Other,            #wide to long
               names_to = "Type",
               values_to = "Value")


head(gq_pop_2020)

glimpse(gq_pop_2020)


## group quarters institutionalized proportion, by State

gq_prop_2020 <- get_decennial(
  geography = "state",
  geometry = TRUE,
  output = "wide",
  year = 2020,
  variables = c(Population = "P1_001N",
                Correctional = "P5_003N",
                Juvenile = "P5_004N",
                Nursing = "P5_005N",
                Other = "P5_006N")) %>% 
  mutate(CorrProp = (Correctional/Population)*100,  #calculate proportions
         JuvenileProp = (Juvenile/Population)*100,
         NursingProp = (Nursing/Population)*100,
         OtherProp = (Other/Population)*100) %>%
  select(NAME, CorrProp:OtherProp) %>%              
  pivot_longer(cols = CorrProp:OtherProp,           #wide to long
               names_to = "Type",
               values_to = "Value")

head(gq_prop_2020)

glimpse(gq_prop_2020)


## visualize your output, custom color palette

cbPalette <- c("#E69F00", 
               "#009E73", 
               "#F0E442", 
               "#0072B2", 
               "#56B4E9", 
               "#D55E00", 
               "#CC79A7")


## group quarters compared to population

ggplot(gq_pop_2020) +
  geom_bar(mapping = aes(x = reorder(NAME,Value), 
                         y = Value, 
                         fill = Type), 
           stat = 'identity') +
  scale_fill_manual(values=cbPalette) +
  labs(title = "Group Quarters and Population by State, 2020",
       x = "State",
       y = "Population") + 
  theme(plot.title = element_text (hjust = 0.5)) +
  coord_flip()


## group quarters by state

ggplot(gq_prop_2020) +
  geom_bar(mapping = aes(x = reorder(NAME,Value), 
                         y = Value, 
                         fill = Type), 
           stat = 'identity') +
  scale_fill_manual(values=cbPalette) +
  labs(title = "Group Quarters Population by State, 2020",
       x = "State",
       y = "Proportion") + 
  theme(plot.title = element_text (hjust = 0.5)) +
  coord_flip()


## who has the highest GQ pop and proportion by GQ type

# GQ population

HighestPop <- gq_pop_2020 %>% 
  group_by(Type) %>%
  mutate(the_rank  = rank(-Value, ties.method = "random")) %>%
  filter(the_rank == 1) %>% 
  select(-the_rank)

HighestPop$NAME


# GQ proportion

HighestProp <- gq_prop_2020 %>% 
  group_by(Type) %>%
  mutate(the_rank  = rank(-Value, ties.method = "random")) %>%
  filter(the_rank == 1) %>% 
  select(-the_rank)

HighestProp$NAME


## joining the highest states in each GQ category

PropSF <- pop20 %>% 
  select(NAME, geometry, variable, value) %>%
  rename(Type = variable, Value = value) %>%
  filter(!NAME %in% c('Louisiana', 'Montana', 'North Dakota', 'Kansas')) %>%
  rbind(HighestProp) %>%
  mutate(Value = ifelse(Value > 100,0, Value))


## county with GQ

county_gq_2020 <- get_decennial(
  geography = "county",
  geometry = TRUE,
  year = 2020,
  output = "wide",
  variables = c(Population = "P1_001N",
                Correctional = "P5_003N",
                Juvenile = "P5_004N",
                Nursing = "P5_005N",
                Other = "P5_006N")
) %>% 
  mutate(CorrProp = (Correctional/Population)*100,
         JuvenileProp = (Juvenile/Population)*100,
         NursingProp = (Nursing/Population)*100,
         OtherProp = (Other/Population)*100)

str(county_gq_2020)


## places with GQ

place_gq_2020 <- get_decennial(
  geography = "place",
  geometry = TRUE,
  year = 2020,
  output = "wide",
  variables = c(Population = "P1_001N",
                Correctional = "P5_003N",
                Juvenile = "P5_004N",
                Nursing = "P5_005N",
                Other = "P5_006N")
) %>% 
  mutate(CorrProp = (Correctional/Population)*100,
         JuvenileProp = (Juvenile/Population)*100,
         NursingProp = (Nursing/Population)*100,
         OtherProp = (Other/Population)*100)

str(place_gq_2020)


## ranges of values in each GQ category

range(place_gq_2020$CorrProp[place_gq_2020$CorrProp>0], na.rm = TRUE)

range(place_gq_2020$JuvenileProp[place_gq_2020$JuvenileProp>0], na.rm = TRUE)

range(place_gq_2020$NursingProp[place_gq_2020$NursingProp>0], na.rm = TRUE)

range(place_gq_2020$OtherProp[place_gq_2020$OtherProp>0], na.rm = TRUE)


# keep places with significant population

place_gq_2020_pop <- place_gq_2020 %>%
  filter(CorrProp > 10 | JuvenileProp > 10 | NursingProp > 10 | OtherProp > 10)


# pivot data

place_gq_2020_pop_long <- place_gq_2020_pop %>%
  select(NAME, CorrProp:OtherProp) %>%
  pivot_longer(cols = CorrProp:OtherProp,
               names_to = "Type",
               values_to = "Value") %>%
  group_by(Type) %>%
  top_n(15, Value) %>%
  filter(Value > 0)


# create bar chart of proportion by place, faceted by type

ggplot(place_gq_2020_pop_long, aes(x = reorder(NAME, Value),
                                   y = Value, fill = Type)) +
  geom_col() +
  labs(title = "GQ Proportion by State, 2020",
       x = "State",
       y = "Proportion",
       fill = "Type") +
  theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5, size = 6))+
  theme(plot.title = element_text(hjust = 0.5)) + 
  coord_flip() + facet_grid(Type ~ ., scale = "free")


##  gather place locations of places with group quarters

place_gq_2020_pop_all <- place_gq_2020_pop %>%
  select(NAME, CorrProp:OtherProp) %>%
  pivot_longer(cols = CorrProp:OtherProp,
               names_to = "Type",
               values_to = "Value") %>%
  group_by(Type) %>%
  filter(Value > 0)

