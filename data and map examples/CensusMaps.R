#install packages (if necessary)
#install.packages(c("tidycensus", "tidyverse", "sf", "tigris", "tmap"))

#load libraries

library(tidycensus)
library(tidyverse)
library(sf)
library(tigris)
library(tmap)

## Decennial population state level

pop20 <- get_decennial(
  geography = "state",
  variables = "P1_001N",
  geometry = TRUE,
  year = 2020)


pop20 <- get_decennial(
  geography = "state",
  variables = "P1_001N",
  geometry = TRUE,
  year = 2020) %>% 
  shift_geometry()  ## what does shift geometry do?


## detailed map of 2020 population

ggplot() +
  geom_sf(data = pop20, aes(fill = value)) +   
  scale_fill_continuous(type = "viridis", direction = -1) +
  labs(title = "Population by State, 2020") + 
  theme(plot.title = element_text (hjust = 0.5))


## MD decennial population by county

md_pop_2020 <- get_decennial(
  geography = "county",
  variables = "P1_001N",
  geometry = TRUE,
  state = "MD",
  year = 2020) %>% 
  shift_geometry()


## detailed map of 2020 MD population

ggplot() +
  geom_sf(data = md_pop_2020, aes(fill = value)) +   
  scale_fill_continuous(type = "viridis", direction = -1) +
  labs(title = "Population in MD, 2020") + 
  theme(plot.title = element_text (hjust = 0.5))


## US Decennial population by County

county_pop_2020 <- get_decennial(
  geography = "county",
  variables = "P1_001N",
  geometry = TRUE,
  year = 2020
) %>% 
  shift_geometry(position = "outside")


## map of 2020 county population

ggplot() +
  geom_sf(data = county_pop_2020, aes(fill = value)) +   
  scale_fill_gradient(low = "light yellow", high = "dark green", 
                      na.value = NA) +
  labs(title = "Population by County, 2020") + 
  theme(plot.title = element_text (hjust = 0.5))


## mapping the highest states in each GQ category

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
  shift_geometry() %>%
  mutate(CorrProp = (Correctional/Population)*100,  #calculate proportions
         JuvenileProp = (Juvenile/Population)*100,
         NursingProp = (Nursing/Population)*100,
         OtherProp = (Other/Population)*100) %>%
  select(NAME, CorrProp:OtherProp) %>%              
  pivot_longer(cols = CorrProp:OtherProp,           #wide to long
               names_to = "Type",
               values_to = "Value")


HighestProp <- gq_prop_2020 %>% 
  group_by(Type) %>%
  mutate(the_rank  = rank(-Value, ties.method = "random")) %>%
  filter(the_rank == 1) %>% 
  select(-the_rank)


PropSF <- pop20 %>% 
  select(NAME, geometry, variable, value) %>%
  rename(Type = variable, Value = value) %>%
  filter(!NAME %in% c('Louisiana', 'Montana', 'North Dakota', 'Kansas')) %>%
  rbind(HighestProp) %>%
  mutate(Value = ifelse(Value > 100,0, Value))


## basic map of the highest states in each GQ category

ggplot(PropSF, aes(fill = Value)) +   # a start
  geom_sf()+ 
  scale_fill_continuous(high = "#556B2F", low = "#97FFFF")


## detailed map of the highest states in each GQ category

ggplot(data = PropSF, aes(fill = Value)) + 
  geom_sf() + 
  scale_fill_distiller(palette = "Spectral",  
                       limits = c(min(PropSF$Value), max(PropSF$Value))) + 
  labs(title = "Highest Group Quarter Populations, 2020",
       caption = "Data source: 2020 US Census Bureau",
       fill = "Prop") +
  theme_void() + 
  theme(plot.title = element_text (hjust = 0.5))


## map locations of places with significant GQ proportion

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
         OtherProp = (Other/Population)*100) %>% 
  shift_geometry()


place_gq_2020_pop <- place_gq_2020 %>%
  filter(CorrProp > 10 | JuvenileProp > 10 | NursingProp > 10 | OtherProp > 10)

place_gq_2020_pop_all <- place_gq_2020_pop %>%
  select(NAME, CorrProp:OtherProp) %>%
  pivot_longer(cols = CorrProp:OtherProp,
               names_to = "Type",
               values_to = "Value") %>%
  group_by(Type) %>%
  filter(Value > 0)


ggplot() +
  geom_sf(data = pop20, aes(fill = value, geometry = geometry)) +
  geom_sf(data = place_gq_2020_pop_all, aes(colour = Type, 
                                            geometry = geometry))+ 
  labs(title = "State Population and Group Quarters by Place, 2020",
       caption = "Data source: 2020 Decennial Data, US Census Bureau",
       fill = "Population", colour = "GQ Type") + 
  theme_dark() +
  theme(plot.title = element_text(hjust = 0.5))
