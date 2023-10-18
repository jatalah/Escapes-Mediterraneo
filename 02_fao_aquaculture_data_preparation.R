library(tidyverse)
theme_set(theme_minimal())
rm(list = ls())

# production data ---------------
aqua_all <-
  read_rds('C:/Users/javiera/OneDrive - Cawthron/Stats/data/FAO aqua/production_data_all.RDS') %>% 
  filter(Scientific_Name  == "Sparus aurata" | Scientific_Name  == "Dicentrarchus labrax") %>% 
  filter(Area_name =="Mediterranean and Black Sea") %>% 
  filter(ENVIRONMENT.ALPHA_2_CODE == "MA") %>% 
  filter(country_name != "Italy") 


read_rds('C:/Users/javiera/OneDrive - Cawthron/Stats/data/FAO aqua/production_data_all.RDS') %>%
  filter(Scientific_Name  == "Sparus aurata" |
           Scientific_Name  == "Dicentrarchus labrax") %>%
  filter(Area_name == "Mediterranean and Black Sea") %>%
  filter(ENVIRONMENT.ALPHA_2_CODE == "MA") %>% 
  ggplot(aes(Year, VALUE , color = Scientific_Name)) +
  geom_line() +
  facet_wrap( ~ country_name)


aqua <- 
  aqua_all %>% 
  # filter(country_name %in% c("Spain", "France", "Slovenia", "Greece", "Malta", "Cyprus")) %>%
  rename(Tonnes = VALUE,
         Species = Scientific_Name) %>% 
  group_by(Year, Species) %>%
  summarise(Tonnes = sum(Tonnes, na.rm = T)) %>% 
  write_csv('data/aqua.csv')


ggplot(aqua, aes(Year, Tonnes, color = Species)) +
  geom_line()

aqua_six <- 
  aqua_all %>% 
  filter(country_name %in% c("Spain", "France", "Slovenia", "Greece", "Malta", "Cyprus")) %>%
  rename(Tonnes = VALUE,
         Species = Scientific_Name) %>% 
  group_by(Year, Species) %>%
  summarise(Tonnes = sum(Tonnes, na.rm = T))



ggplot(aqua_six, aes(Year, Tonnes, color = Species)) +
  geom_line()

# estimate escapes at 5% of aquaculture production----
escapes <- 
  aqua%>% 
  mutate(Tonnes = Tonnes/20) %>% 
  write_csv('data/escapes.csv')

escapes_six <- 
  aqua_six %>% 
  mutate(Tonnes = Tonnes/20) %>% 
  write_csv('data/escapes_six.csv')


d <- bind_rows("Fisheries" = read_csv('data/landings.csv'), "Escapes" = escapes, .id = "Type")


ggplot(d, aes(Year, Tonnes, color = Type)) +
  geom_line() +
  facet_wrap(~Species, scales = 'fixed')

# Granger test of causality -------------
library(lmtest)
bream <- 
d %>% 
  filter(Species == "Sparus aurata") %>% 
  pivot_wider(names_from = Type, values_from = Tonnes)

grangertest(Fisheries~Escapes, order = 2, data = bream)

bass <- 
  d %>% 
  filter(Species == "Dicentrarchus labrax") %>% 
  pivot_wider(names_from = Type, values_from = Tonnes)

grangertest(Fisheries~Escapes, order = 2, data = bass)

