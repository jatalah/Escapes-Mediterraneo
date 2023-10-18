library(tidyverse)
theme_set(theme_minimal())
rm(list = ls())


# read fleet register data------
fleet <- 
  readxl::read_excel('data/FleetperCountry.xlsx') %>% 
  select(Year, `Fleet 6 countries`) %>% 
  rename(Fleet = `Fleet 6 countries`)

ggplot(fleet_new, aes(Year, Fleet)) +
  geom_line()


# calculate and save lpue data -----
# read and filter capture data -----
cap_six <- 
  read_csv('C:/Users/javiera/OneDrive - Cawthron/Stats/data/FAO captures/capture_fao_data.csv') %>% 
  filter(Area_name == "Mediterranean and Black Sea") %>% 
  filter(Scientific_Name  == "Sparus aurata" | Scientific_Name  == "Dicentrarchus labrax") %>% 
  filter(country  %in% c("Spain", "France", "Slovenia", "Greece", "Malta", "Cyprus")) %>% 
  write_csv('data/landings_six.csv')

bass_six <-
  cap_six %>% 
  filter(Scientific_Name == "Dicentrarchus labrax") %>%
  group_by(Year) %>%
  summarise(Tonnes = sum(VALUE, na.rm = T))

bream_six <-
  cap_six %>% 
  filter(Scientific_Name == "Sparus aurata") %>%
  group_by(Year) %>%
  summarise(Tonnes = sum(VALUE, na.rm = T))

landings_six <- 
  bind_rows("Sparus aurata" = bream_six, "Dicentrarchus labrax" = bass_six, .id = "Species")

lpue <-
  landings_six %>%
  left_join(fleet) %>%
  mutate(Tonnes = Tonnes / Fleet) %>%
  select(-Fleet) %>%
  write_csv('data/lpue_data.csv')

