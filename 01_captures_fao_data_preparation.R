library(tidyverse)
theme_set(theme_minimal())
rm(list = ls())


# read and filter capture data -----
cap <- 
  read_csv('C:/Users/javiera/OneDrive - Cawthron/Stats/data/FAO captures/capture_fao_data.csv') %>% 
  filter(Area_name == "Mediterranean and Black Sea") %>% 
  filter(Scientific_Name  == "Sparus aurata" | Scientific_Name  == "Dicentrarchus labrax") %>%
  filter(country  != "Italy")

ggplot(cap, aes(Year, VALUE, color = Scientific_Name)) +
  geom_line() +
  facet_wrap(~country)


bass <-
  cap %>% 
  filter(Scientific_Name == "Dicentrarchus labrax") %>%
  group_by(Year) %>%
  summarise(Tonnes = sum(VALUE, na.rm = T))
         
bream <-
  cap %>% 
  filter(Scientific_Name == "Sparus aurata") %>%
  group_by(Year) %>%
  summarise(Tonnes = sum(VALUE, na.rm = T))

landings <- 
  bind_rows("Sparus aurata" = bream, "Dicentrarchus labrax" = bass, .id = "Species") %>% 
  write_csv('data/landings.csv')
  

ggplot(landings, aes(Year, Tonnes, color = Species)) +
  geom_line() 


# Italian data --------
cap_italy <- 
  read_csv('C:/Users/javiera/OneDrive - Cawthron/Stats/data/FAO captures/capture_fao_data.csv') %>% 
  filter(Area_name == "Mediterranean and Black Sea") %>% 
  filter(Scientific_Name  == "Sparus aurata" | Scientific_Name  == "Dicentrarchus labrax") %>% 
  filter(country  == "Italy")

ggplot(cap_italy, aes(Year, VALUE, color = Scientific_Name)) +
  geom_line() 

