library(tidyverse)
library(lmtest)
theme_set(theme_minimal())
rm(list = ls())

# read landing and escapes data-----------
d <- bind_rows("Fisheries" = read_csv('data/landings.csv'), "Escapes" = read_csv('data/escapes.csv'), .id = "Type")

d %>% 
  filter(Type == "Escapes") %>%
  group_by(Species) %>% 
  summarise(tot_esc = sum(Tonnes))

# Plot escapes vs landings------
esc_fish_plot <- 
ggplot(d, aes(Year, Tonnes, color = Type)) +
  geom_line() +
  facet_wrap(~Species, scales = 'fixed', nrow = 1) +
  theme(legend.position = c(.1,.8),
        strip.text = element_text(face = 'italic')) +
  scale_color_manual(values = c( rgb(0.2, 0.6, 0.9, 1), 'red2'), name = NULL)

esc_fish_plot

# Granger test of causality -------------

# seabream-----
bream <- 
  d %>% 
  filter(Species == "Sparus aurata") %>% 
  pivot_wider(names_from = Type, values_from = Tonnes) 

grangertest(Fisheries~Escapes, order = 1, data = bream) %>%
  broom::tidy() 


cor(bream$Fisheries, bream$Escapes/5, use = "complete.obs")

# regression plot-----
ggplot(bream, aes(Escapes, Fisheries)) +
  geom_point() +
  geom_smooth(se = T, method = lm) +
  ggpubr::stat_cor(aes(label = paste(
    ..rr.label.., gsub("p", "P", ..p.label..), sep = "~`,`~"
  )),
  p.accuracy = 0.001,
  r.accuracy = 0.01)

# seabass----
bass <- 
  d %>% 
  filter(Species == "Dicentrarchus labrax") %>% 
  pivot_wider(names_from = Type, values_from = Tonnes)

grangertest(Fisheries~Escapes, order = 2, data = bass) %>% 
  broom::tidy() 

# regression plot-----
ggplot(bass, aes(Escapes, Fisheries)) +
  geom_point() +
  geom_smooth(se = T, method = lm) +
  ggpubr::stat_cor(aes(label = paste(
    ..rr.label.., gsub("p", "P", ..p.label..), sep = "~`,`~"
  )),
  p.accuracy = 0.001,
  r.accuracy = 0.01)
