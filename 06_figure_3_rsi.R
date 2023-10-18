library(tidyverse)
library(lmtest)
theme_set(theme_minimal())
rm(list = ls())
### Read results from excel Radionov---

# Catches---
res_all <-
  bind_rows(
    Seabream = readxl::read_xlsx('data/regime_shift_results_catch.xlsx', sheet = 'ton_anom_seabream'),
    Seabass = readxl::read_xlsx('data/regime_shift_results_catch.xlsx', sheet = 'ton_anom_seabass'),
    .id = "Species"
  ) %>%
  group_by(Species) %>%
  mutate(
    cum_RIS = cumsum(RSI),
    mean_RSI = mean(cum_RIS),
    anom_RSI = cum_RIS - mean_RSI
  ) %>%
  ungroup() %>% 
  select(-contains("..."))


## LPUE### -----
res_all_lpue <-
  bind_rows(
    Seabream = readxl::read_xlsx('data/lpue_regime_shift_results.xlsx', sheet = 'ton_anom_seabream'),
    Seabass = readxl::read_xlsx('data/lpue_regime_shift_results.xlsx', sheet = 'ton_anom_seabass'),
    .id = "Species"
  ) %>%
  group_by(Species) %>%
  mutate(
    cum_RIS = cumsum(RSI),
    mean_RSI = mean(cum_RIS, na.rm = T),
    anom_RSI = cum_RIS - mean_RSI
  ) %>%
  ungroup() %>% 
  select(-contains("..."))


## Todas juntas----
figure_3 <- 
bind_rows("Landings" = res_all, "LPUE" = res_all_lpue, .id = 'Type') %>%
  mutate(Species = fct_recode(Species, `Sparus aurata` = "Seabream", `Dicentrarchus labrax` = "Seabass")) %>% 
  ggplot() +
  geom_line(aes(Year, y = anom_RSI, color = Type), lty = 2) +
  geom_line(aes(Year, ton_anom, color = Type)) +
  scale_color_discrete(name = NULL) +
  theme(legend.position = c(.1,.9),
        strip.text = element_text(face = 'italic'))+ 
  xlab(label = NULL) +
  ylab(label = 'Standarised anomalies') +
  facet_wrap(~Species)

figure_3
ggsave(plot = figure_3,
       filename = 'figures/figure_3_rsi_plots.png',
       device = 'png',
       width = 7,
       height = 3,
       bg = 'white')

