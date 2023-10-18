library(tidyverse)
library(rshift)
theme_set(theme_minimal())
rm(list = ls())

# LPUE------
lpue <- 
  read_csv('data/lpue_data.csv') %>% 
  group_by(Species) %>%
  mutate(
    ton_mean = mean(Tonnes, na.rm = TRUE),
    ton_sd = sd(Tonnes, na.rm = TRUE),
    ton_anom = (Tonnes - ton_mean) / ton_sd
  ) %>%
  ungroup() %>% 
  drop_na(Tonnes) %>% 
  select(Species, Year, ton_anom) %>% 
  write_csv('data/lpue_regime_shift_data.csv')

bream <- lpue %>% filter(Species  == "Sparus aurata")
bass <- lpue %>% filter(Species == "Dicentrarchus labrax") 

RSI_bream <- 
  Rodionov(data = bream, col = "Tonnes", time = "Year", l = 5, merge = TRUE, prob = 0.95) %>% 
  mutate(RSI_cum = cumsum(RSI))

RSI_bass <- 
  Rodionov(data = bass, col = "Tonnes", time = "Year", l = 5, merge = TRUE, prob = 0.95) %>% 
  mutate(RSI_cum = cumsum(RSI))

RSI_all <- 
  bind_rows(RSI_bream, RSI_bass) %>% 
  pivot_longer(cols = c(Tonnes, RSI_cum))

labs <- as_labeller(c(RSI_cum = "A. Regime Shift Index", Tonnes = "B. LPUE (tonnes per vessel)"))

ggplot(RSI_all) +
  geom_line(aes(Year, value, color = Species)) +
  facet_wrap(~name, nrow = 2, scales = 'free', labeller = labs) +
  scale_color_discrete(name = NULL) +
  theme(legend.text = element_text(face = 'italic'),
        legend.position = c(.15,.9)) + 
  xlab(label = NULL) +
  ylab(label = NULL)

ggsave(plot = ggplot2::last_plot(),
       filename = 'figures/lpue_rsi.png',
       device = 'png',
       width = 5,
       height = 3,
       bg = 'white')


### Read results from excel Radionov---
res_all <-
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


ggplot(res_all) +
  geom_line(aes(Year, y = anom_RSI, color = Species), lty = 2) +
  geom_line(aes(Year, ton_anom, color = Species)) +
  scale_color_discrete(name = NULL) +
  theme(legend.text = element_text(face = 'italic'),
        legend.position = c(.2,.9))+ 
  xlab(label = NULL) +
  ylab(label = 'Standarised fisheries landing anomalies')



ggsave(plot = ggplot2::last_plot(),
       filename = 'figures/catch_rsi.png',
       device = 'png',
       width = 5,
       height = 3,
       bg = 'white')

