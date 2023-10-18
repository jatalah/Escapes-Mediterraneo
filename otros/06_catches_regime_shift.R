library(tidyverse)
library(rshift)
theme_set(theme_minimal())
rm(list = ls())

catch <-
  read_csv('data/landings.csv') %>%
  group_by(Species) %>%
  mutate(
    ton_mean = mean(Tonnes, na.rm = TRUE),
    ton_sd = sd(Tonnes, na.rm = TRUE),
    ton_anom = (Tonnes - ton_mean) / ton_sd
  ) %>%
  ungroup() %>% 
  # select(Species, Year, ton_anom) %>% 
  write_csv('data/regime_shift_data_catch.csv')
  
bream <- catch %>% filter(Species  == "Sparus aurata") 

# write_csv('data/bream_catch_RSI.csv')
bass <- catch %>% filter(Species == "Dicentrarchus labrax") 

RSI_bream <- 
  Rodionov(data = bream, col = "ton_anom", time = "Year", l = 5, merge = TRUE, prob = 0.95) %>% 
  mutate(RSI_cum = cumsum(RSI),
         mean_RIS = mean(RSI_cum))

ggplot(RSI_bream) +
  geom_line(aes(Year, RSI_cum - mean_RIS)) +
  geom_line(aes(Year, ton_anom)) 


RSI_bass <- 
  Rodionov(data = bass, col = "ton_anom", time = "Year", l = 5, merge = TRUE, prob = 0.95) %>% 
  mutate(RSI_cum = cumsum(RSI),
         mean_RIS = mean(RSI_cum))

ggplot(RSI_bass) +
  geom_line(aes(Year, RSI_cum - mean_RIS)) +
  geom_line(aes(Year, ton_anom)) 

RSI_all <- 
  bind_rows(RSI_bream, RSI_bass) %>% 
  pivot_longer(cols = c(ton_anom   , RSI_cum))

labs <- as_labeller(c(RSI_cum = "A. Regime Shift Index", Tonnes = "B. Catch (tonnes)"))

ggplot(RSI_all) +
  geom_line(aes(Year, value, color = Species)) +
  facet_wrap(~name, nrow = 2, scales = 'free', labeller = labs) +
  scale_color_discrete(name = NULL) +
  theme(legend.text = element_text(face = 'italic'),
        legend.position = c(.2,.9))+ 
  xlab(label = NULL) +
  ylab(label = NULL)

# ggsave(plot = ggplot2::last_plot(),
#        filename = 'figures/catch_rsi.png',
#        device = 'png',
#        width = 5,
#        height = 3,
#        bg = 'white')


