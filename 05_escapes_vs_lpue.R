library(tidyverse)
library(lmtest)
theme_set(theme_minimal())
rm(list = ls())

# read lpue and escapes data-----------
dd <- bind_rows("LPUE" = read_csv('data/lpue_data.csv'), "Escapes" = read_csv('data/escapes.csv'), .id = "Type") %>% 
  drop_na()


# visualise data ---
ggplot(dd, aes(Year, Tonnes, color = Species)) +
  geom_line() +
  facet_wrap(~Type, scales = 'free', nrow = 2) +
  scale_color_discrete(name = NULL) +
  theme(legend.position = c(.2,.8),
        legend.text = element_text(face = 'italic'),
        legend.spacing = unit(.01, 'mm')) +
 labs(x = NULL)

# Granger test of causality -------------
# seabream-------
bream <- 
  dd %>% 
  filter(Species == "Sparus aurata") %>% 
  drop_na() %>% 
  pivot_wider(names_from = Type, values_from = Tonnes)

gt_bream <- 
grangertest(LPUE~Escapes, order = 1, data = bream) %>% 
  broom::tidy()
gt_bream

# regression plot-----
ggplot(bream, aes(Escapes, LPUE)) +
  geom_point() +
  geom_smooth(se = T, method = lm) +
  ggpubr::stat_cor(aes(label = paste(
    ..rr.label.., gsub("p", "P", ..p.label..), sep = "~`,`~"
  )),
  p.accuracy = 0.001,
  r.accuracy = 0.01)

# seabass test----
bass <- 
  dd %>% 
  filter(Species == "Dicentrarchus labrax") %>% 
  drop_na() %>% 
  pivot_wider(names_from = Type, values_from = Tonnes)

gt_bass <- 
grangertest(LPUE~Escapes, order = 4, data = bass) %>% 
  broom::tidy()


bind_rows('Seabream' = gt_bream, 'Seabass' = gt_bass, .id = "Species") %>% 
  write_csv('data/granger_test_lpue.csv')

ggplot(bass, aes(Escapes, LPUE)) +
  geom_point() +
  geom_smooth(se = T, span = .5)

## Plot with double axis-------
colors <- c("Escapes" = rgb(0.2, 0.6, 0.9, 1), "LPUE" = "#69b3a2")

dd %>%
  pivot_wider(names_from = Type, values_from = Tonnes) %>%
  ggplot() +
  geom_line(aes(Year, LPUE * 1e5, color = "LPUE")) +
  geom_line(aes(Year, Escapes, color = "Escapes")) +
  theme(
    legend.position = c(.1, .8),
    legend.spacing = unit(.01, 'mm')
  ) +
  labs(x = NULL, y = "Tonnes") +
  scale_y_continuous(name = "Escapes (tonnes)",
    sec.axis = sec_axis( ~ . / 1e5, name = "LPUE (tonnes per vessel)")) +
  facet_wrap( ~ Species, nrow = 1) +
  theme(strip.text = element_text(face = 'italic'),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 15))) +
  scale_color_manual(values = colors, name = NULL)
