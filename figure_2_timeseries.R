library(tidyverse)
library(ggpubr)
rm(list = ls())

# read landing and escapes data-----------
d <- bind_rows("Fisheries landings" = read_csv('data/landings.csv'), "Escapes" = read_csv('data/escapes.csv'), .id = "Type")


# Plot escapes vs landings------
ann_t <-
  tibble(
    Species = c("Sparus aurata", "Dicentrarchus labrax"),
    Year = c(1960, 1960),
    Tonnes = c(6e3, 6e3),
    lab = c("F = 12.7, p < 0.001", "F = 0.1, p > 0.05")
  )

esc_fish_plot <- 
  ggplot(d, aes(Year, Tonnes)) +
  geom_line(aes(color = Type)) +
  geom_text(data = ann_t, aes(Year, Tonnes, label = lab), size = 1.8) +
  facet_wrap(~Species, scales = 'fixed', nrow = 1) +
  theme_bw(base_size = 8) +
  theme(legend.position = c(.1,.8),
        strip.text = element_text(face = 'italic', size = 8),
        legend.key.size = unit(0.5, 'lines'),
        legend.text=element_text(size= 5),
        panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(color = "gray50"),
        axis.ticks = element_line(color = "gray50")) +
  scale_color_manual(values = c( rgb(0.2, 0.6, 0.9, 1), 'red2'), name = NULL) +
  labs(x = NULL)

esc_fish_plot

# read LPUE and escapes data-----------
dd <- bind_rows("LPUE" = read_csv('data/lpue_data.csv'), "Escapes" = read_csv('data/escapes.csv'), .id = "Type") %>% 
  drop_na()


## Plot with double axis-------
colors <- c("Escapes" = rgb(0.2, 0.6, 0.9, 1), "LPUE" = "#69b3a2")

ann_text <-
  tibble(
    Species = c("Sparus aurata", "Dicentrarchus labrax"),
    Year = c(1978, 1978),
    Tonnes = c(7e3, 7e3),
    lab = c("F = 15.5, p < 0.001", "F = 1.0, p > 0.05")
  )

esc_lpue_plot <- 
  dd %>%
  pivot_wider(names_from = Type, values_from = Tonnes) %>%
  ggplot() +
  geom_line(aes(Year, LPUE * 1e5, color = "LPUE")) +
  geom_line(aes(Year, Escapes, color = "Escapes")) +
  geom_text(data = ann_text, aes(Year, Tonnes, label = lab), size = 1.8) +
  scale_y_continuous(name = "Tonnes",
                     sec.axis = sec_axis( ~ . / 1e5, name = "LPUE (tonnes per vessel)")) +
  facet_wrap( ~ Species, nrow = 1) +
  labs(x = NULL, y = "Tonnes") +
  theme_bw(base_size = 8) +
  theme(strip.text = element_text(face = 'italic', size = 8),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 15), size = 8),
        legend.position = c(.1, .8),
        legend.key.size = unit(0.5, 'lines'),
        legend.text=element_text(size= 5),
        panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(color = "gray50"),
        axis.ticks = element_line(color = "gray50")) +
  scale_color_manual(values = colors, name = NULL) 
  


figure_2 <-
  ggarrange(esc_fish_plot,
            esc_lpue_plot,
            nrow = 2,
            labels = 'auto', font.label = list(size = 8, face = "plain"))


figure_2

ggsave(plot =figure_2,
       filename = 'figures/figure_2_timeseries.png',
       device = 'png',
       width = 7,
       height = 6,
       bg = 'white')

ggsave(plot =figure_2,
       filename = 'figures/figure_2_timeseries.pdf',
       device = 'pdf',
       units = "mm",
       width = 160,
       height = 75,
       dpi = 900)
