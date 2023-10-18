library(tidyverse)

ddd <-
  bind_rows(
    "LPUE" = read_csv('data/lpue_data.csv'),
    "Escapes" = read_csv('data/escapes.csv'),
    .id = "Type",
    "Fisheries" = read_csv('data/landings.csv')
  ) %>%
  drop_na()

colors <-
  c(
    "Escapes" = rgb(0.2, 0.6, 0.9, 1),
    "LPUE" = "#69b3a2",
    'Fisheries' = "#e85129"
  )
ddd %>%
  pivot_wider(names_from = Type, values_from = Tonnes) %>%
  ggplot() +
  geom_line(aes(Year, LPUE * 1e5, color = "LPUE")) +
  geom_line(aes(Year, Escapes, color = "Escapes")) +
  geom_line(aes(Year, Fisheries, color = "Fisheries")) +
  theme(legend.position = c(.1, .8),
        legend.spacing = unit(.01, 'mm')) +
  labs(x = NULL, y = "Tonnes") +
  scale_y_continuous(name = "Tonnes",
                     sec.axis = sec_axis(~ . / 1e5, name = "LPUE (tonnes per vessel)")) +
  facet_wrap(~ Species, nrow = 1) +
  theme(
    strip.text = element_text(face = 'italic'),
    axis.title.y = element_text(margin = margin(
      t = 0,
      r = 10,
      b = 0,
      l = 0
    )),
    axis.title.y.right = element_text(margin = margin(
      t = 0,
      r = 0,
      b = 0,
      l = 15
    ))
  ) +
  scale_color_manual(values = colors, name = NULL)
