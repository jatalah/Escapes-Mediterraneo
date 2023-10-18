d <- bind_rows("Fisheries landings" = read_csv('data/landings.csv'), "Aquaculture" = read_csv('data/aqua.csv'), .id = "Type")

figure_1 <- 
ggplot(d, aes(Year, Tonnes, color = Type)) +
  geom_line() +
  facet_wrap(~Species, scales = 'fixed') +
  theme(legend.position = c(.15,.8),
        strip.text = element_text(face = 'italic')) +
  labs(x = NULL) +
  scale_color_discrete(name = NULL)

figure_1

ggsave(plot = figure_1,
       filename = 'figures/figure_1_aqua_vs_fisheries.png',
       device = 'png',
       width = 7,
       height = 3,
       bg = 'white')
