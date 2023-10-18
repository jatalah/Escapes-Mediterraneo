d %>%
  filter(Type == "Escapes") %>%
  select(-Type) %>%
  mutate(`Escapes 2.5%` = Tonnes / 2,
         `Escapes 1%` = Tonnes / 5) %>%
  select(-Tonnes) %>%
  pivot_longer(cols = `Escapes 2.5%`:`Escapes 1%`,
               names_to = "Type",
               values_to = "Tonnes") %>%
  bind_rows(d) %>%
  mutate(Type = fct_recode(Type, `Escapes 5%` = "Escapes"),
         Type = fct_relevel(Type, "Escapes 5%", after = 2)) %>%
  ggplot(aes(Year, Tonnes, linetype = fct_rev(Type))) +
  geom_line() +
  facet_wrap(~ Species) +
  scale_linetype(name = NULL) + theme(strip.text = element_text(face = 'italic')) +
  labs(x = NULL)
