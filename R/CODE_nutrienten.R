# Setup -------------------------------------------------------------------


library(tidyverse)
library(HHSKwkl)
library(glue)
library(colorspace)
library(scales)
library(trend)
library(reactable)
library(patchwork)
library(gt)

rap_jaar <- 2025

fys_chem <- readRDS("data/fys_chem.rds") %>% HHSKwkl::add_jaar()
meetpunten <- readRDS("data/meetpunten.rds")
parameters <- readRDS("data/parameters.rds")

theme_set(hhskthema())

panel_theme_extra <- 
  theme(plot.background = element_rect(fill = prismatic::clr_lighten(blauw_l, 0.90, space = "HSL")),
        legend.background = element_rect(fill = prismatic::clr_lighten(blauw_l, 0.90, space = "HSL")), 
        strip.background = element_rect(fill = prismatic::clr_lighten(blauw_l, 0.90, space = "HSL")),
        plot.title.position = "plot",
        plot.subtitle.position = "plot",
        panel.spacing.x = unit(20, "points"),
        margins = margin_auto(10),
        axis.text.y = element_text(hjust = 0))

# panel_theme_extra <- 
#   theme(plot.background = element_rect(fill = prismatic::clr_lighten(blauw_l, 0.90, space = "HSL")),
#         plot.title.position = "plot",
#         panel.grid.major.x = element_blank(),
#         panel.spacing.x = unit(20, "points"),
#         margins = margin_auto(10))

landgebruik_sel <- c("glastuinbouw", "grasland - agrarisch", "grasland - natuur",
                     "stedelijk", "boezem", "grote plassen", "akkerbouw")
mp_sel <-
  meetpunten %>%
  filter(meetpunttypering == 1, landgebruik %in% landgebruik_sel) %>%
  select(mp, landgebruik) %>%
  mutate(landgebruik = str_remove(landgebruik, " - .*"))
# het onderscheid tussen gras natuur en agrarisch weglaten

grafiek_data <-
  fys_chem %>%
  filter(jaar >= rap_jaar - 9,
         jaar <= rap_jaar,
         parnr %in% c(3, 7)) %>%
  inner_join(mp_sel, by = "mp") %>%
  # soms worden in 1 maand meerdere metingen gedaan, die betreffende meetpunten wegen dan zwaarder dan de rest.
  # Daarom is het beter om eerst het maandgemiddelde te nemen om bias te voorkomen.
  add_maand() %>%
  group_by(mp, parnr, landgebruik, jaar, maand) %>%
  summarise(waarde = mean(waarde)) %>%
  group_by(parnr, landgebruik, jaar) %>%
  summarise(waarde_gem = mean(waarde)) %>%
  ungroup() %>%
  left_join(parameters, by = "parnr") %>%
  # group_by(parnr, landgebruik) %>%
  # mutate(rank = rank(waarde_gem),
  #        tekst = glue("{landgebruik} {jaar}<br>Gemiddeld {format(waarde_gem, digits = 2)} {eenheid}")) %>%
  # ungroup() %>%
  # arrange(parnr, landgebruik, desc(waarde_gem)) %>%
  mutate(#order = row_number(),
         fill_group = ifelse(jaar == rap_jaar, rap_jaar, "Andere jaren"),
         landgebruik = fct_reorder(landgebruik, waarde_gem, .desc = TRUE))

kleuren_functies_nutrienten <-
  c(Akkerbouw              = RColorBrewer::brewer.pal(12, "Set3")[6], # akkerbouw
    Boezem                 = RColorBrewer::brewer.pal(12, "Set3")[5],# boezem
    "Grasland - natuur"    = RColorBrewer::brewer.pal(12, "Set3")[11], #afvoer
    Glastuinbouw           = RColorBrewer::brewer.pal(12, "Set3")[3],
    "Grasland - agrarisch" = RColorBrewer::brewer.pal(12, "Set3")[7],
    Stedelijk              = RColorBrewer::brewer.pal(12, "Set3")[4],
    "Grote plassen"        = RColorBrewer::brewer.pal(12, "Set3")[1],
    "Grasland"             = RColorBrewer::brewer.pal(12, "Set3")[7])


# Nutriënten plot ----------------------------------------------------------


nutrienten_plot <-
  grafiek_data %>%
  filter(jaar == rap_jaar) %>%
  mutate(landgebruik = str_to_sentence(landgebruik)) %>%
  mutate(par_strip = c("3" = "Fosfaat (mg P/l)", "7" = "Stikstof (mg N/l)")[as.character(parnr)]) %>%
  mutate(landgebruik = fct_reorder(landgebruik, .fun = first, waarde_gem, .desc = FALSE)) %>%
  ggplot(aes(waarde_gem, landgebruik)) +
  geom_col(aes(fill = landgebruik)) +
  # geom_col(colour = blauw_d, linewidth = 0.5, fill = grijs_m) +
  facet_wrap(vars(par_strip), scales = "free", strip.position = "top",) +
  scale_x_continuous(limits = c(0, NA), expand = expansion(c(0, 0.1)), labels = scales::label_number(decimal.mark = ",")) +
  scale_fill_manual(values = kleuren_functies_nutrienten) +
  labs(title = "Glastuinbouwgebied heeft dubbel zoveel fosfaat en stikstof",
       subtitle = glue("Gemiddelde concentratie per gebied in {rap_jaar}"),
       y = "",
       x = "") +
  theme(axis.ticks.y = element_blank(),
        strip.placement = "outside",
        # strip.background = ggplot2::element_rect(fill = NA, colour = NA),
        strip.text = ggplot2::element_text(size = 12),
        panel.spacing = unit(1, "cm"),
        panel.grid.major.y = element_blank()
        ) +
  guides(fill = "none") +
  # geom_vline(xintercept = 0,color = "grey40", size = 0.5) + # kunstmatige y-aslijn
  panel_theme_extra

kleuren_functies_lijnen <-
  kleuren_functies_nutrienten %>%
  darken(amount = 0.3, space = "combined", method = "absolute") %>%
  set_names(str_to_lower(names(kleuren_functies_nutrienten)))



# Nutriëntenlimitatie -----------------------------------------------------

lim_data <- 
  fys_chem %>% 
  filter(year(datum) == 2025, parnr %in% c(2,4, 6), month(datum) %in% c(4:9)) %>% 
  select(-parnr, -eenheid) %>% 
  pivot_wider(names_from = par, values_from = c(detectiegrens, waarde)) %>% 
  mutate(p_lim = !is.na(detectiegrens_PO4),
         n_lim = !is.na(detectiegrens_NH4) & !is.na(detectiegrens_sNO3NO2),
         nut_lim = p_lim | n_lim) %>% 
  group_by(mp) %>% 
  summarise(aantal = n(),
            aantal_lim = sum(nut_lim),
            lim_frac = aantal_lim / aantal,
            
            # aantal_lim_n = sum(n_lim),
            # lim_frac_n = aantal_lim_n / aantal,
            # 
            # aantal_lim_p = sum(p_lim),
            # lim_frac_p = aantal_lim_p / aantal
            ) %>% 
  inner_join(mp_sel, by = "mp") %>% 
  group_by(landgebruik) %>% 
  summarise(lim_frac = mean(lim_frac),
            Nutrienten = 1 - lim_frac,
            aantal_locs = n(),
            aantal_locs_lim = sum(aantal_lim > 0),
            frac_locs_lim = aantal_locs_lim / aantal_locs,
            
            # lim_frac_n = mean(lim_frac_n),
            # Stikstof = 1 - lim_frac_n,
            # aantal_locs_lim_n = sum(aantal_lim_n > 0),
            # frac_locs_lim_n = aantal_locs_lim_n / aantal_locs,
            # 
            # lim_frac_p = mean(lim_frac_p),
            # Fosfaat = 1 - lim_frac_p,
            # aantal_locs_lim_p = sum(aantal_lim_p > 0),
            # frac_locs_lim_p = aantal_locs_lim_p / aantal_locs,
  )

limitatie_plot <-
  lim_data %>% 
  mutate(landgebruik = str_to_sentence(landgebruik)) %>%
  mutate(landgebruik = fct_reorder(landgebruik, frac_locs_lim, .desc = TRUE)) %>% 
  ggplot(aes(1 - frac_locs_lim, landgebruik)) +
  geom_col() +
  scale_x_continuous(limits = c(0, NA), expand = expansion(c(0, 0.1)), labels = scales::label_percent()) +
  labs(title = "Welke soort gebieden hebben teveel nutriënten?",
       subtitle = 'Op welk deel van de locaties raakt fosfaat en/of stikstof niet "op"?',
       x = "Aandeel van alle meetlocaties",
       y = "") +
  panel_theme_extra 

# Fosfor plot --------------------------------------------------------------



fosfor_plot <-
  grafiek_data %>%
  filter(parnr == 3) %>%
  mutate(landgebruik = fct_reorder(landgebruik, waarde_gem, .desc = TRUE)) %>%
  ggplot(aes(jaar, waarde_gem, group = landgebruik, colour = landgebruik)) +
  geom_line(linewidth = 1) +
  ggrepel::geom_text_repel(aes(label = landgebruik, x = jaar + 0.1), data = filter(grafiek_data, parnr == 3, jaar == rap_jaar),
                           hjust = "left",  size = 5, direction = "y", nudge_x = 0.2, xlim = c(NA, rap_jaar + 3)) +

  # geom_smooth(se = FALSE, span = 0.6) +
  # scale_color_brewer(palette = "Dark2", guide = FALSE) +
  scale_colour_manual(values = kleuren_functies_lijnen, guide = FALSE) +
  scale_y_continuous(limits = c(0,NA), expand = expansion(c(0,0.1))) +
  scale_x_continuous(breaks = pretty_breaks(n = 10), limits = c(NA, NA)) +
  labs(title = "Gemiddelde fosfaatconcentratie",
       x = "",
       y = "mg P/l" )  +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(5.5, 90, 5.5, 5.5, unit = "pt")) +
  NULL


# Stikstof plot -----------------------------------------------------------


stikstof_plot <-
  grafiek_data %>%
  filter(parnr == 7) %>%
  mutate(landgebruik = fct_reorder(landgebruik, waarde_gem, .desc = TRUE)) %>%
  ggplot(aes(jaar, waarde_gem, group = landgebruik, colour = landgebruik)) +
  geom_line(linewidth = 1) +
  ggrepel::geom_text_repel(aes(label = landgebruik, x = jaar + 0.1), data = filter(grafiek_data, parnr == 7, jaar == rap_jaar),
                           hjust = "left",  size = 5, direction = "y", nudge_x = 0.2, xlim = c(NA, rap_jaar + 3)) +

  # geom_smooth(se = FALSE, span = 0.6) +
  # scale_color_brewer(palette = "Dark2", guide = FALSE) +
  scale_colour_manual(values = kleuren_functies_lijnen, guide = FALSE) +
  scale_y_continuous(limits = c(0,NA), expand = expansion(c(0,0.1))) +
  scale_x_continuous(breaks = pretty_breaks(n = 10), limits = c(NA, NA)) +
  labs(title = "Gemiddelde stikstofconcentratie",
       x = "",
       y = "mg N/l" )  +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(5.5, 90, 5.5, 5.5, unit = "pt")) +
  NULL


# Berekening trend en verandering -----------------------------------------

# Deze code wordt niet direct opgenomen in de rapportage maar wordt gebruikt in de voetnoten van de tekst 
# om aan te geven of significante veranderingen zijn en hoe groot deze zijn



# x <- grafiek_data %>% filter(parnr == 3, landgebruik == "glastuinbouw") %>% pull(waarde_gem)

# y <- sens.slope(x)

# str(y)

# glue("{round(y$estimates * 10, digits = 3)} mg P/l per 10 jaar" )

trends <-
  grafiek_data %>%
  mutate(landgebruik = str_to_sentence(landgebruik)) %>% 
  # filter(jaar > 2013) %>%
  group_by(parnr, par, landgebruik) %>%
  nest() %>%
  ungroup() %>% 
  mutate(sen = map(data, ~trend::sens.slope(.x$waarde_gem)),
         helling = map_dbl(sen, "estimates"),
         p_value = map_dbl(sen, "p.value"),
         gem = map_dbl(data, ~mean(.x$waarde_gem))) %>%
  mutate(decade = round(helling * 10, digits = 3),
         decade_rel = decade / (2 * gem)) %>%
  mutate(kans =  1 / p_value) %>%
  # filter(p_value < 0.05) %>%
  arrange(kans) %>% 
  mutate(trend = case_when(
    p_value > 0.05 ~ "Geen verandering",
    helling < 0 ~ "Afname",
    helling > 0 ~ "Toename",
  ))
  
# Reactable
trend_tabel_n <-
  trends %>%
  filter(par == "Ntot") %>%
  mutate(decade = ifelse(trend == "Geen verandering", NA, decade),
         decade_rel = ifelse(trend == "Geen verandering", NA, decade_rel)) %>%
  select(landgebruik, trend, decade_rel, decade) %>%
  arrange(decade_rel, landgebruik) %>%
  reactable(
    columns = list(
      landgebruik = colDef(name = "Type landgebruik"),
      trend = colDef(name = "Trend"),
      decade_rel = colDef(name = "Afname in 10 jaar (%)",
                          format = colFormat(percent = TRUE, digits = 0)),
      decade = colDef(name = "Afname in 10 jaar",
                      format = colFormat(digits = 2, suffix = " mg N/l"))
    )
  )

trend_tabel_p <-
  trends %>%
  filter(par == "Ptot") %>%
  mutate(decade = ifelse(trend == "Geen verandering", NA, decade),
         decade_rel = ifelse(trend == "Geen verandering", NA, decade_rel)) %>%
  select(landgebruik, trend, decade_rel, decade) %>%
  arrange(decade_rel, landgebruik) %>%
  reactable(
    columns = list(
      landgebruik = colDef(name = "Type landgebruik"),
      trend = colDef(name = "Trend"),
      decade = colDef(name = "Verandering per 10 jaar",
                      format = colFormat(digits = 2, suffix = " mg P/l")),
      decade_rel = colDef(name = "Relatieve verandering per 10 jaar",
                          format = colFormat(percent = TRUE, digits = 0))
    )
  )

# Variant met gt
# trend_tabel_n <-
#   trends %>% 
#   filter(par == "Ntot") %>% 
#   mutate(decade = ifelse(trend == "Geen verandering", NA, decade),
#          decade_rel = ifelse(trend == "Geen verandering", NA, decade_rel)) %>% 
#   select(landgebruik, trend, decade_rel, decade) %>% 
#   arrange(decade_rel, landgebruik) %>% 
#   mutate(trend = case_when(
#     trend ==  "Geen verandering" ~ NA,
#     trend == "Afname" ~ "fas fa-arrow-trend-down",
#     trend == "Toename" ~ "fas fa-arrow-trend-up",
#   )) %>% 
#   gt(rowname_col = "landgebruik") %>% 
#   cols_label(trend ~ "Trend",
#              decade_rel ~ "Afname in 10 jaar (%)",
#              decade ~ "Afname in 10 jaar") %>% 
#   tab_stubhead(label = md("**Landgebruik**")) %>% 
#   fmt_icon(columns = trend, fill_color = blauw, height = "1.5em") %>% 
#   fmt_percent(columns = decade_rel, dec_mark = ",", decimals = 0, incl_space = TRUE) %>% 
#   fmt_number(columns = decade, dec_mark = ",", pattern = "{x} mg N/l") %>% 
#   sub_missing() %>% 
#   cols_align("center", columns = !landgebruik) 

# trend_tabel_p <-
#   trends %>% 
#   filter(par == "Ptot") %>% 
#   mutate(decade = ifelse(trend == "Geen verandering", NA, decade),
#          decade_rel = ifelse(trend == "Geen verandering", NA, decade_rel)) %>% 
#   select(landgebruik, trend, decade_rel, decade) %>% 
#   arrange(decade_rel, landgebruik) %>% 
#   mutate(trend = case_when(
#     trend ==  "Geen verandering" ~ NA,
#     trend == "Afname" ~ "fas fa-arrow-trend-down",
#     trend == "Toename" ~ "fas fa-arrow-trend-up",
#   )) %>% 
#   gt(rowname_col = "landgebruik") %>% 
#   cols_label(trend ~ "Trend",
#              decade_rel ~ "Afname in 10 jaar (%)",
#              decade ~ "Afname in 10 jaar") %>% 
#   tab_stubhead(label = md("**Landgebruik**")) %>% 
#   fmt_icon(columns = trend, fill_color = blauw, height = "1.5em") %>% 
#   fmt_percent(columns = decade_rel, dec_mark = ",", decimals = 0, incl_space = TRUE) %>% 
#   fmt_number(columns = decade, dec_mark = ",", pattern = "{x} mg P/l") %>% 
#   sub_missing() %>% 
#   cols_align("center", columns = !landgebruik) 

# Glastuinbouw t.o.v. andere gebieden -------------------------------------

grafiek_data_glas <-
  fys_chem %>%
  filter(jaar >= rap_jaar - 9,
         jaar <= rap_jaar,
         parnr %in% c(3, 7)) %>%
  inner_join(mp_sel, by = "mp") %>%
  add_maand() %>%
  mutate(landgebruik = fct_collapse(landgebruik, glastuinbouw = "glastuinbouw", other_level = "andere gebieden")) %>% 
  group_by(mp, parnr, landgebruik, jaar, maand) %>%
  summarise(waarde = mean(waarde)) %>%
  group_by(parnr, landgebruik, jaar) %>%
  summarise(waarde_gem = mean(waarde)) %>%
  ungroup() %>%
  left_join(parameters, by = "parnr") %>%
  mutate(
    fill_group = ifelse(jaar == rap_jaar, rap_jaar, "Andere jaren"),
    landgebruik = fct_reorder(landgebruik, waarde_gem, .desc = TRUE))




plot_glas_n <-
  grafiek_data_glas %>%
  filter(parnr == 7) %>%
  mutate(landgebruik = fct_reorder(landgebruik, waarde_gem, .desc = TRUE)) %>%
  ggplot(aes(jaar, waarde_gem, group = landgebruik, colour = landgebruik)) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = c(glastuinbouw = oranje, `andere gebieden` = blauw)) +
  scale_y_continuous(limits = c(0,NA), 
                     expand = expansion(c(0,0.1)), 
                     labels = scales::label_number(decimal.mark = ","), 
                     breaks = pretty_breaks(5)) +
  scale_x_continuous(breaks = scales::breaks_width(2, 1), limits = c(NA, NA)) +
  labs(title = "Stikstof",
       x = "",
       y = "mg N/l" )  +
  coord_cartesian(clip = "off") +
  theme(
    plot.title = element_text(size = rel(1.2)), legend.position = "none") 

plot_glas_p <- 
  grafiek_data_glas %>%
  filter(parnr == 3) %>%
  mutate(landgebruik = fct_reorder(landgebruik, waarde_gem, .desc = TRUE)) %>%
  ggplot(aes(jaar, waarde_gem, group = landgebruik, colour = landgebruik)) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = c(glastuinbouw = oranje, `andere gebieden` = blauw)) +
  scale_y_continuous(limits = c(0,NA), expand = expansion(c(0,0.1)), labels = scales::label_number(decimal.mark = ","), breaks = pretty_breaks(5)) +
  scale_x_continuous(breaks = scales::breaks_width(2, 1), limits = c(NA, NA)) +
  labs(title = "Fosfaat",
       x = "",
       y = "mg P/l" )  +
  coord_cartesian(clip = "off") +
  theme(
    plot.title = element_text(size = rel(1.2)),
    legend.position = "none") +
  NULL

# library(patchwork)

plot_glastuinbouw <- 
(plot_glas_p + plot_glas_n) + 
  patchwork::plot_annotation(title = glue("Ontwikkeling van fosfaat en stikstof"),
                             subtitle = glue("<span style='color:{oranje};'>glastuinbouwgebied</span> t.o.v. <span style='color:{blauw};'>andere gebieden</span>"),
                             theme = theme(plot.title = ggtext::element_markdown(),
                                           plot.subtitle = ggtext::element_markdown()))  

  