# stop("Checken of alle planten ingedeeld zijn (submers etc...)")
# gecheckt 22-12-2025

## ---- libs ----

library(tidyverse)
library(HHSKwkl)
library(twn)
library(readxl)
library(glue)
library(sf)
library(leaflet)

## ---- load_data ----

# copy_data(c("meetpunten.csv", "planten_info.xlsx", "biologie.csv"))

rap_jaar <- 2025

meetpunten <- readRDS("data/meetpunten.rds")
bio <- readRDS("data/biologie.rds") %>% HHSKwkl::add_jaar()
fys_chem <- readRDS("data/fys_chem.rds")
# ws_grens <- read_sf("data/ws_grens.gpkg") %>% st_transform(crs = 4326)
# HHSKwkl::ws_grens_wgs
planten_info <- readxl::read_excel("data/planten_info.xlsx", sheet = "planten_info")


f_mp_type <- maak_opzoeker(meetpunten, mp, meetpunttypering)
f_gebied <-  maak_opzoeker(meetpunten, mp, gebiednaam)

panel_theme_extra <- 
  theme(plot.background = element_rect(fill = prismatic::clr_lighten(blauw_l, 0.90, space = "HSL")),
        strip.background = element_rect(fill = prismatic::clr_lighten(blauw_l, 0.90, space = "HSL")),
        legend.background = element_rect(fill = prismatic::clr_lighten(blauw_l, 0.90, space = "HSL")),
        plot.title.position = "plot",
        panel.grid.major.x = element_blank(),
        panel.spacing.x = unit(20, "points"),
        margins = margin_auto(10))

## ---- planten-voorbewerking ----

planten <-
  bio %>%
  filter(methode %in% c("VEGSTO", "VEG%"),
         f_mp_type(mp) %in% c(1, 2, 3)) %>%
  select(-contains("stadium")) %>%
  distinct()

stowa_fun <- function(x){

  stowa_codes <- tibble::tribble(
    ~stowa, ~bedekking,
         0,          0,
         1,        0.5,
         2,          1,
         3,          2,
         4,          5,
         5,          8,
         6,         19,
         7,         38,
         8,         63,
         9,         88
  ) %>% tibble::deframe()

  unname(stowa_codes[as.character(x)])

}

planten_basis <-
  planten %>%
  select(mp, datum, methode, naam, waarde = waarde_totaal, eenheid) %>%
  mutate(waarde = ifelse(methode == "VEGSTO", stowa_fun(waarde), waarde)) %>%
  select(-methode, -eenheid) %>%
  left_join(select(planten_info, naam, bedekkingslaag, aquatisch, bijzonder), by = "naam") %>%
  mutate(jaar = year(datum)) %>%
  mutate(naam_species = ifelse(twn_taxonlevel(naam) < "Genus",
                               yes = twn::increase_taxonlevel(naam, taxonlevel = "Species"), no = NA))

planten_per_groep <-
  planten_basis %>%
  group_by(mp, datum, bedekkingslaag, jaar) %>%
  summarise(waarde = sum(waarde),
            waarde = ifelse(waarde > 100, 100, waarde)) %>%
  ungroup() %>%
  complete(nesting(mp, datum, jaar), bedekkingslaag, fill = list(waarde = 0)) %>% # toevoegen van ontbrekende bedekkingslagen
  filter(!is.na(bedekkingslaag)) # planten zonder bedekkingslaag zijn niet aquatisch


# Grafieken submers en kroos ----------------------------------------------


plot_submers <-
  planten_per_groep %>%
  mutate(gebied = f_gebied(mp)) %>%
  filter(jaar > 2008, bedekkingslaag == "submers",
         gebied %in% c("Schieland", "Krimpenerwaard")) %>%
  group_by(jaar, gebied) %>%
  summarise(perc = perc(waarde >= 5)) %>%
  ungroup() %>% 
  ggplot(aes(jaar, perc)) +
  geom_line(colour = blauw, linewidth = 1) +
  geom_point(colour = blauw) + 
  ggrepel::geom_text_repel(aes(label = paste0(round(perc), "%")), nudge_y = 8, direction = "y",
                            color = grijs_d, segment.color = grijs, segment.linetype = "dotted",
                            family = "Ruda Bold", size = 4,
                            data = . %>% filter(jaar %in% c(min(jaar), max(jaar)))) +
  facet_wrap(~gebied, scales = "free_y") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.0)), limits = c(0, 85), labels = scales::label_percent(scale = 1)) +
  scale_x_continuous(breaks = scales::breaks_width(5, 0), limits = c(NA, rap_jaar)) +
  labs(title = "Onderwaterplanten zijn in de Krimpenerwaard verdwenen",
       subtitle = "Wateren met planten onder water",
       y = "aandeel van alle wateren",
       x = "",
       caption = "locaties met tenminste 5% begroeiing") +
  hhskthema() +
  panel_theme_extra +
  theme(strip.text = element_text(size = 12, hjust = 0))

#"#edf7f8" 95%
#EDF5FF


plot_kroos <-
  planten_per_groep %>%
  mutate(gebied = f_gebied(mp)) %>%
  filter(jaar > 2008, bedekkingslaag == "kroos",
         gebied %in% c("Schieland", "Krimpenerwaard")) %>%
  group_by(jaar, gebied) %>% 
  summarise(perc = perc(waarde > 50)) %>% 
  ggplot(aes(jaar, perc)) +
  geom_line(colour = blauw, linewidth = 1) +
  geom_point(colour = blauw) +
  facet_wrap(~gebied, scales = "free_y") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.0)), limits = c(0, 50), labels = scales::label_percent(scale = 1)) +
  scale_x_continuous(breaks = scales::breaks_width(5, 0), limits = c(NA, rap_jaar)) +
  labs(title = "Wateren met veel kroos",
       subtitle = " ",
       y = "aandeel van alle wateren",
       x = "",
       caption = "locaties meer dan de helft bedekt met kroos") +
  hhskthema() +
  panel_theme_extra +
  theme(strip.text = element_text(size = 12, hjust = 0))


# Kaart met kreeften ------------------------------------------------------
  
f_nednaam <-
  tibble::tribble(
    ~latijn,                               ~nednaam,
    "Procambarus clarkii",        "Rode amerikaanse rivierkreeft",
    "Procambarus acutus",  "Gestreepte amerikaanse rivierkreeft",
    "Faxonius virilis", "Geknobbelde amerikaanse rivierkreeft",
    "Faxonius limosus",    "Gevlekte amerikaanse rivierkreeft"
  ) %>% maak_opzoeker()
  
pal <- colorFactor(palette = c(blauw, oranje), domain = c(TRUE, FALSE))  
  
  
kaart_kreeften <- 
  bio %>%
  select(-contains("stadium")) %>%
  distinct() %>%
  filter(methode == "KR12", jaar == rap_jaar, taxatype == "MACEV") %>%
  filter(naam != "Eriocheir sinensis") %>% # Wolhandkrab
  mutate(waarde_totaal = round(waarde_totaal, digits = 0)) %>% 
  mutate(nednaam = f_nednaam(naam)) %>%
  mutate(tekst_b = glue("<b>{nednaam}:</b> {waarde_totaal}")) %>%
  arrange(mp, desc(waarde_totaal)) %>% #filter(is.na(nednaam)) %>% View()
  summarise(aantal = sum(waarde_totaal),
            tekst_label = glue("Totaal aantal kreeften: {aantal}"),
            tekst_a = glue("<b>Totaal aantal kreeften:</b> {aantal}"),
            tekst_b = ifelse(aantal == 0, "", glue_collapse(tekst_b, sep = "<br>")),
            tekst = ifelse(aantal == 0, tekst_a, glue("{tekst_a}<br><br><i>Aantallen per soort</i><br>{tekst_b}")),
            .by = mp) %>% 
  left_join(meetpunten) %>%
  sf::st_as_sf(coords = c("x", "y"), crs = 28992) %>%
  st_transform(crs = 4326) %>%
  basiskaart(type = "cartolight") %>%
  addPolylines(data = ws_grens_wgs, color = "#616161", weight = 3, label = ~"waterschapsgrens") %>%
  addCircleMarkers(fillColor = ~pal(aantal > 0), fillOpacity = 1, radius = 7, 
                   color = "#616161", opacity = 1, weight = 1, 
                   popup = ~tekst, label = ~tekst_label) %>% 
  addLegend(colors = c(blauw, oranje), labels = c("Geen kreeften aanwezig", "Wel kreeften aanwezig"), opacity = 1) %>% 
  leaflet.extras::addFullscreenControl()



# Figuren kreeften --------------------------------------------------------

kreeften_tot <-
  bio %>% 
  filter(methode == "KR12",
         taxatype == "MACEV") %>% 
  select(-contains("stadium")) %>%
  rename(waarde = waarde_totaal) %>% 
  distinct() %>% 
  filter(str_detect(naam, "Procambarus|Faxonius|Geen kreeften")) %>% # Uitsluiten Wolhandkrab
  add_jaar() %>% 
  group_by(mp, jaar) %>% 
  summarise(n_kreeften = sum(waarde)) %>% 
  ungroup() %>% 
  mutate(gebied = f_gebied(mp)) 

plot_kreeften_aandeel_locs <-
  kreeften_tot %>% 
  group_by(gebied, jaar) %>% 
  summarise(n_locs = n_distinct(mp),
            n_aangetroffen = sum(n_kreeften > 0),
            frac_aangetroffen = n_aangetroffen / n_locs) %>% 
  ggplot(aes(jaar, frac_aangetroffen)) +
  geom_line(linewidth = 1) +
  geom_point() +
  # ggrepel::geom_text_repel(aes(label = paste0(round(frac_aangetroffen * 100), "%")), nudge_y = -0.08, direction = "y",
  #                          color = blauw, segment.color = grijs, segment.linetype = "dotted",
  #                          family = "Ruda Bold", size = 4,
  #                          data = . %>% filter(jaar %in% c(min(jaar), max(jaar)))) +
  scale_y_continuous(limits = c(0, 1), expand = expansion(c(0, 0.1)), labels = scales::percent_format()) +
  facet_wrap(~gebied, axes = "all") +
  labs(title = "Toename van plekken met kreeften in Schieland",
       subtitle = "Op welke deel van de locaties worden kreeften aangetroffen?",
       x = "",
       y = "% locaties met kreeften") +
  hhskthema() +
  panel_theme_extra +
  theme(strip.text = element_text(size = 12, hjust = 0))

plot_kreeften_aantallen <-
  kreeften_tot %>% 
  filter(n_kreeften > 0) %>% 
  group_by(gebied, jaar) %>% 
  summarise(gem_n_kreeften = mean(n_kreeften, trim = 0.05)) %>% 
  ggplot(aes(jaar, gem_n_kreeften)) +
  geom_line(linewidth = 1) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = round(gem_n_kreeften)), nudge_y = -4, direction = "y",
                           color = grijs_d, segment.color = grijs, , segment.linetype = "dotted", 
                           family = "Ruda Bold", size = 4,
                            data = . %>% filter(jaar %in% c(min(jaar), max(jaar)))) +
  scale_y_continuous(limits = c(0, 1), expand = expansion(c(0, 0.1)), labels = scales::percent_format()) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(c(0, 0.1))) +
  facet_wrap(~gebied, axes = "all") +
  labs(title = "Verdubbelling van het aantal kreeften sinds 2020",
       subtitle = "Ontwikkeling van het gemiddeld aantal gevangen kreeften",
       x = "",
       y = "Aantal",
       caption = "Getrimd gemiddelde van locaties waar kreeften zijn aangetroffen") +
  hhskthema() +
  panel_theme_extra +
  theme(strip.text = element_text(size = 12, hjust = 0))



# Kaart blauwalgen --------------------------------------------------------


blauwalg_klassen <- c("(Vrijwel) geen blauwalgen", "Blauwalgen aanwezig", "Veel blauwalgen aanwezig")

pal <- colorFactor(palette = c(blauw, oranje_l, oranje), domain = blauwalg_klassen)  

kaart_blauwalgen <- 
  fys_chem %>% 
  filter(parnr == 415, year(datum) == rap_jaar) %>% 
  summarise(waarde = max(waarde), .by = mp) %>% 
  mutate(klasse = cut(waarde, c(-1, 12, 75,9999), labels = blauwalg_klassen)) %>% 
  left_join(meetpunten) %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 28992) %>%
  st_transform(crs = 4326) %>%
  basiskaart(type = "cartolight") %>% 
  addPolylines(data = ws_grens_wgs, color = "#616161", weight = 3, label = ~"waterschapsgrens") %>%
  addCircleMarkers(fillColor = ~pal(klasse), fillOpacity = 1, radius = 8, 
                   color = "#616161", opacity = 1, weight = 1, label = ~klasse) %>% 
  addLegend(pal = pal, values = ~klasse, opacity = 1, title = "Hoeveelheid blauwalgen") %>% 
  leaflet.extras::addFullscreenControl()
  
