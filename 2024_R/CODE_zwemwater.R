## ---- Setup ----

library(HHSKwkl)
library(tidyverse)
library(scales)
library(leaflet)
library(sf)
library(ggtext)


rap_jaar <- 2024

meetpunten <- readRDS("data/meetpunten.rds")
ws_grens <- read_sf("data/ws_grens.gpkg") %>% st_transform(crs = 4326)
pfas <- readRDS("data/fys_chem.rds") %>% filter(parnr == 2499, year(datum) == rap_jaar) %>% summarise(peq = mean(waarde), .by = mp)

maatregelen <- readxl::read_excel("data/Zwemwater maatregelen vanaf 2012 v2.xlsx") %>% rename_all(tolower)

blauwe_tekst <- function(tekst, grootte = NA){
  grootte <- if (is.na(grootte)) "" else glue::glue(";font-size:{grootte}px")
  glue::glue("<span style='color:#0079c2{grootte}'>{tekst}</span>")
}

oranje_tekst <- function(tekst, grootte = NA){
  grootte <- if (is.na(grootte)) "" else glue::glue(";font-size:{grootte}px")
  glue::glue("<span style='color:#C25100{grootte}'>{tekst}</span>")
}

## ---- kaart-zwemlocaties ----



zwemlocaties <- tibble::tribble(
  ~mp,                            ~naam,     ~oordeel,
  "S_0058",     "Zevenhuizerplas Nesselande", "Uitstekend",
  "S_0124",               "Bleiswijkse Zoom", "Uitstekend",
  "S_0128",                 "Kralingse Plas", "Uitstekend",
  "S_0131", "Zevenhuizerplas Noordwestzijde", "Uitstekend",
  "S_0152",           "Willem-Alexanderbaan", "Aanvaardbaar",
  "S_1120",               "'t Zwarte Plasje", "Uitstekend",
  "S_1124",               "Kralings Zwembad", "Uitstekend",
  "K_1102",                  "Krimpenerhout", "Uitstekend"
)


zwem_icons <- leaflet::iconList(
  Uitstekend =   leaflet::makeIcon("images/icon_zwemwater_blauw.png",
                                   iconWidth = 30, iconHeight = 30,
                                   iconAnchorX = 15, iconAnchorY = 15),
  Goed =         leaflet::makeIcon("images/icon_zwemwater_groen.png",
                                   iconWidth = 30, iconHeight = 30,
                                   iconAnchorX = 15, iconAnchorY = 15),
  Aanvaardbaar = leaflet::makeIcon("images/icon_zwemwater_oranje.png",
                                   iconWidth = 30, iconHeight = 30,
                                   iconAnchorX = 15, iconAnchorY = 15),
  Slecht =       leaflet::makeIcon("images/icon_zwemwater_rood.png",
                                   iconWidth = 30, iconHeight = 30,
                                   iconAnchorX = 15, iconAnchorY = 15)
  # `Geen oordeel` =       leaflet::makeIcon("images/icon_zwemwater_grijs.png",
  #                                  iconWidth = 30, iconHeight = 30,
  #                                  iconAnchorX = 15, iconAnchorY = 15)
)

# Let op dat de iconen ook in _book/images staan anders dan doet de legenda het niet
zwem_legend <- paste0("<b>Oordeel</b></br>",
                      "<img src='images/icon_zwemwater_blauw.png' height='30' width = '30'> Uitstekend<br/>",
                      "<img src='images/icon_zwemwater_groen.png' height='30' width = '30'> Goed<br/>",
                      "<img src='images/icon_zwemwater_oranje.png' height='30' width = '30'> Aanvaardbaar<br/>",
                      "<img src='images/icon_zwemwater_rood.png' height='30' width = '30'> Slecht<br>"
                      # "<img src='images/icon_zwemwater_grijs.png' height='30' width = '30'> Geen oordeel"
                      )


kaart_zwemlocaties <-
  zwemlocaties %>%
  mutate(popup = paste0(naam, "</br><b>Oordeel:</b> ", oordeel)) %>%
  left_join(meetpunten, by = "mp") %>%
  st_as_sf(coords = c("x", "y"), crs = 28992) %>% 
  st_transform(crs = 4326) %>% 
  HHSKwkl::basiskaart(type = "cartolight") %>%
  addMarkers(icon = ~zwem_icons[oordeel], label = ~paste(naam, "-", oordeel), popup = ~popup, options = markerOptions(riseOnHover = TRUE)) %>%
  addPolylines(data = ws_grens, color = "#616161", weight = 3, label = ~"waterschapsgrens") %>%
  addControl(html = zwem_legend, position = "topright")

## ---- waarschuwingen-blauwalg ----

# afkortingen <- c("S_0058" = "ZN",
#                  "S_0124" = "BZ",
#                  "S_0128" = "KP",
#                  "S_0131" = "ZH",
#                  "S_0152" = "WA",
#                  "S_1120" = "ZP",
#                  "S_1124" = "KZ",
#                  "K_1102" = "KH")

waarschuwingen <-
  maatregelen %>% 
  filter(probleem == "Blauwalg", jaar <= rap_jaar) %>%
  left_join(zwemlocaties, by = c("meetpunt" = "mp")) %>%
  filter(!is.na(naam)) %>%
  group_by(jaar, meetpunt, naam, maatregel) %>%
  summarise(dagen = sum(dagen, na.rm = TRUE)) %>%
  ungroup() #%>%
  # mutate(afkorting = afkortingen[meetpunt])

# waarschuwingen_rap_jaar <- waarschuwingen %>% filter(jaar == rap_jaar) %>% summarise(dagen = sum(dagen)) %>% pull(dagen)

# titel_tekst <- glue("Dagen met blauwalg in {rap_jaar}: <b style='color:#0079c2'>{waarschuwingen_rap_jaar}</b>")

blauwalgenplot <-
  waarschuwingen %>%
  complete(jaar, nesting(naam, meetpunt), fill = list(dagen = 0, maatregel = "Waarschuwing")) %>%
  filter(!(meetpunt == "S_0152" & jaar < 2023)) %>%
  mutate(dagen_totaal = sum(dagen, na.rm = TRUE), .by = c(jaar, naam)) %>% 
  mutate(naam = fct_reorder(naam, dagen, .desc = TRUE, .fun = last)) %>% 
  mutate(maatregel = fct_rev(maatregel)) %>% 
  filter(jaar >= rap_jaar - 9) %>%
  ggplot(aes(jaar, dagen, fill = maatregel)) +
  geom_col(width = 0.85, colour = grijs_l) +
  geom_text(aes(y = dagen_totaal, label = dagen_totaal), nudge_y = 15, colour = "grey60") +
  facet_wrap(~naam, ncol = 2, scales = "free_x") +
  scale_x_continuous(limits = c(rap_jaar - 9.5, rap_jaar + 0.5), breaks = pretty_breaks(10), guide = guide_axis(n.dodge = 1), expand = c(0,0.1)) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(c(0, 0.1))) +
  scale_fill_manual(values = c("Waarschuwing" = oranje_l, "Negatief zwemadvies" = oranje),
                    guide = guide_legend(title = "", reverse = FALSE)) +
  labs(title = "Aantal dagen met blauwalg",
       x = "",
       y = "") +
  thema_line_facet +
  theme(plot.title = element_markdown(face = "bold"),
        axis.line.x = element_line(colour = "grey60"),
        panel.spacing.x = unit(30, "points"),
        panel.spacing.y = unit(20, "points"),
        axis.ticks.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "top")

aantal_dagen_blauwalg_gemiddeld <- 
  waarschuwingen %>%
  filter(jaar == rap_jaar) %>% 
  summarise(gem_aantal = sum(dagen) / 8) %>% 
  pull(gem_aantal) %>% 
  round()

# blauwalgenplot_totaal <- 
#   waarschuwingen %>%
#   filter(maatregel != "Zwemverbod") %>% 
#   complete(jaar, nesting(naam, meetpunt), maatregel, fill = list(dagen = 0)) %>%
#   filter(!(meetpunt == "S_0152" & jaar < 2023)) %>%
#   filter(jaar >= rap_jaar - 9) %>% 
#   group_by(jaar, maatregel) %>% 
#   summarise(gem_dagen = mean(dagen)) %>% 
#   ungroup() %>% 
#   mutate(dagen_totaal = round(sum(gem_dagen, na.rm = TRUE), digits = 0), .by = c(jaar)) %>%
#   # mutate(naam = fct_reorder(naam, dagen, .desc = TRUE, .fun = last)) %>% 
#   mutate(maatregel = fct_rev(maatregel)) %>% 
#   
#   ggplot(aes(jaar, gem_dagen, fill = maatregel)) +
#   geom_col(width = 0.85, colour = grijs_l) +
#   geom_text(aes(y = dagen_totaal, label = dagen_totaal), nudge_y = 2
#             , colour = "grey60") +
#   # facet_wrap(~naam, ncol = 2, scales = "free_x") +
#   scale_x_continuous(limits = c(rap_jaar - 9.5, rap_jaar + 0.5), breaks = pretty_breaks(10), guide = guide_axis(n.dodge = 1), expand = c(0,0.1)) +
#   scale_y_continuous(limits = c(0, NA), expand = expansion(c(0, 0.1))) +
#   scale_fill_manual(values = c("Waarschuwing" = oranje_l, "Negatief zwemadvies" = oranje),
#                     guide = guide_legend(title = "", reverse = FALSE)) +
#   labs(title = "Gemiddeld aantal dagen met blauwalg",
#        subtitle = "Alle locaties",
#        x = "",
#        y = "") +
#   thema_line_facet +
#   theme(plot.title = element_markdown(face = "bold"),
#         axis.line.x = element_line(colour = "grey60"),
#         panel.spacing.x = unit(30, "points"),
#         panel.spacing.y = unit(20, "points"),
#         axis.ticks.x = element_blank(),
#         axis.line.y = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         panel.grid.major.y = element_blank(),
#         legend.position = "top")



# Zwemmersjeuk ------------------------------------------------------------



locaties_jeuk  <- 
  maatregelen %>% 
  filter(jaar == rap_jaar, probleem == "Jeukklachten", maatregel != "Nader onderzoek") %>% 
  rename(mp = meetpunt) %>% 
  summarise(jeukdagen = sum(dagen), .by = mp)

kaart_jeukklachten <-
  zwemlocaties %>%
  left_join(meetpunten, by = "mp") %>% 
  inner_join(locaties_jeuk) %>% 
  st_as_sf(coords = c("x", "y"), crs = 28992) %>% 
  st_transform(crs = 4326) %>% 
  HHSKwkl::basiskaart(type = "cartolight") %>%
  addMarkers(icon = ~leaflet::makeIcon("images/icon_zwemwater_driehoek.png",
                                       iconWidth = 35, iconHeight = 30,
                                       iconAnchorX = 15, iconAnchorY = 15), 
             label = ~naam, 
             popup = ~naam, 
             options = markerOptions(riseOnHover = TRUE)) %>%
  addPolylines(data = ws_grens, color = "#616161", weight = 3) 


# PFAS --------------------------------------------------------------------



plot_pfas <- 
  pfas %>% 
  inner_join(zwemlocaties) %>% 
  mutate(naam = fct_reorder(naam, peq)) %>% 
  ggplot(aes(peq, naam)) + 
  geom_col(width = 0.85) + 
  geom_vline(xintercept = 280, colour = oranje, linetype = "dashed", linewidth = 1) +
  scale_x_continuous(limits = c(0, NA), expand = expansion(c(0, 0.1))) +
  # scale_x_log10() +
  labs(title = "Hoeveelheid PFAS op zwemlocaties",
       y = "",
       x = "PFAS - PFOA-equivalenten in ng/l") +
  annotate("text", x = 300, y = 9, hjust = 0, label = "Advieswaarde RIVM", colour = oranje, fontface = "bold") +
  coord_cartesian(ylim = c(1, 8.7)) +
  hhskthema() +
  theme(panel.grid.major.y = element_blank(),
        plot.title.position = "plot",
        axis.text.y = element_text(size = 10),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank())

