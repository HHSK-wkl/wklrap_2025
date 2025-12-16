# source("R/CODE_index_setup.R")

# LETOP overal de beoordeling vervangen voor het juiste jaar

# Setup  ------------------------------------------------------------------


library(HHSKwkl)
library(tidyverse)
library(glue)
library(sf)
library(leaflet)
library(readxl)

rap_jaar <- 2024

# INFORMATIE OVER WATERLICHAAM TOVOEGEN AAN POPUP

waterlichamen <- st_read("data/waterlichamen.gpkg", quiet = TRUE) %>% 
  rename(naam = OWMNAAM, nr = OWMIDENT) #%>% 

ws_grens <- st_read("data/ws_grens.gpkg", quiet = TRUE) %>% st_transform(crs = 4326)

f_krw_omsch <-
  tibble::tribble(
  ~code,                                ~omsch,
  "M10",         "Laagveen vaarten en kanalen",
  "M14",    "Grote ondiepe gebufferde plassen",
  "M1a",             "Zoete gebufferde sloten",
  "M20",  "Matig grote diepe gebufferde meren",
  "M27", "Matig grote ondiepe laagveenplassen",
   "M3",      "Gebufferde (regionale) kanalen",
  "M30",                 "Zwak brakke wateren",
   "M8",           "Gebufferde laagveensloten"
  ) %>% maak_opzoeker()

krw_data <-
  readxl::read_excel("data/waterlichamen_ekrs_en_doelen_2024.xlsx") %>%
  mutate(type = fct_relevel(type, c("Algen", "Waterplanten", "Macrofauna", "Vis")),
         groep = fct_relevel(groep, "Boezem", "Plassen", "Sloten", "Kanalen Krimpenerwaard", "Kanalen Schieland")) %>% 
  arrange(nr, type)
  


# Waterlichamen-kaart -------------------------------------------------------------------

popup_data <-
  krw_data %>%
  # mutate(oordeel = cut(ekr_2021 / doelen, c(0, 0.33333, 0.66666, 1, 50), labels = c("Slecht", "Ontoereikend", "Matig", "Goed"))) %>%
  mutate(frac = ekr / doelen,
         oordeel = scales::percent(ifelse(frac > 1, 1, frac), accuracy = 1)) %>%
  group_by(nr, naam, watertype) %>%
  summarise(oordeel = glue_collapse(glue("<b>{type}:</b> {oordeel}", .na = "-"), sep = "<br>")) %>%
  ungroup() %>%
  mutate(popup_tekst = glue("<b>Waterlichaam:</b> {naam}<br><b>Watertype:</b> {f_krw_omsch(watertype)} ({watertype})<hr><b>Toestand t.o.v. doel</b><br>{oordeel}")) %>%
  select(nr, popup_tekst)

pal <- leaflet::colorFactor(palette = RColorBrewer::brewer.pal(9, "Set1") , domain = waterlichamen$naam)

kaart_waterlichamen <-
  waterlichamen %>%
  left_join(popup_data, by = "nr") %>% #st_drop_geometry() %>%  View()
  st_transform(crs = 4326) %>%
  leaflet() %>%
  leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group = "Kaart") %>%
  leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Luchtfoto") %>%
  leaflet::addLayersControl(baseGroups = c("Kaart", "Luchtfoto"),
                            options = leaflet::layersControlOptions(collapsed = FALSE), position = "topleft") %>%
  addPolylines(data = ws_grens, opacity = 1, color = "grey", weight = 2, label = "waterschapsgrens") %>%
  addPolygons(weight = 4, color = ~pal(naam),
              fillOpacity = 0.8, opacity = 0.8,
              label = ~naam,
              popup = ~popup_tekst,
              highlightOptions = highlightOptions(color = blauw, bringToFront = TRUE, opacity = 1)) %>% 
  leaflet.extras::addFullscreenControl()


# KRW-doelen -----------------------------------------------------------------

krw_doelen <-
  krw_data %>%
  mutate(fractie = doelen / 0.6,
         fractie = if_else(fractie > 1, 1, fractie)) %>%
  mutate(doelaanpassing = 1 - fractie) %>%
  rename(doel = fractie) %>%
  pivot_longer(cols = c(doel, doelaanpassing), names_to = "dummy", values_to = "fractie2") %>%

  ggplot(aes(fractie2, fct_reorder(naam, nr, .fun = first, .desc = TRUE), fill = fct_rev(dummy))) +
  geom_col() +
  facet_grid(cols = vars(type), rows = vars(groep),
             scales = "free_y", space = "free_y", switch = "y", labeller = label_wrap_gen(16)) +
  scale_fill_manual(values = c(doel = blauw_m, doelaanpassing = grijs_m)) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0),
                     labels = function(x) scales::percent(x, accuracy = 1), breaks = scales::pretty_breaks(2)) +
  labs(x = "Hoogte doel t.o.v. standaard",
       y ="",
       title = "Doelen waterlichamen",
       caption = "Sloten zijn watergangen die smaller zijn dan 8 meter, kanalen zijn watergangen die breder zijn dan 8 meter.") +
  hhskthema() +
  theme(panel.spacing = unit(1.5, "lines"),
        plot.margin = margin(5.5, 15, 5.5, 5.5, unit = "pt"),
        strip.placement = "outside",
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        panel.grid.major.y =  element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "top",
        plot.title.position = "plot") +
  guides(fill = guide_legend(title = "", reverse = TRUE))


# KRW-opgave --------------------------------------------------------------

krw_opgave <-
  krw_data %>%
  mutate(fractie = ekr / doelen,
         fractie = if_else(fractie > 1, 1, fractie)) %>%
  mutate(doelgat = 1 - fractie) %>%
  rename(`huidige toestand` = fractie) %>%
  pivot_longer(cols = c(`huidige toestand`, doelgat), names_to = "dummy", values_to = "fractie2") %>%
  ggplot(aes(fractie2, fct_reorder(naam, nr, .fun = first, .desc = TRUE), fill = dummy)) +
  geom_col() +
  facet_grid(cols = vars(type), rows = vars(groep),
             scales = "free_y", space = "free_y", switch = "y", labeller = label_wrap_gen(16)) +
  scale_fill_manual(values = c("huidige toestand" = blauw_m, doelgat = oranje_m)) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0),
                     labels = function(x) scales::percent(x, accuracy = 1), breaks = scales::pretty_breaks(2)) +
  labs(x = "Toestand ten opzichte van het doel",
       y ="",
       title = "Opgave waterlichamen",
       caption = "Sloten zijn watergangen die smaller zijn dan 8 meter, kanalen zijn watergangen die breder zijn dan 8 meter.") +
  hhskthema() +
  theme(panel.spacing = unit(1.5, "lines"),
        plot.margin = margin(5.5, 15, 5.5, 5.5, unit = "pt"),
        strip.placement = "outside",
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        panel.grid.major.y =  element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "top",
        plot.title.position = "plot") +
  guides(fill = guide_legend(title = "", reverse = TRUE))


# Overig water ------------------------------------------------------------


kleuren_ov <- c(Akkerbouwgebied       = RColorBrewer::brewer.pal(9, "Set1")[6], # akkerbouw
                Glastuinbouwgebied    = RColorBrewer::brewer.pal(9, "Set1")[2],
                Weidegebied            = RColorBrewer::brewer.pal(9, "Set3")[7],
                "Stedelijk gebied"       = RColorBrewer::brewer.pal(9, "Set1")[1],
                "Eendragtspolder plas-dras" = RColorBrewer::brewer.pal(9, "Set1")[5],
                "Zwemplas Krimpenerhout" = RColorBrewer::brewer.pal(9, "Set1")[4],
                "Natuurgebied" = RColorBrewer::brewer.pal(9, "Set1")[3],
                "Waterparel Zuidplaspolder" = RColorBrewer::brewer.pal(9, "Set1")[8])

f_kleur_ov <- colorFactor(palette = kleuren_ov, levels = names(kleuren_ov))

nieuwste_bestand <- function(pattern, pad = "data"){
  tibble(files =  list.files(path = pad, pattern = pattern, full.names = TRUE)) %>%
    mutate(tijd = file.info(files)$mtime) %>%
    filter(tijd == max(tijd)) %>%
    .[[1,1]]
}


kaart_overig <-
  st_read("data/overig_water_kaart.gpkg") %>%
  st_transform(crs = 4326) %>%
  basiskaart(type = "cartolight") %>%
  addPolylines(data = ws_grens, opacity = 1, color = "grey", weight = 2, label = "waterschapsgrens") %>%
  addPolygons(color = ~f_kleur_ov(naam_ovw), fillOpacity = 0.7, stroke = 0, label = ~naam_ovw, popup = ~naam_ovw) %>%
  addLegend(position = "topright", pal = f_kleur_ov, values = ~naam_ovw, opacity = 0.7, title = "Overig water") %>% 
  leaflet.extras::addFullscreenControl()

ekr_overig <- read_excel(nieuwste_bestand("EKRs overig water.xlsx")) %>%
  filter(!str_detect(ovw_naam, "Eendragtspolder|Krimpenerhout"))

ekr_edp <- read_excel(nieuwste_bestand("EKR's EDP Plas Dras.xlsx")) %>%
  mutate(ovw_naam = "Eendragtspolder plas-dras", ovw_code = "NL39_DOW_6") %>%
  filter(jaar >= rap_jaar - 2) %>%
  group_by(ovw_code, ovw_naam) %>%
  summarise(ekr = mean(ekr))


ekr_kriho <- read_excel(nieuwste_bestand("EKR's Krimpenerhout.xlsx")) %>%
  mutate(ovw_naam = "Zwemplas Krimpenerhout", ovw_code = "NL39_DOW_8") %>%
  filter(jaar >= rap_jaar - 2) %>%
  group_by(ovw_code, ovw_naam) %>%
  summarise(ekr = mean(ekr))

# cuts <- 2021 - c(0, 3, 6, 9 , 12, 15, 18, 21, 50) + 0.5

doelen_overig <- tibble::tribble(
  ~ovw_naam,                      ~ovw_code, ~doel_ekr_veg,
  "Stedelijk gebied",          "NL39_DOW_1",           0.3,
  "Weidegebied",               "NL39_DOW_2",           0.4,
  "Glastuinbouwgebied",        "NL39_DOW_3",           0.3,
  "Akkerbouwgebied",           "NL39_DOW_4",          0.35,
  "Natuurgebied",              "NL39_DOW_5",          0.45,
  "Eendragtspolder plas-dras", "NL39_DOW_6",           0.6,
  "Waterparel Zuidplaspolder", "NL39_DOW_7",          0.45,
  "Zwemplas Krimpenerhout",    "NL39_DOW_8",           0.5
)


overig_doelen <-
  doelen_overig %>%
  mutate(ovw_naam = fct_reorder(ovw_naam, ovw_code, .fun = last)) %>%
  mutate(doel = pmin(doel_ekr_veg / 0.6, 1),
         doelaanpassing = 1 - doel) %>%
  pivot_longer(cols = c(doel, doelaanpassing)) %>%
  ggplot(aes(value, fct_rev(ovw_naam), fill = fct_rev(name))) +
  geom_col() +
  scale_fill_manual(values = c(doel = blauw_m, doelaanpassing = grijs_m)) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0), labels = function(x) scales::percent(x, accuracy = 1)) +
  labs(x = "Hoogte doel t.o.v. standaard",
       y ="",
       title = "Doelen overig water - waterplanten") +
  hhskthema() +
  theme(axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        panel.grid.major.y =  element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "top",
        plot.title.position = "panel",
        plot.margin = margin(5.5, 15, 5.5, 5.5)) +
  guides(fill = guide_legend(title = "", reverse = TRUE))


overig_opgave <-
  ekr_overig %>%
  add_jaar() %>%
  filter(jaar >= rap_jaar - 2) %>%
  group_by(ovw_code, ovw_naam) %>%
  summarise(ekr = mean(ekr)) %>%
  ungroup() %>%
  bind_rows(ekr_edp, ekr_kriho) %>%
  left_join(doelen_overig) %>%
  mutate(`huidige toestand` = pmin(ekr / doel_ekr_veg, 1),
         doelgat = 1 - `huidige toestand`) %>%
  pivot_longer(c(`huidige toestand`, doelgat)) %>%
  mutate(ovw_naam = fct_reorder(ovw_naam, ovw_code, .fun = last)) %>%
  ggplot(aes(value, fct_rev(ovw_naam), fill = name)) +
  geom_col() +
  scale_fill_manual(values = c("huidige toestand" = blauw_m, doelgat = oranje_m)) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0), labels = function(x) scales::percent(x, accuracy = 1)) +
  labs(x = "Toestand ten opzichte van het doel",
       y ="",
       title = "Opgave overig water - waterplanten",
       caption = "Gemiddelde van de laatste 3 jaar") +
  hhskthema() +
  theme(axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        panel.grid.major.y =  element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "top",
        plot.title.position = "panel",
        plot.margin = margin(5.5, 15, 5.5, 2)) +
  guides(fill = guide_legend(title = "", reverse = TRUE))