library(HHSKwkl)
library(tidyverse)
library(sf)
library(leaflet)
library(reactable)
library(glue)
library(twn)

rap_jaar <- 2024

fys_chem <- readRDS("data/fys_chem.rds") 
  

meetpunten <- readRDS("data/meetpunten.rds")
parameters <- readRDS("data/parameters.rds")
bio <- readRDS("data/biologie.rds") %>% HHSKwkl::add_jaar()
ws_grens <- st_read("data/ws_grens.gpkg", quiet = TRUE) %>% st_transform(crs = 4326)

meetnetten <- 
  readxl::read_excel("data/tabellen_monitoringsplan_OMS_03_01_2025.xlsx", "meetnetten") %>% 
  filter(!is.na(meetnet_code)) %>% 
  select(contains("meetnet"), mp = meetpunt_code, meetjaar) %>% 
  distinct()


blauwe_tekst <- function(tekst, grootte = NA){
  grootte <- if (is.na(grootte)) "" else glue::glue(";font-size:{grootte}px")
  glue::glue("<span style='color:#0079c2{grootte}'>{tekst}</span>")
}

fys_chem_sel <- 
  fys_chem %>% filter(year(datum) == rap_jaar) %>% 
  filter(!parnr %in% c(35, 900:999),
         !str_detect(mp, "^H_|^ADHOC_"),
         parnr < 9000,
         !parnr %in% c(500:599))

n_locs_fc <- 
  fys_chem_sel %>% summarise(n = n_distinct(mp)) %>% pull(n)

n_monsters_fc <- fys_chem_sel %>% summarise(n = n_distinct(mp, datum)) %>% pull(n)

n_metingen_fc <- fys_chem_sel %>% summarise(n = n_distinct(mp, datum, parnr)) %>% pull(n)

n_parameters_fc <- fys_chem_sel %>% summarise(n = n_distinct(parnr)) %>% pull(n)

groepen <- 
  parameters %>% 
  mutate(groep = case_when(
    parnr %in% c(1:99) ~ "Algemene stoffen",
    parnr %in% c(100:199) ~ "Veldwaarnemingen",
    parnr %in% c(200:299) ~ "Metalen",
    parnr %in% c(300:499) ~ "Zwemwatermetingen",
    parnr %in% c(2000:2399) ~ "Organische stoffen",
    .default = cluster
  )) %>% 
  select(parnr, groep, parnaamlang) 


tabel_chemie_metingen <-
  fys_chem_sel %>% 
  left_join(groepen) %>% 
  mutate(groep = fct_reorder(groep, parnr),
         parnr = fct_reorder(as.character(parnr), parnr)) %>% 
  # Alleen eerst letter naar upper
  mutate(parnaamlang = paste0(toupper(substr(parnaamlang, 1, 1)), substr(parnaamlang, 2, nchar(parnaamlang)))) %>% 
  # mutate(n = 1) %>% 
  group_by(groep, parnaamlang) %>% 
  summarise("Aantal metingen" = n(),
            "Aantal meetlocaties" = n_distinct(mp)) %>%
  ungroup() %>% 
  rename(Groep = groep, Stof = parnaamlang) %>% 
  reactable::reactable(filterable = TRUE, onClick = "expand", outlined = TRUE,
                       groupBy = "Groep",
                       columns = list(
                         Stof = colDef(),
                         `Aantal metingen` = colDef(aggregate = "sum", filterable = FALSE),
                         `Aantal meetlocaties` = colDef(aggregate = "max", filterable = FALSE)
                         )
                       )

bio_sel <- 
  bio %>% 
  select(-contains("stadium")) %>% 
  distinct() %>% 
  filter(jaar == rap_jaar - 1,
         !methode %in% c("VISNHA", "VISBM", "KRVEG", "VISBMCM", "VISNHACM")) %>% 
  filter(!naam %in% c("Geen kreeften", "Plantae", "Pisces"))

n_locs_bio <- bio_sel %>% summarise(n = n_distinct(mp)) %>% pull(n)

n_waarnemingen_bio <- bio_sel %>% summarise(n = n_distinct(mp, datum, naam)) %>% pull(n)

n_soorten_bio <- bio_sel %>% 
  mutate(naam = naam |> twn_voorkeurnaam() |> increase_taxonlevel("Species")) %>% 
  summarise(n = n_distinct(naam)) %>% pull(n)
  

taxongroepen <- 
  tibble::tribble(
    ~taxatype,                          ~Soortgroep,
    "DIATM",          "Kiezelwieren (diatomeeën)",
    "FYTPT",               "Algen (fytoplankton)",
    "MACAG",               "Algen (fytoplankton)",
    "MACEV", "Waterdiertjes (macro-evertebraten)",
    "MACFT",               "Planten (macrofyten)",
    "VISSN",                             "Vissen",
    "ZOOPT",         "Zoöplankton (watervlooien)"
  )

tabel_bio_waarnemingen <- 
  bio_sel %>% 
  mutate(nednaam = twn::twn_localname(naam)) %>% 
  left_join(taxongroepen) %>% 
  mutate(Soortgroep = ifelse(methode == "KR12" & taxatype == "MACEV", "Kreeften", Soortgroep)) %>% 
  group_by(Soortgroep, naam, nednaam) %>% 
  summarise("Aantal waarnemingen" = n(),
            "Aantal meetlocaties" = n_distinct(mp)) %>% 
  ungroup() %>% 
  rename(`Wetenschappelijke naam` = naam, `Nederlandse naam` = nednaam) %>% 

  reactable::reactable(filterable = TRUE, onClick = "expand", outlined = TRUE,
                       groupBy = "Soortgroep",
                       columns = list(
                         `Wetenschappelijke naam` = colDef(),
                         `Nederlandse naam` = colDef(),
                         `Aantal waarnemingen` = colDef(aggregate = "sum", filterable = FALSE),
                         `Aantal meetlocaties` = colDef(aggregate = "max", filterable = FALSE)
                       )
  )

meetnet_labels <- tibble::tribble(
  ~meetnet_nr,                 ~meetnet_label,
           1L,                "Basis meetnet",
           2L,            "Roulerend meetnet",
           3L,            "Meetnet zwemwater",
           5L, "Meetnet bestrijdingsmiddelen",
         101L,         "Meetnet KRW biologie",
         102L,         "Meetnet waterplanten",
         104L,             "Meetnet kreeften"
  )


meetpunt_link <- function(mp){
  glue("<a href='https://www.schielandendekrimpenerwaard.nl/kaart/waterkwaliteit/wkl_gegevens_op_kaart/{mp}.html' target='_blank'>Meetpunt {mp}</a>")
}

meetnetten_sel <-   
  meetnetten %>%
  filter(meetnet_nr %in% c(1,2,3,5,101,102,104)) %>%
  mutate(popup_tekst = case_when(
    meetnet_nr == 1 ~ glue("<b>Basis meetnet</b><br><b>Meetpunt: </b>{mp}<hr><b>Soort metingen:</b> Algemene stoffen<br><b>Frequentie: </b>Jaarlijks maandelijkse metingen<br><br>Zie ook: {meetpunt_link(mp)}"),
    meetnet_nr == 2 ~ glue("<b>Roulerend meetnet</b><br><b>Meetpunt: </b>{mp}<hr><b>Soort metingen:</b> Algemene stoffen<br><b>Frequentie: </b>Eens per 3 jaar maandelijkse metingen<br><b>Volgend meetjaar:</b> {meetjaar}<br><br>Zie ook: {meetpunt_link(mp)}"),
    meetnet_nr == 3 ~ glue("<b>Meetnet zwemwater</b><br><b>Meetpunt: </b>{mp}<hr><b>Soort metingen:</b> Blauwalgen en bacteriën<br><b>Frequentie: </b>Van mei tot en met september tweewekelijkse metingen<br><br>Zie ook: {meetpunt_link(mp)}"),
    meetnet_nr == 5 ~ glue("<b>Meetnet bestrijdingsmiddelen</b><br><b>Meetpunt: </b>{mp}<hr><b>Soort metingen:</b> Bestrijdingsmiddelen<br><b>Frequentie: </b>Jaarlijks 4 tot 6 metingen<br><br>Zie ook: {meetpunt_link(mp)}"),
    meetnet_nr == 101 ~ glue("<b>Meetnet KRW biologie</b><br><b>Meetpunt: </b>{mp}<hr><b>Soort metingen:</b> Algen, planten, waterdiertjes en vissen<br><b>Frequentie: </b>Eens per 3 jaar<br><b>Volgend meetjaar:</b> {meetjaar}"),
    meetnet_nr == 102 ~ glue("<b>Meetnet waterplanten</b><br><b>Meetpunt: </b>{mp}<hr><b>Soort metingen:</b> Waterplanten<br><b>Frequentie: </b>Eens per 3 jaar<br><b>Volgend meetjaar:</b> {meetjaar}"),
    meetnet_nr == 104 ~ glue("<b>Meetnet kreeften</b><br><b>Meetpunt: </b>{mp}<hr><b>Soort metingen:</b> Kreeften<br><b>Frequentie: </b>Eens per 3 jaar<br><b>Volgend meetjaar:</b> {meetjaar}"),
    .default = ""
                                 )) %>% 
  # mutate(label_tekst = case_when(
  #   meetnet_nr %in% 
  # ))
  left_join(select(meetpunten, mp, x, y), by = "mp") %>%
  left_join(meetnet_labels) %>% 
  filter(!is.na(x),
         x != 0) %>%
  sf::st_as_sf(coords = c("x", "y"), crs = 28992) %>%
  st_transform(crs = 4326)

pal <- leaflet::colorFactor(RColorBrewer::brewer.pal(3, "Accent"), domain = meetnetten_sel$meetjaar, na.color = blauw)  

kaart_meetnetten <- 
  meetnetten_sel %>% 
  leaflet() %>% 
  addProviderTiles("CartoDB.Positron", group = "Kaart") %>% 
  addPolylines(data = ws_grens, color = "#616161", weight = 3, label = ~"waterschapsgrens") %>%
  addCircleMarkers(group = ~meetnet_label, label = ~mp, popup = ~popup_tekst,
                   color = ~pal(meetjaar), stroke = FALSE, fillOpacity = 1, radius = 8) %>% 
  addLayersControl(baseGroups = meetnet_labels$meetnet_label, 
                   options = layersControlOptions(collapsed = FALSE), position = "topright") %>% 
  addLegend(pal = pal, values = meetnetten_sel$meetjaar, opacity = 1, na.label = "Jaarlijks", title = "Volgende meetjaar", position = "bottomright") %>% 
  leaflet.extras::addFullscreenControl() %>% 
  htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-list').prepend('<b>Meetnetten</b>');
        }
    ")

# leaflet() %>% 
#   addProviderTiles("CartoDB.Positron", group = "Kaart") %>% 
#   addPolylines(data = ws_grens, color = "#616161", weight = 3, label = ~"waterschapsgrens") %>%
#   addCircleMarkers(data = filter(meetnetten_sel, meetnet_nr == 1),  group = "Basis meetnet",
#                    color = ~pal(meetjaar), stroke = FALSE, fillOpacity = 1, radius = 8,
#                    label = ~mp, popup = ~popup_tekst) %>% 
#   addCircleMarkers(data = filter(meetnetten_sel, meetnet_nr == 2),  group = "Roulerend meetnet",
#                    color = ~pal(meetjaar), stroke = FALSE, fillOpacity = 1, radius = 8,
#                    label = ~mp, popup = ~popup_tekst) %>% 
#   addCircleMarkers(data = filter(meetnetten_sel, meetnet_nr == 3),  group = "Meetnet zwemwater",
#                    color = ~pal(meetjaar), stroke = FALSE, fillOpacity = 1, radius = 8,
#                    label = ~mp, popup = ~popup_tekst) %>% 
#   addCircleMarkers(data = filter(meetnetten_sel, meetnet_nr == 5),  group = "Meetnet bestrijdingsmiddelen",
#                    color = ~pal(meetjaar), stroke = FALSE, fillOpacity = 1, radius = 8,
#                    label = ~mp, popup = ~popup_tekst) %>% 
#   addCircleMarkers(data = filter(meetnetten_sel, meetnet_nr == 101),  group = "Meetnet KRW biologie",
#                    color = ~pal(meetjaar), stroke = FALSE, fillOpacity = 1, radius = 8,
#                    label = ~mp, popup = ~popup_tekst) %>% 
#   addCircleMarkers(data = filter(meetnetten_sel, meetnet_nr == 102),  group = "Meetnet waterplanten",
#                    color = ~pal(meetjaar), stroke = FALSE, fillOpacity = 1, radius = 8,
#                    label = ~mp, popup = ~popup_tekst) %>% 
#   addCircleMarkers(data = filter(meetnetten_sel, meetnet_nr == 104),  group = "Meetnet kreeften",
#                    color = ~pal(meetjaar), stroke = FALSE, fillOpacity = 1, radius = 8,
#                    label = ~mp, popup = ~popup_tekst) %>% 
#   addLayersControl(baseGroups = c("Basis meetnet", "Roulerend meetnet", "Meetnet zwemwater", "Meetnet bestrijdingsmiddelen", 
#                                   "Meetnet KRW biologie", "Meetnet waterplanten", "Meetnet kreeften"), 
#                    options = layersControlOptions(collapsed = FALSE), position = "topright") %>% 
#   addLegend(pal = pal, values = meetnetten_sel$meetjaar, opacity = 1, na.label = "Jaarlijks", title = "Meetjaar", position = "topleftright") %>% 
#   leaflet.extras::addFullscreenControl()

# 
# meetnetten_shared <- 
#   meetnetten %>% 
#   filter(meetnet_nr %in% c(1,2,3,5,101,102,103)) %>% 
#   left_join(select(meetpunten, mp, x, y), by = "mp") %>% 
#   filter(!is.na(x), 
#          x != 0) %>% 
#   sf::st_as_sf(coords = c("x", "y"), crs = 28992) %>% 
#   st_transform(crs = 4326) %>% 
#   mutate(meetnet_omschrijving = fct_reorder(meetnet_omschrijving, meetnet_nr)) %>% 
#   SharedData$new()
# 
# filter_meetnet <- 
#   crosstalk::filter_checkbox("meetnet", label = "Selecteer een meetnet", meetnetten_shared,
#                            group = ~meetnet_omschrijving, columns = 2)
# 
# kaart_meetnet <- 
# meetnetten_shared %>% 
#   basiskaart() %>% 
#   addCircleMarkers(label = ~paste(mp, "-", meetjaar))

# mp_shared <- meetpunten %>% filter(X != 0)%>% SharedData$new()
# 
# pal <- colorFactor(RColorBrewer::brewer.pal(9, "Set1"), mp_shared$data()$Meetpunttype)
# 
# crosstalk::filter_select("mpcode", "Selecteer een meetpuntcode", mp_shared, ~Meetpunt)
# 
# crosstalk::filter_checkbox("mp_type", label = "Selecteer een meetpunttype", mp_shared, 
#                            group = ~Meetpunttype, columns = 2)
# 
# crosstalk::filter_checkbox("gebied", label = "Selecteer een gebied", mp_shared, 
#                            group = ~Deelgebied, columns = 2)
# 
# crosstalk::filter_checkbox("fc_bio", label = "Selecteer soort metingen", mp_shared, 
#                            group = ~`Chemie of biologie`, columns = 2)
# 
# crosstalk::filter_slider("meetjaar", "Laatste meetjaar", mp_shared, column = ~`Laatste meetjaar`, step = 1, sep = "")



# kaart_meetnet <- function(meetnet){
#   meetpunten %>% 
#     right_join(filter(meetnetten, meetnet_code == meetnet), by = "mp") %>% 
#     sf::st_as_sf(coords = c("x", "y"), crs = 28992) %>% 
#     st_transform(crs = 4326) %>% 
#     basiskaart() %>% 
#     addPolylines(data = ws_grens, opacity = 1, color = "grey", weight = 3, label = "waterschapsgrens") %>% 
#     leaflet.extras::addFullscreenControl()
#     
# }
# n_locs_meetnet <- function(meetnet){
#   filter(meetnetten, meetnet_code == meetnet) %>% 
#   pull(mp) %>% unique() %>% length()
# }

# n_locs_meetnet("basis")

# meetnet_basis <- 
#   kaart_meetnet("basis") %>% 
#   addCircleMarkers(fillColor = blauw, fillOpacity = 1, radius = 8, 
#                    color = "#616161", opacity = 1, weight = 0, stroke = FALSE,
#                    popup = ~mp, label = ~mp)

# kaart_waterlichamen <-
#   waterlichamen %>%
#   left_join(popup_data, by = "nr") %>% #st_drop_geometry() %>%  View()
#   st_transform(crs = 4326) %>%
#   leaflet() %>%
#   leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group = "Kaart") %>%
#   leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Luchtfoto") %>%
#   leaflet::addLayersControl(baseGroups = c("Kaart", "Luchtfoto"),
#                             options = leaflet::layersControlOptions(collapsed = FALSE), position = "topleft") %>%
#   addPolylines(data = ws_grens, opacity = 1, color = "grey", weight = 2, label = "waterschapsgrens") %>%
#   addPolygons(weight = 4, color = ~pal(naam),
#               fillOpacity = 0.8, opacity = 0.8,
#               label = ~naam,
#               popup = ~popup_tekst,
#               highlightOptions = highlightOptions(color = blauw, bringToFront = TRUE, opacity = 1)) %>% 
#   leaflet.extras::addFullscreenControl()