# SETUP -------------------------------------------------------------------

library(tidyverse)
library(HHSKwkl)
library(glue)
# library(colorspace)
library(scales)
library(ggbeeswarm)
library(ggtext)
# library(plotly)
library(leaflet)

rap_jaar <- 2024

fys_chem <- readRDS("data/fys_chem.rds") %>% HHSKwkl::add_jaar()
meetpunten <- readRDS("data/meetpunten.rds")
parameters <- readRDS("data/parameters.rds")
toxiciteit <- readxl::read_excel("data/gbm_toxiciteit.xlsx", sheet = "SSDinfo")
ws_grens <- sf::st_read("data/ws_grens.gpkg", quiet = TRUE) %>% sf::st_transform(crs = 4326)

theme_set(hhskthema())

f_parnaam <- maak_opzoeker(parameters, parnr, parnaamlang)
f_aquopar <- maak_opzoeker(parameters, parnr, aquo_parcode)
f_landgebruik <- meetpunten %>% 
  mutate(landgebruik = str_to_sentence(landgebruik)) %>% 
  mutate(landgebruik = case_when(
    mp == "S_0504" ~ "Akkerbouw",
    mp == "S_0609" ~ "Glastuinbouw", # S_0609 is lastig toe te delen - lijkt deels op glas
    TRUE ~ landgebruik
  )) %>% 
  maak_opzoeker(mp, landgebruik)

blauwe_tekst <- function(tekst, grootte = NA){
  grootte <- if (is.na(grootte)) "" else glue::glue(";font-size:{grootte}px")
  glue::glue("<span style='color:#0079c2{grootte}'>{tekst}</span>")
}

oranje_tekst <- function(tekst, grootte = NA){
  grootte <- if (is.na(grootte)) "" else glue::glue(";font-size:{grootte}px")
  glue::glue("<span style='color:#C25100{grootte}'>{tekst}</span>")
}

data_gbm <- fys_chem %>%
  filter(parnr > 999, parnr < 2000, jaar <= rap_jaar, jaar > 2008) %>%
  group_by(mp, jaar, parnr) %>%
  filter(n() > 1) %>% # verwijder alle meetlocaties waar maar 1 keer is gemeten of stoffen die maar 1 keer zijn gemeten
  ungroup()

# voor transpermethrin kan de norm van permethrin worden gebruikt.
transpermethrin <- tibble(parnr = 1324, naam = "trans-permethrin", wns_code = NA_character_,
                        norm_JGM = NA_real_, norm_MAX = NA_real_, norm_P90 = 0.0002, min_norm = 0.0002)

formetanaat <- tibble(parnr = 1346, naam = "formetanaat", wns_code = "WNS5784",
                      norm_JGM = 0.11, norm_MAX = 0.11, norm_P90 = NA_real_, min_norm = 0.11)

normen <- HHSKwkl::import_normen_rivm(parameterdf = parameters) %>%
  bind_rows(transpermethrin) %>%
  bind_rows(formetanaat)

mspaf <- function(paf_vector){
  1 - prod(1 - paf_vector, na.rm = TRUE)
}

# direct aangepast in scale_y_continuous maakt de functie overbodig. De functie werkt ook niet correct meer.
# reverselog_trans <- function(base = 10, n = 5) {
#   trans <- function(x) -log(x, base)
#   inv <- function(x) base^(-x)
#   scales::trans_new(paste0("reverselog-", format(base)), trans, inv,
#                     scales::log_breaks(n = n, base = base),
#                     domain = c(1e-100, Inf))
# }


meer_of_minder <- function(waarde) {
  if (sign(waarde) == -1) tekst <- blauwe_tekst(paste(-waarde, "minder"), 20)
  else if (sign(waarde) == 1) tekst <-oranje_tekst(paste(waarde, "meer"), 20)
  else tekst <- blauwe_tekst("evenveel als")
  tekst
}

kleuren_functies_gbm <-
  c(Akkerbouw              = RColorBrewer::brewer.pal(12, "Set3")[6], # akkerbouw
    Boezem                 = RColorBrewer::brewer.pal(12, "Set3")[5],# boezem
    "Grasland - natuur"    = RColorBrewer::brewer.pal(12, "Set3")[11], #afvoer
    Glastuinbouw           = RColorBrewer::brewer.pal(12, "Set3")[3],
    "Grasland - agrarisch" = RColorBrewer::brewer.pal(12, "Set3")[7],
    Stedelijk              = RColorBrewer::brewer.pal(12, "Set3")[4],
    "Grote plassen"        = RColorBrewer::brewer.pal(12, "Set3")[1],
    "Grasland"             = RColorBrewer::brewer.pal(12, "Set3")[7])


# Toetsing en PAF berekeningen --------------------------------------------

toetsing <- data_gbm %>% HHSKwkl::toetsing_gbm(normen, factor_detectiegrens = 0.5) %>%
  filter(aantal > 1) # screeningswaarden wegfilteren; op 1 waarde is geen toetsing mogelijk

mspaf_mp <-
  data_gbm %>%
  filter(jaar > rap_jaar - 10) %>%
  mutate(paf_acuut = paf_gbm(f_aquopar(parnr),
                             concentratie = waarde,
                             detectiegrens = detectiegrens,
                             ssd_data = toxiciteit,
                             type_paf = "acuut"),
         paf_chronisch = paf_gbm(f_aquopar(parnr),
                                 concentratie = waarde,
                                 detectiegrens = detectiegrens,
                                 ssd_data = toxiciteit,
                                 type_paf = "chronisch")) %>%
  # afhandelen detectiegrenswaarden
  mutate(paf_acuut = ifelse(is.na(detectiegrens), paf_acuut, 0),
         paf_chronisch = ifelse(is.na(detectiegrens), paf_chronisch, 0)) %>%
  mutate(aantal_monsters = n_distinct(datum), .by = c(mp, jaar)) %>%
  # alleen de hoogste waarde telt mee in de msPAF
  filter(waarde == max(waarde), .by = c(mp, parnr, jaar)) %>%
  group_by(mp, aantal_monsters, jaar) %>%
  summarise(`Acute effecten` = mspaf(paf_acuut),
            `Chronische effecten` = mspaf(paf_chronisch)) %>%
  ungroup() %>%
  mutate(landgebruik = ifelse(mp == "S_0609", "Glastuinbouw", str_to_sentence(f_landgebruik(mp)))) %>%
  mutate(mp2 = glue("{landgebruik} -- {mp}")) %>%
  mutate(mp2 = fct_reorder(mp2, `Acute effecten`)) %>%
  pivot_longer(cols = c(`Acute effecten`, `Chronische effecten`), names_to = "type", values_to = "mspaf" ) %>%
  mutate(landgebruik = fct_reorder(landgebruik, mspaf, .desc = TRUE))

paf <-
  data_gbm %>%
  mutate(paf_acuut = paf_gbm(f_aquopar(parnr),
                             concentratie = waarde,
                             detectiegrens = detectiegrens,
                             ssd_data = toxiciteit,
                             type_paf = "acuut"),
         paf_chronisch = paf_gbm(f_aquopar(parnr),
                                 concentratie = waarde,
                                 detectiegrens = detectiegrens,
                                 ssd_data = toxiciteit,
                                 type_paf = "chronisch")) %>%
  # afhandelen detectiegrenswaarden
  mutate(paf_acuut = ifelse(is.na(detectiegrens), paf_acuut, 0),
         paf_chronisch = ifelse(is.na(detectiegrens), paf_chronisch, 0)) %>%
  group_by(mp, jaar, datum) %>%
  summarise(`Acute effecten` = mspaf(paf_acuut),
            `Chronische effecten` = mspaf(paf_chronisch)) %>%
  ungroup() %>%
  mutate(landgebruik = f_landgebruik(mp))


# aantallen GBM ----

aantal_stoffen <-
  toetsing %>%
  filter(aantal > aantal_det) %>%
  group_by(jaar, parnr) %>%
  summarise(max_factor = max(hoogste_overschrijding)) %>%
  ungroup() %>%
  mutate(categorie = ifelse(max_factor > 1, "Normoverschrijdend", "Aangetroffen")) %>%
  group_by(jaar, categorie) %>%
  summarise(aantal = n()) %>%
  group_by(jaar) %>%
  mutate(totaal = sum(aantal)) %>%
  ungroup() %>%
  mutate(tekst = case_when(
    categorie == "Aangetroffen" ~ glue("{totaal} verschillende gewasbeschermingsmiddelen aangetroffen"),
    categorie == "Normoverschrijdend" ~ glue("{aantal} normoverschrijdende gewasbeschermingsmiddelen"),
  )) %>%
  mutate(cat_fill = paste0(categorie, ifelse(jaar == rap_jaar, "", "_andere_jaren")))

aantal_gbm_aanwezig <-   aantal_stoffen %>% filter(jaar == rap_jaar, categorie == "Aangetroffen") %>% pull(totaal)

# verschil_gbm_aanwezig <-
#   aantal_stoffen %>%
#   filter(jaar >= rap_jaar - 3, jaar < rap_jaar, categorie == "Aangetroffen") %>%
#   pull(totaal) %>%
#   mean() %>%
#   round() %>%
#   {aantal_gbm_aanwezig - .} %>%
#   meer_of_minder()

aantal_gbm_overschrijdend <-  aantal_stoffen %>% filter(jaar == rap_jaar, categorie == "Normoverschrijdend") %>% pull(aantal)

# verschil_gbm_overschrijdend <-
#   aantal_stoffen %>%
#   filter(jaar >= rap_jaar - 3, jaar < rap_jaar, categorie == "Normoverschrijdend") %>%
#   pull(aantal) %>%
#   mean() %>%
#   round() %>%
#   {aantal_gbm_overschrijdend - .} %>%
#   meer_of_minder()

stoffen_per_monster <-
  data_gbm %>%
  filter(is.na(detectiegrens)) %>%
  # filter(f_landgebruik(mp) == "Glastuinbouw")
  left_join(select(normen, parnr, min_norm)) %>%
  mutate(categorie = case_when(
    is.na(min_norm)    ~ "Onder de norm",
    waarde > min_norm  ~ "Boven de norm",
    waarde <= min_norm ~"Onder de norm")) %>%
  group_by(mp, jaar, datum, categorie) %>%
  summarise(aantal = n()) %>%
  group_by(jaar, categorie) %>%
  summarise(aantal_per_cat = round(mean(aantal, na.rm = TRUE), digits = 1)) %>%
  mutate(cat_fill = paste0(categorie, ifelse(jaar == rap_jaar, "", "_andere_jaren"))) %>%
  group_by(jaar) %>%
  mutate(aantal_tot = sum(aantal_per_cat),
         tekst = case_when(
           categorie == "Onder de norm" ~ glue("Gemiddeld <b>{aantal_tot}</b> verschillende stoffen aanwezig"),
           categorie == "Boven de norm" ~ glue("Gemiddeld <b>{aantal_per_cat}</b> verschillende stoffen boven normwaarde")
         ))

gbm_per_monster_aanwezig <-   stoffen_per_monster %>% filter(jaar == rap_jaar, categorie == "Boven de norm") %>% pull(aantal_tot) %>% round(digits = 0)
gbm_per_monster_overschrijdend <-  stoffen_per_monster %>% filter(jaar == rap_jaar, categorie == "Boven de norm") %>% pull(aantal_per_cat) %>% round(digits = 0)


# Kaart overschrijdingen --------------------------------------------------

ind_overschr <- 
  toetsing %>%
  filter(aantal > aantal_det) %>% # dit filter verwijderd meetpunten waar nooit GBM worden gevonden
  # filter(normoverschrijding) %>%
  filter(jaar == rap_jaar) %>%
  group_by(mp) %>%
  slice_max(hoogste_overschrijding, n = 5) %>%
  filter(hoogste_overschrijding >= 0.1) %>%
  mutate(tekst_enkel = glue("{str_to_sentence(naam)}: {format(signif(hoogste_overschrijding, digits = 2), decimal.mark = ',')} x de norm")) %>%
  summarise(label_tekst_basis = glue_collapse(tekst_enkel, sep = "<br>"))

# Aanpassen voor verschillende categorieen overschrijdingen
pal <- colorFactor(palette = c(oranje_m, blauw, oranje, "#752E00"), 
                   domain = c("Geen normoverschrijding", "Beperkte normoverschrijding (1-10x)", "Grote normoverschrijding (10-100x)", "Zeer grote normoverschrijding (> 100x)"))

mspaf_label <- 
  mspaf_mp %>% 
  filter(jaar == rap_jaar) %>% 
  select(mp, type, mspaf) %>% 
  mutate(mspaf = percent(mspaf, accuracy = 0.1, decimal.mark = ",")) %>% 
  pivot_wider(names_from = type, values_from = mspaf)

kaart_overschrijdingen <- 
  toetsing %>%
  filter(jaar == rap_jaar) %>% 
  mutate(landgebruik = f_landgebruik(mp)) %>%
  left_join(ind_overschr, by = "mp") %>%
  group_by(jaar, mp, landgebruik) %>%
  left_join(mspaf_label) %>% 
  # mutate(`landgebruik 2015` = ifelse(`landgebruik 2015` == "Afvoer/gemaal", "Polderafvoer", `landgebruik 2015`)) %>%
  summarise(normoverschrijding = any(normoverschrijding, na.rm = TRUE),
            sno = sum(hoogste_overschrijding),
            sno_rond = format(signif(sno, digits = 3), decimal.mark = ","),
            sno_tekst = cut(sno, breaks = c(-1,1, 10, 100, 99999), labels = c("Geen normoverschrijding", "Beperkte normoverschrijding (1-10x)", "Grote normoverschrijding (10-100x)", "Zeer grote normoverschrijding (> 100x)"), ordered_result = TRUE),
            label_tekst = glue("<b>Meetpunt {first(landgebruik)}:</b> {first(mp)}<br><br>
                               <b>Opgetelde normoverschrijding:</b> {sno_rond} x<br>
                               <b>Acute toxiciteit:</b> {first(`Acute effecten`)}<br>  
                               <b>Chronische toxiciteit:</b> {first(`Chronische effecten`)}<br>
                               <br>
                               <b>Top 5 normoverschrijdende stoffen</b><br>
                               {first(label_tekst_basis)}")) %>%
  ungroup() %>% 
  left_join(select(meetpunten, mp ,x,y)) %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 28992) %>% 
  sf::st_transform(crs = 4326) %>% 
  basiskaart(type = "cartolight") %>% 
  addPolylines(data = ws_grens, opacity = 1, color = "grey", weight = 2, label = "waterschapsgrens") %>%
  addCircleMarkers(fillColor = ~pal(sno_tekst), stroke = TRUE, fillOpacity = 1, popup = ~label_tekst, 
                   opacity = 1, color = "#333333", weight = 1,
                   label = ~glue("De opgetelde normoverschrijding is {sno_rond} x")) %>% 
  addLegend(values = ~sno_tekst, pal = pal, opacity = 1, title = "Normoverschrijdingen") %>% 
  leaflet.extras::addFullscreenControl()

# Toxiciteit --------------------------------------------------------------

plot_mspaf_mp <-
  mspaf_mp %>%
  filter(jaar == rap_jaar) %>% 
  mutate(mp2 = fct_reorder2(mp2, desc(type), mspaf) |> fct_rev()) %>%
  # filter(aantal_monsters > 2) %>% # locaties met 2 monsters en 1 pakket eruit alleen in 2023!!!
  ggplot() +
  geom_col(aes(mspaf, mp2, fill = landgebruik), colour = "grey60", linewidth = 0.2) +
  geom_vline(xintercept = 0.005, colour = oranje, linetype = "dashed", linewidth = 0.8) + 
  scale_x_continuous(expand = expansion(c(0, 0.05)), labels = function(.x) scales::percent(.x, accuracy = 1)) +
  scale_fill_manual(values = kleuren_functies_gbm) +
  facet_wrap(~type, scales = "free_x") +
  labs(title = "Hoe schadelijk zijn de gewasbeschermingsmiddelen?",
       subtitle = glue("per locatie in {rap_jaar}"),
       x = "Percentage aangetaste soorten",
       y = "Meetlocatie",
       fill = "") +
  thema_hor_bar +
  theme(legend.position = "bottom",
        plot.subtitle = element_text(face = "italic"),
        panel.grid.major.x = element_line(linewidth = 0.8),
        axis.title.y.left = element_text(margin = margin(r = 10)),
        panel.spacing = unit(2, "lines"),
        axis.text.y = element_text(hjust = 0))

plot_mspaf_tijd <- 
  mspaf_mp %>% 
  filter(type == "Acute effecten") %>% 
  mutate(mspaf_i = 1 / mspaf) %>% 
  filter(mspaf_i < 1000) %>% 
  mutate(landgebruik = fct_reorder(landgebruik, mspaf_i)) %>% 
  ggplot(aes(jaar,  mspaf)) +
  geom_hline(yintercept = c(0.10, 0.005), colour = oranje, linetype = rep(c(1,2), times = 4)) +
  ggbeeswarm::geom_quasirandom(width = 0.15, colour = "grey20") +
  scale_y_continuous(trans = c("log10"), limits = c(0.001, 0.6), breaks = breaks_log(n =6),
                     labels = function(x) scales::percent(x, decimal.mark = ",", accuracy = 0.1)) +
  # scale_y_continuous(trans = c("log10", "reverse"), breaks = breaks_log(n = 6), 
  #                    labels = scales::label_number(prefix = "1 op de ", big.mark = ".", accuracy = 1),
  #                    sec.axis = sec_axis(trans = ~ 1 / ., name = "msPAF acuut", breaks = breaks_log(n = 6),
  #                                        labels = function(x) scales::percent(x, decimal.mark = ",", accuracy = 0.1))) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(rap_jaar - 9.3, rap_jaar), guide = guide_axis(n.dodge = 2)) +
  facet_wrap(~landgebruik, ncol = 2, scales = "free") +
  labs(title = "Welk deel van de soorten wordt aangetast?",
       subtitle = "Acute effecten per locatie",
       x = "",
       y = "Aandeel aangetaste soorten") +
  thema_line_facet +
  theme(plot.subtitle = element_text(face = "italic"),
        axis.text.y = element_text(hjust = 1),
        panel.grid.major.x = element_blank(),
        axis.ticks.x = element_blank(), 
        strip.background = element_blank(),
        panel.spacing = unit(20, "points")
        
        ) +
  NULL

# plot_mspaf_tijd <-
#   paf %>%
#   filter(jaar >= rap_jaar - 10) %>%
#   group_by(jaar) %>%
#   mutate(n = n(),
#          jaar2 = glue("{jaar}\n\n n = {n}")) %>%
#   ungroup() %>%
#   mutate(mspaf_i = 1 / `Acute effecten`) %>%
#   filter(mspaf_i < 100000) %>%
#   ggplot(aes(jaar2, mspaf_i)) +
#   ggbeeswarm::geom_quasirandom(width = 0.3, colour = "grey30") +
#   scale_y_continuous(trans = c("log10", "reverse"), breaks = breaks_log(n = 8), 
#                      labels = scales::label_number(prefix = "1 op de ", big.mark = ".", accuracy = 1),
#                      sec.axis = sec_axis(trans = ~ 1 / ., name = "msPAF acuut", breaks = breaks_log(n = 8),
#                                          labels = function(x) scales::percent(x, decimal.mark = ",", accuracy = 0.001))) +
#   # scale_x_continuous(breaks = scales::pretty_breaks(n = 14)) +
#   geom_hline(yintercept = c(10, 200), colour = oranje, linetype = c(1,2)) +
#   labs(title = "Welk deel van de soorten wordt aangetast?",
#        subtitle = "Acute effecten per monster",
#        x = "",
#        y = "Aandeel aangetaste soorten") +
#   hhskthema() +
#   theme(plot.subtitle = element_text(face = "italic"),
#         axis.text.y = element_text(hjust = 1)) +
#   NULL

# plot_mspaf_tijd_chronisch <-
#   paf %>%
#   filter(jaar >= rap_jaar - 10) %>%
#   group_by(jaar) %>%
#   mutate(n = n(),
#          jaar2 = glue("{jaar}\n\n n = {n}")) %>%
#   ungroup() %>%
#   mutate(mspaf_i = 1 / `Chronische effecten`) %>%
#   filter(mspaf_i < 100000) %>%
#   ggplot(aes(jaar2, mspaf_i)) +
#   ggbeeswarm::geom_quasirandom(width = 0.3, colour = "grey30") +
#   scale_y_continuous(trans = c("log10", "reverse"), breaks = breaks_log(n = 8), 
#                      labels = scales::label_number(prefix = "1 op de ", big.mark = ".", accuracy = 1),
#                      sec.axis = sec_axis(trans = ~ 1 / ., name = "msPAF chronisch", breaks = breaks_log(n = 8),
#                                          labels = function(x) scales::percent(x, decimal.mark = ",", accuracy = 0.001))) +
#   # scale_x_continuous(breaks = scales::pretty_breaks(n = 14)) +
#   geom_hline(yintercept = c(20, 200), colour = oranje, linetype = c(1,2)) +
#   labs(title = "Welk deel van de soorten wordt aangetast?",
#        subtitle = "Chronische effecten per monster",
#        x = "",
#        y = "Aandeel aangetaste soorten") +
#   hhskthema() +
#   theme(plot.subtitle = element_text(face = "italic"),
#         axis.text.y = element_text(hjust = 1)) +
#   NULL

# % van alle toetsingen ---------------------------------------------------

plot_overschr_freq <-
  toetsing %>% 
  filter(jaar >= rap_jaar - 10) %>% 
  mutate(landgebruik = str_to_sentence(f_landgebruik(mp))) %>%
  mutate(landgebruik = case_when(
    mp == "S_0504" ~ "Akkerbouw",
    mp == "S_0609" ~ "Glastuinbouw", # S_0609 is lastig toe te delen - lijkt deels op glas
    TRUE ~ landgebruik
  )) %>%
  group_by(jaar, landgebruik) %>%
  summarise(n_toetsingen = n(),
            n_overschrijdingen = sum(normoverschrijding),
            fractie = n_overschrijdingen / n_toetsingen) %>%
  mutate(landgebruik = fct_reorder(landgebruik, fractie, .desc = TRUE)) %>%
  filter(!is.na(landgebruik), landgebruik != "Stedelijk") %>%
  ggplot(aes(jaar, fractie)) +
  geom_smooth(se = FALSE, method = "lm", linetype = "dashed", colour = blauw_l) +
  geom_line(colour = blauw, linewidth = 1) +
  geom_point(shape = 19, size = 2, colour = blauw) +
  facet_wrap(~landgebruik, scales = "free", ncol = 2) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 0.035), labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(limits = c(rap_jaar - 10, rap_jaar), breaks = scales::breaks_pretty(6)) +
  labs(title = "Hoe vaak wordt de norm overschreden?",
       subtitle = "per type landgebruik",
       x = "",
       y = "% normoverschrijdingen") +
  thema_line_facet +
  theme(panel.spacing = unit(25, "points"))

# Niet toetsbare stoffen -----------------------------------------

f_par_werking <-
  readRDS("data/gbm_toelating_werking.rds") %>% 
  select(parnr, werking) %>% 
  maak_opzoeker()


tabel_niet_toetsbaar <-
  data_gbm %>%
  anti_join(normen, by = "parnr") %>%
  filter(jaar == rap_jaar) %>%
  mutate(Naam = str_to_sentence(f_parnaam(parnr))) %>% 
  # mutate(`Soort stof` = f_par_werking(parnr)) %>% 
  filter(any(is.na(detectiegrens)), .by = Naam) %>% 
  summarise(n = n(),
            n_aanwezig = sum(is.na(detectiegrens)),
            `Aantal keer aangetroffen` = glue("{n_aanwezig} ({percent(n_aanwezig / n)})"),
            .by = c(Naam)) %>% 
  select(-n, -n_aanwezig) %>% 
  
  bind_cols(tibble(`Soort stof` = c("Fungicide (metaboliet)", rep("Insecticide", 3)))) %>% #, Bijzonderheden = c("", "Giftig voor bijen", "Afbraak product van tolylfluanide"))) %>%
  knitr::kable(align = "lrl")



# NO-CHUNK PLOT SNO -------------------------------------------------------

# het is de vraag of de factoren onder de norm meegewogen moeten worden.

# ind_overschr <- toetsing %>%
#   filter(aantal > aantal_det) %>% # dit filter verwijderd meetpunten waar nooit GBM worden gevonden
#   # filter(normoverschrijding) %>%
#   filter(jaar == rap_jaar) %>%
#   group_by(mp) %>%
#   slice_max(hoogste_overschrijding, n = 5) %>%
#   filter(hoogste_overschrijding >= 0.1) %>%
#   mutate(tekst_enkel = glue("{str_to_sentence(naam)}: {signif(hoogste_overschrijding, digits = 3)} x de norm")) %>%
#   summarise(label_tekst_basis = glue_collapse(tekst_enkel, sep = "<br>"))
# 
# sno_per_mp <-
#   toetsing %>%
#   filter(aantal > aantal_det) %>% # dit filter verwijderd meetpunten waar nooit GBM worden gevonden
#   # filter(normoverschrijding) %>%
#   filter(jaar == rap_jaar) %>%
#   left_join(ind_overschr, by = "mp") %>%
#   # left_join(meetpunten) %>%
#   mutate(landgebruik = f_landgebruik(mp)) %>%
#   mutate(landgebruik = case_when(
#     mp == "S_0504" ~ "Akkerbouw",
#     mp == "S_0609" ~ "Glastuinbouw", # S_0609 is lastig toe te delen - lijkt deels op glas
#     TRUE ~ landgebruik
#   )) %>%
#   group_by(jaar, mp, landgebruik) %>%
#   # mutate(`landgebruik 2015` = ifelse(`landgebruik 2015` == "Afvoer/gemaal", "Polderafvoer", `landgebruik 2015`)) %>%
#   summarise(sno = sum(hoogste_overschrijding),
#             sno_rond = signif(sno, digits = 3),
#             label_tekst = glue("<b>Meetpunt {first(landgebruik)}:</b> {first(mp)}<br><b>Opgetelde overschrijding:</b> {sno_rond} x<br><br><b>Top 5</b><br>{first(label_tekst_basis)}")) %>%
#   ungroup()
#   # left_join(meetpunten)
# 
# 
# 
# grafiek_SNO <- 
#   sno_per_mp %>% 
#   ggplot(aes(sno, "cat", 
#              fill = fct_reorder(landgebruik, sno, .desc = TRUE, .fun = max),
#              text = label_tekst)) +
#   geom_quasirandom(bandwidth = 0.3, shape = 21, size = 5) +
#   # geom_beeswarm(shape = 21, size = 5, side = 1, cex = 5) +
#   scale_fill_manual(values = kleuren_functies_gbm, name = "Landgebruik") +
#   # scale_y_continuous(limits = c(0.9, 2)) +
#   scale_x_log10(breaks = scales::breaks_log(10), limits = c(1,500)) +
#   coord_cartesian(ylim = c(0.99,1.5)) +
#   labs(title = "Hoe ernstig zijn de normoverschrijdingen?",
#        subtitle = "per meetpunt",
#        y = "",
#        x = "Opgetelde normoverschrijding (logaritmisch)",
#       ) +
#   theme(axis.line.y = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.title.y = element_blank(),
#         panel.grid.major.y = element_blank(),
#         legend.position = "bottom")
# 
# 
# plotly_SNO <- 
#   grafiek_SNO %>%
#   plotly::ggplotly(tooltip = "text") %>%
#   plotly::config(displayModeBar = FALSE) %>%
#   plotly::layout(dragmode = FALSE,
#                  title = list(x = "0"),
#                  hoverlabel = list(align = "left"))

# grafiek_SNO_old <-
#   sno_per_mp %>%
#   mutate(landgebruik = str_to_sentence(landgebruik)) %>%
#   # mutate(`landgebruik 2015` = ifelse(`landgebruik 2015` == "Afvoer/gemaal", "Polderafvoer", `landgebruik 2015`)) %>%
#   ggplot(aes(x = fct_reorder(mp, sno, .desc = TRUE),
#              y = sno,
#              fill = fct_reorder(landgebruik, sno, .desc = TRUE, .fun = max),
#              text = label_tekst)) +
#   geom_col(colour = "grey60") +
#   scale_x_discrete(labels = NULL) +
#   # scale_y_log10(breaks = scales::breaks_log(7), #limits = c(0.42, NA),
#   #               labels = scales::label_number(big.mark = "", digits = 1, drop0trailing = TRUE)) +
#   scale_fill_manual(values = kleuren_functies_gbm) +
#   labs(title = "Hoe ernstig zijn de normoverschrijdingen?",
#        x = "",
#        y = "Opgetelde normoverschrijding (logaritmisch)",
#        caption = "Elke staaf vertegenwoordigd één meetpunt") +
#   guides(fill = guide_legend(title = "") ) +
#   thema_vert_bar +
#   theme(axis.line.x = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.line.y = element_line(colour = "grey60"),
#         axis.ticks.y = element_line(colour = "grey60"),
#         # plot.margin = margin(5.5, 15, 5.5, 5.5),
#         plot.title = element_markdown(hjust = 0),
#         panel.grid.major.y = element_line()
#         )
# 
# plotly_SNO_old <-
#   grafiek_SNO %>%
#   plotly::ggplotly(tooltip = "text") %>%
#   plotly::config(displayModeBar = FALSE) %>%
#   plotly::layout(dragmode = FALSE,
#                  title = list(x = "0"),
#                  hoverlabel = list(align = "left"))






# kaart GBM-locs ---------------------------------------------------------------



# pal <- colorFactor(kleuren_functies_gbm, names(kleuren_functies_gbm), names(kleuren_functies_gbm))
# 
# meetpunten_gbm <- 
#   data_gbm %>% 
#   filter(jaar == rap_jaar) %>% 
#   select(mp) %>% 
#   distinct() %>% 
#   mutate(landgebruik = f_landgebruik(mp))
# 
# kaart_gbm <- 
#   meetpunten_gbm %>% 
#   left_join(select(meetpunten, mp ,x, y)) %>% 
#   filter(x > 0) %>% 
#   sf::st_as_sf(coords = c("x", "y"), crs = 28992) %>% 
#   sf::st_transform(crs = 4326) %>% 
#   basiskaart() %>% 
#   addCircleMarkers(label = ~mp, 
#                    fillColor = ~pal(landgebruik), fillOpacity = 1, 
#                    opacity = 1, color = "black",
#                    weight = 1, radius = 8) %>% 
#   addLegend("topright", pal = pal, values = ~landgebruik,
#             title = "Type landgebruik",
#             opacity = 1
#   )



# Bijlage plots -----------------------------------------------------------

# plot_aantal_stoffen <-
#   aantal_stoffen %>%
#   ggplot(aes(as.character(jaar), aantal, fill = cat_fill, text = tekst)) +
#   geom_col(width = 0.8, colour = "grey60", size = 0.2) +
#   scale_fill_manual(values = c(Aangetroffen = blauw_m, Normoverschrijdend = oranje_m,
#                                Aangetroffen_andere_jaren = blauw_l, Normoverschrijdend_andere_jaren = oranje_l),
#                     breaks = c("Aangetroffen", "Normoverschrijdend")) +
#   scale_y_continuous(limits = c(0, 95), expand = c(0,0)) +
#   labs(title = "Hoeveel verschillende stoffen worden er aangetroffen?",
#        y = "Aantal stoffen",
#        x = "") +
#   guides(fill = guide_legend(title = NULL)) +
#   thema_vert_bar +
#   theme(axis.line.x = element_line(colour = "grey60"))
# 
# 
# plotly_aantal_stoffen <- ggplotly(plot_aantal_stoffen, tooltip = "text") %>%
#   plotly::config(displayModeBar = FALSE) %>%
#   plotly::layout(dragmode = FALSE,
#                  title = list(x = "0"),
#                  legend = list(orientation= "h", yanchor = "top"))
# 
# # LET OP legenda wordt gewijzigd obv positie
# if (plotly_aantal_stoffen$x$data[[2]]$legendgroup == "Aangetroffen_andere_jaren") plotly_aantal_stoffen$x$data[[2]]$showlegend <- FALSE
# if (plotly_aantal_stoffen$x$data[[4]]$legendgroup == "Normoverschrijdend_andere_jaren") plotly_aantal_stoffen$x$data[[4]]$showlegend <- FALSE
# 
# 
# # NO-CHUNK ---- gewasbeschermingsmiddelen-gem-stoffen-per-monster-grafiek ----
# 
# stoffen_per_monster <-
#   data_gbm %>%
#   filter(is.na(detectiegrens)) %>%
#   # filter(f_landgebruik(mp) == "Glastuinbouw")
#   left_join(select(normen, parnr, min_norm)) %>%
#   mutate(categorie = case_when(
#     is.na(min_norm)    ~ "Onder de norm",
#     waarde > min_norm  ~ "Boven de norm",
#     waarde <= min_norm ~"Onder de norm")) %>%
#   group_by(mp, jaar, datum, categorie) %>%
#   summarise(aantal = n()) %>%
#   group_by(jaar, categorie) %>%
#   summarise(aantal_per_cat = round(mean(aantal, na.rm = TRUE), digits = 1)) %>%
#   mutate(cat_fill = paste0(categorie, ifelse(jaar == rap_jaar, "", "_andere_jaren"))) %>%
#   group_by(jaar) %>%
#   mutate(aantal_tot = sum(aantal_per_cat),
#          tekst = case_when(
#            categorie == "Onder de norm" ~ glue("Gemiddeld <b>{aantal_tot}</b> verschillende stoffen aanwezig"),
#            categorie == "Boven de norm" ~ glue("Gemiddeld <b>{aantal_per_cat}</b> verschillende stoffen boven normwaarde")
#          ))
# 
# plot_per_monster <-
#   stoffen_per_monster %>%
#   ggplot(aes(factor(jaar), aantal_per_cat, fill = fct_rev(cat_fill), text = tekst)) +
#   geom_col(width = 0.8, colour = "grey60", size = 0.2) +
#   scale_fill_manual(values = c("Onder de norm" = blauw_m, "Boven de norm" = oranje_m,
#                                "Onder de norm_andere_jaren" = blauw_l, "Boven de norm_andere_jaren" = oranje_l),
# 
#                     breaks = c("Onder de norm" , "Boven de norm"),
#                     position = "left") +
#   scale_y_continuous(expand = c(0, 0)) +
#   labs(title = "Hoeveel gewasbeschermingsmiddelen zitten er in het water?",
#        y = "Aantal stoffen",
#        x = "",
#        caption = "") +
#   guides(fill = guide_legend(title = "")) +
#   thema_vert_bar +
#   theme(plot.subtitle = element_text(face = "italic")) +
#   theme(axis.line.x = element_line(colour = "grey60"))
# 
# 
# plotly_per_monster <- ggplotly(plot_per_monster, tooltip = "text") %>%
#   plotly::config(displayModeBar = FALSE) %>%
#   plotly::layout(dragmode = FALSE,
#                  title = list(x = "0"),
#                  legend = list(orientation= "h", yanchor = "top"))
# 
# # LET OP legenda wordt gewijzigd obv positie
# if (plotly_per_monster$x$data[[1]]$legendgroup == "Onder de norm_andere_jaren") plotly_per_monster$x$data[[1]]$showlegend <- FALSE
# if (plotly_per_monster$x$data[[3]]$legendgroup == "Boven de norm_andere_jaren") plotly_per_monster$x$data[[3]]$showlegend <- FALSE
# 
# 
# # NO-CHUNK ---- top10-aangetroffen-gbm ----
# 
# meest_aangetroffen_stoffen <-
# 
#   data_gbm %>%
#   filter(jaar == rap_jaar) %>%
#   left_join(normen, by = "parnr") %>%
#   group_by(naam) %>%
#   summarise(aantal_gemeten      = n(),
#             aantal_aangetroffen = sum(is.na(detectiegrens)),
#             aantal_boven_norm   = sum(is.na(detectiegrens) & waarde > min_norm)) %>%
#   mutate(naam = str_to_sentence(naam),
#          fractie_aangetroffen   = aantal_aangetroffen / aantal_gemeten,
#          fractie_boven_norm     = aantal_boven_norm / aantal_gemeten,
#          fractie_aangetroffen_ex= fractie_aangetroffen - fractie_boven_norm) %>%
#   arrange(desc(aantal_aangetroffen)) %>%
#   filter(aantal_gemeten > 3) %>% # om te voorkomen dat zelden gemeten stoffen het beeld bepalen (o.a. glyfosaat)
# 
#   pivot_longer(cols = c(fractie_boven_norm, fractie_aangetroffen_ex), names_to = "groep", values_to = "waarde") %>%
#   mutate(label_tekst = case_when(
#     groep == "fractie_aangetroffen_ex" ~ glue("{naam} is in {aantal_aangetroffen} van de {aantal_gemeten} monsters aangetroffen."),
#     groep == "fractie_boven_norm" ~ glue("{naam} is in {aantal_boven_norm} van de {aantal_gemeten} monsters boven de normwaarde gemeten."),
#     TRUE ~ "ERROR"
#   ),
#   label_tekst2 = glue("({aantal_aangetroffen} van {aantal_gemeten})")
#   ) %>%
#   # select(-contains("aantal")) %>%
#   top_n(n = 20, wt = fractie_aangetroffen) %>%
#   mutate(naam = fct_reorder(naam, fractie_aangetroffen)) %>%
#   mutate(groep2 = ifelse(groep == "fractie_boven_norm", "Boven de norm", "Onder de norm"))
# 
# # subtitel_tekst <- glue("{blauwe_tekst(meest_aangetroffen_stoffen$naam[[1]])} zit in {blauwe_tekst(scales::percent(meest_aangetroffen_stoffen$fractie_aangetroffen[[1]]))} van alle monsters")
# 
# plot_top10 <-
#   meest_aangetroffen_stoffen %>%
#   ggplot(aes(waarde, naam, fill = fct_rev(groep2), text = label_tekst)) +
#   geom_col(width = 0.8, colour = "grey60", size = 0.2) +
#   # geom_label(aes(label = label_tekst2), colour = "white", x = 0.2) +
#   scale_fill_manual(values = c("Onder de norm" = blauw_m, "Boven de norm" = oranje_m)) +
#   scale_x_continuous(expand = c(0, 0), limits = c(0,1), labels = scales::percent_format(), position = "top") +
#   labs(title = "Welke gewasbeschermingsmiddelen worden het vaakst aangetroffen?",
#        # subtitle = subtitel_tekst,
#        y = "",
#        x = "% van alle metingen",
#        caption = "") +
#   guides(fill = guide_legend(title = "", reverse = TRUE)) +
#   thema_hor_bar +
#   theme(legend.position = "top",
#         panel.grid.major.x = element_line(size = 1),
#         plot.subtitle = element_markdown(),
#         plot.margin = margin(t = 5.5, r = 15, b = 5.5, l = 5.5))

