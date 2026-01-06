library(tidyverse)
library(readxl)
library(HHSKwkl)

doelen <-
  readxl::read_excel("data-raw/waterlichamen_ekrs_en_doelen_2024.xlsx") %>% 
  select(type, nr, doelen, groep) 
  


ekrs <- 
  read_excel("data-raw/overzicht ekr nieuwe toetsing 2025 v11-07-2025.xlsx") %>% 
  pivot_longer(cols = starts_with("20"), names_to = "jaar", values_to = "ekr", values_drop_na = TRUE) %>% 
  rename_all(str_to_lower) %>%
  mutate(jaar = as.numeric(jaar)) %>% 
  arrange(type, nr, naam, jaar) %>% 
  group_by(type, nr, naam) %>% 
  mutate(ekr3 = slider::slide_dbl(ekr, ~round(mean(.x), digits = 3), .before = 2)) %>% 
  ungroup() %>% 
  mutate(naam = str_replace(naam, "t Weegje", "'t Weegje")) %>% 
  left_join(doelen) %>% 
  filter(jaar == max(jaar), .by = c(naam, type)) %>% 
  select(wl_code = nr, wl_naam = naam, wl_type = watertype, groep, type, , doel = doelen, jaar, ekr = ekr3) %>% 
  arrange(wl_code, type)

ekrs %>% 
  openxlsx::write.xlsx("data/waterlichamen_ekrs_en_doelen_2025.xlsx")
