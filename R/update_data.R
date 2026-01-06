# Script om data op te halen die online beschikbaar is

library(HHSKwkl)
library(tidyverse)
library(sf)


HHSKwkl::download_data(c("fys_chem.rds", "meetpunten.rds", "parameters.rds", "normen.rds", "biologie.rds"))

# HHSKwkl::download_data("fys_chem.rds")

f_krw_type <-
  tibble::tribble(
    ~code,                                ~omsch,
    "M10",         "Laagveen vaarten en kanalen",
    "M14",    "Grote ondiepe gebufferde plassen",
    "M1a",             "Zoete gebufferde sloten",
    "M20",  "Matig grote diepe gebufferde meren",
    "M27", "Matig grote ondiepe laagveenplassen",
    "M3",       "Gebufferde (regionale) kanalen",
    "M30",                 "Zwak brakke wateren",
    "M8",           "Gebufferde laagveensloten"
  ) %>% maak_opzoeker(2,1)

waterlichamen <- 
  get_open_gisdata("krw_2022_2027") %>% 
  rename(wl_code = CODE, wl_naam = NAAM) %>% 
  filter(!is.na(wl_code)) %>% 
  mutate(wl_type = f_krw_type(TYPOLOGIEHUIDIG)) %>%
  arrange(wl_code) %>% 
  relocate(wl_code, wl_naam, wl_type) 

st_write(waterlichamen, "data/waterlichamen.gpkg")
