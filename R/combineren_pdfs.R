library(pdftools)
library(tidyverse)

list.files("pdf-versie/delen 30-1-2026/", full.names = TRUE) %>% 
  pdftools::pdf_combine(output = "pdf-versie/Rapportage-waterkwaliteit-2025.pdf")

