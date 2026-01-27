# dir.create("_book/images")

pdf_versie <- "pdf-versie/Rapportage-waterkwaliteit-2025.pdf"

file.copy(pdf_versie, file.path("_book/", basename(pdf_versie)))