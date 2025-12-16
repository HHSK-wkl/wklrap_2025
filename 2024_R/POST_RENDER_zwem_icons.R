# dir.create("_book/images")

zwem_iconen_files <- list.files("images", pattern = "icon_zwemwater.*\\.png", full.names = TRUE)

file.copy(zwem_iconen_files, file.path("_book/images", basename(zwem_iconen_files)))