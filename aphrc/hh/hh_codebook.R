library(openxlsx)

target_name <- paste0(file_prefix, "codebook", ".xlsx")
saveXlsx(codebook, target_name)

