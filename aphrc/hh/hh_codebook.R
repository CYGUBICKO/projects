# This script uses saveXlsx function to save an .xlsx copy of codebook
library(openxlsx)

target_name <- paste0(file_prefix, "codebook", ".xlsx")
saveXlsx(codebook, target_name)

# rdnosave
