#############
#This reads xc results from a pdf and converts to a csv. 
#############

library(pdftools)
library(tidyverse)
source("parse_FUNC.R")

path_root <- "C:/Users/James/Dropbox/Mine/Personal/Running/Races&events/"

files <- list.files(paste0(path_root, "xc_raw")) %>% 
  str_subset(".pdf")
n_files <- length(files)

for (i in c(1 : n_files)){
  file_name <- files[i]
  source("code_parse/xc_to_csv_DO.R")
}
