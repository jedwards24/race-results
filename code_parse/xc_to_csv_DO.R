#############
#This reads xc results from a pdf and converts to a csv. 
#############

library(pdftools)
library(tidyverse)
library(readxl)
source("xc_FUNC.R")

path_root <- "C:/Users/James/Dropbox/Mine/Personal/Running/Races&events/"

# pdf files
files <- list.files(paste0(path_root, "xc_raw")) %>% 
  str_subset(".pdf")
n_files <- length(files)

for (i in c(1 : n_files)){
  #read pdf
  file_name <- files[i]
  path_name <- str_c(path_root, "xc_raw/", file_name)
  lines <- pdf_text(path_name) %>% 
    str_split("\n") %>% unlist
  
  #process
  text_tbl <- xc_label_lines(lines)
  race_tbl <- xc_category_results(text_tbl)
  place_tbl <-  xc_to_column(race_tbl)
  sm <- xc_split_results(place_tbl)
  
  #save
  files_root <- str_sub(file_name, 1, -5) #remove ".pdf"
  csv_name <- str_c(path_root, "xc_csv/", files_root, ".csv")
  headings <- names(sm)
  write_csv(sm, csv_name)
  cat(files_root, "processed and saved\n")
}

# text file -----------
file_name <- "lancs_witton20.txt"
path_name <- str_c(path_root, "xc_raw/", file_name)
place_tbl <- readLines(path_name) %>% 
  tibble(text = .) %>% 
  slice(-1) %>% 
  mutate(text = str_trim(text)) %>% 
  mutate(text = str_remove_all(text, "\\?"))

sm <- xc_split_results(place_tbl)
prinf(sm)
#save
files_root <- str_sub(file_name, 1, -5) #remove ".txt"
csv_name <- str_c(path_root, "xc_csv/", files_root, ".csv")
headings <- names(sm)
write_csv(sm, csv_name)
cat(files_root, "processed and saved\n")

# Northern15 (excel) -----------
file_name <- "northern15.xlsx"
path_name <- str_c(path_root, "xc_raw/", file_name)
dt <- read_xlsx(path_name) #reads time as a datetime
?read_xlsx2

# Northern15 (excel) -----------
file_name <- "northern15.csv"
path_name <- str_c(path_root, "xc_raw/", file_name)
dt <- read_csv(path_name) #reads time as time 
dt2 <- dt %>% 
  unite("name", Forename, Surname, sep = " ") %>% 
  mutate(time = as.character(Time)) %>% 
  rename(place = Pos,
         num = No,
         club = Club) %>% 
  mutate(cat = "none",
         cat_place = 0,
         non_count = FALSE) %>% 
  select(place, num, name, cat, cat_place, club, time, non_count)
dt2
#save
csv_name <- str_c(path_root, "xc_csv/", file_name)
#headings <- names(dt2)
write_csv(dt2, csv_name)
cat(file_name, "processed and saved\n")



#checks ----------
# NAs, empty strings
# contiguous places 1:n
# cat in U20, V40 etc.
# num all digits
# times appropriate
library(edwards)
count_nas(sm3)
count_matches(sm3, "")

# One NA time in hyndburn1314
count_nas(data)
filter(data, is.na(perc_winner)) %>% pull(race_id)
filter(data, race_id == "hyndburn1314", place %in% 228:230)
