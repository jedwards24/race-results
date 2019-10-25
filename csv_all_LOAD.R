################
# Imports all the csvs in "Results_CSV" folder and combines in a single tibble.
# A new variable is added (race time in seconds) and retired runners are removed into a separate
# table ("ret"). 
# Some other processing is done too.
################
library(tidyverse)
library(lubridate)
source("race_FUNC.R")

path_root <- "C:/Users/James/Dropbox/Mine/Personal/Running/Races&events/Results_CSV/"

files <- list.files(path_root)
files

race_id <- str_sub(files, 1, -5)
#race_names <- str_sub(files, 1, -9)
#race_years <- as.numeric(str_sub(files, -8, -5))
n_races <- length(files)
results <- vector('list', n_races)
for (i in 1 : n_races){
  cat(files[i], "\n")
  path <- str_c(path_root, files[i])
  results[[i]] <- read_csv(path, col_types = "cccc") #could use "ccct" here but will convert RET to NAs
  results[[i]] <- mutate(results[[i]], raceID = race_id[i])
}

data <- bind_rows(results)

# Remove runners that retire
ret <- data %>% filter(str_detect(time, "[^(\\d|:)]")) # test for presence of non-digit, non ":"
data <- data %>% filter(!str_detect(time, "[^(\\d|:)]"))  #could convert "time" to a time with "%>% type_convert"

# Add numerical time column (in seconds) and correct name case
data <- data %>% 
  rowwise %>% 
  mutate(seconds = as.integer(string_to_time(time))) %>%
  mutate(name = str_to_title(name)) %>%
  rowwise %>%
  mutate(name = apostrophe_name_title(name))

#standardise categories
data <- data %>%  rowwise %>% mutate(category = standardise_category(category))
#Add placings
data <- data %>% group_by(raceID) %>% mutate(place = min_rank(seconds))
#Category placings
data <- data %>% group_by(raceID, category) %>% mutate(cat_place = min_rank(seconds))
#Add % of winner's time
data <- data %>% group_by(raceID) %>% mutate(perc_winner = seconds / min(seconds))
#Add % of cat winner's time
data <- data %>% group_by(raceID, category) %>% mutate(perc_cat_winner = seconds / min(seconds))
#Add year of race and gender and sort
# Gender is "M", "L", or "Unknown"
data <- data %>% mutate(year = str_sub(raceID, -4, -1)) %>%
  mutate(gender = str_sub(category, 1, 1)) %>% 
  mutate (gender = ifelse(str_detect(gender, "M|L"), gender, "Unknown")) %>%
  ungroup %>% arrange(year, raceID, place)

# Save df as RDATA file
if(F){
  saveRDS(data, "data_processed/results_all.RDS")
}
