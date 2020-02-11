################
# Imports all the csvs in "Results_CSV" folder and combines in a single tibble.
# A new variable is added (race time in seconds) and retired runners are removed into a separate
# table ("ret"). 
# Some other processing is done too.
################
library(tidyverse)
library(edwards)
source("race_FUNC.R")

path_root <- "C:/Users/James/Dropbox/Mine/Personal/Running/Races&events/xc_csv/"

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
  results[[i]] <- read_csv(path, col_types = "iicciccl") %>% 
    mutate(race_id = race_id[i])
}

data <- bind_rows(results)

# Add numerical time column (in seconds) and correct name case
data <- data %>%
  mutate(time = ifelse(str_length(time) == 5, paste0("00:", time), paste0("0", time))) %>% 
  rowwise %>% 
  mutate(seconds = as.integer(string_to_time(time))) 
#  mutate(name = str_to_title(name))

#Add % of winner's time
data <- data %>% group_by(race_id) %>% mutate(pc_win = seconds / min(seconds, na.rm = T))
#Add % of cat winner's time
data <- data %>% group_by(race_id, cat) %>% mutate(pc_cat_win = seconds / min(seconds, na.rm = T)) %>% 
  ungroup()

# Save df as RDATA file
if(F){
  saveRDS(data, "data_processed/xc_results_all.RDS")
}
