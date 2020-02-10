############
# This converts a single text or pdf race result file to a csv and saves
# Designed to be called from "race_to_csv_ALL.R" as variables are not all defined here but
# are instead held in a list called "vars".
############
path_name <- str_c(path_root, "xc_raw/", file_name)
# Read pdf
# if(is_pdf(path_name)){
#   txt <- pdf_text(path_name)
# }else{
#   txt <- readLines(path_name)
# }
txt <- pdf_text(path_name)
lines <- str_split(txt,"\n") %>% unlist

tb_text <- tibble(line_no = 1 : length(lines), text = str_trim(lines)) %>% 
  mutate(is_results = str_detect(text, "RESULTS")) %>% 
  mutate(res_count = cumsum(is_results)) %>% 
  mutate(results_type = case_when(
    !is_results ~ NA_character_,
    str_detect(text, "TEAM") ~ "Team",
    TRUE ~ "Race"
  )) %>% 
  mutate(cat = ifelse(is_results, parse_xc_cat(text), NA)) %>% 
  fill(results_type, cat)  

tb_res <- tb_text %>% 
  filter(is_results)

tb_remove <- tb_text %>% 
  filter(!str_detect(text, "^\\d|^Pos.*\\d"))
tb <- tb_text %>% 
  filter(str_detect(text, "^\\d|^Pos.*\\d"))

sm <- tb %>% 
  filter(results_type == "Race", cat == "Senior Men") %>% 
  select(line_no, text)

sm2 <- sm %>% 
  mutate(text = ifelse(str_detect(text, "^\\d"), 
                       text, 
                       str_remove(text, ".*Time *(?=\\d)"))) %>% 
  separate(text, into = c("col1", "col2"), "(?<=:\\d\\d) ") %>% 
  pivot_longer(-line_no, names_to = "col", names_prefix = "col", values_to = "text") %>% 
  select(text) %>% 
  filter(!is.na(text)) %>% 
  mutate(text = str_trim(text))

sm3 <- sm2 %>% 
  mutate(non_count = str_detect(text, "^\\$")) %>% 
  mutate(text = str_remove(text, "\\$")) %>% 
  mutate(text = str_remove(text, "(?<=^\\d) (?=\\d )")) %>% 
  separate(text, c("text", "time"), " +(?=\\d*:)") %>% 
  extract(text, c("place", "text"), "(^\\d*) *(.*)") %>% 
  extract(text, c("num", "text"), "(^\\d*) *(.*)") %>%
  mutate(text = str_replace_all(text, "-", "_")) %>% 
  extract(text, c("name", "text"), "(^\\w* *\\w*) *(.*)") %>%
  mutate(text = ifelse(str_detect(text, "^\\w\\d\\d"), text, paste0("U00 0 ", text))) %>% 
  extract(text, c("cat", "text"), "(^\\w\\d\\d) *(.*)", ) %>% 
  mutate(text = str_remove(text, "(?<=^\\d) *(?=\\d)")) %>% 
  extract(text, c("cat_place", "text"), "(^\\d*) *(.*)") %>% 
  mutate(cat = ifelse(cat == "U00", "none", cat)) %>%  
  mutate(text = str_squish(text)) %>% 
  rename(club = "text") %>% 
  mutate(place = as.integer(place)) %>%
  mutate(cat_place = as.integer(cat_place)) %>%
  mutate_if(is.character, ~str_replace_all(., "_", "-")) %>% 
  arrange(place)
sm3

# Save as CSV, adding a line for headers 
files_root <- str_sub(file_name, 1, -5) %>% unlist #remove ".pdf" etc.
csv_name <- str_c(path_root, "xc_csv/", files_root, ".csv")
#headings <- "name,club,category,time"
headings <- names(sm3)

write_csv(sm3, csv_name)
#cat(headings, sm3, file=csv_name, sep="\n")

# cat seems to automatically add a \r to each newline when saving so using sep='\n' results in 
# a \r\n after each entry which is what I want.
