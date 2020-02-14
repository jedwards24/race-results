#----------------
# Used to pull category out of XC results text (lines with "RESULTS" in it)
# Used in xc_label_lines()
xc_parse_cat <- function(text) {
  str_remove_all(text, "Race \\d") %>% 
    str_remove_all("\\Q($=non-counter)\\E") %>%
    str_remove_all("TEAM RESULTS|FINAL RESULTS") %>%
    str_remove_all(" . ") %>%
    str_squish()
}

###############
# Converts raw text (from a mid lancs xc pdf) into a table with each row
# a line of text together with line number and columns describing 
# whether it is part of a race or team results table, and which age/sex category 
# it describes. The header and title of each category results table will be labeled as 
# belonging to that table.
###############
xc_label_lines <- function(lines) {
  tibble(line_no = 1 : length(lines), text = str_trim(lines)) %>% 
    mutate(is_results = str_detect(text, "RESULTS")) %>% 
    mutate(res_count = cumsum(is_results)) %>% 
    mutate(results_type = case_when(
      !is_results ~ NA_character_,
      str_detect(text, "TEAM") ~ "Team",
      TRUE ~ "Race"
    )) %>% 
    mutate(cat = ifelse(is_results, xc_parse_cat(text), NA)) %>% 
    fill(results_type, cat)
}


#----------------
#Input is an xc text table (as output from `xc_label_lines()`).
# Output is a table of text lines which have the supplied type and category.
# Use `xc_categories()` to see available categories.
# The header for race results is retained because it sometimes includes actual results too (2 column format). 
xc_category_results <- function(text_tbl, type = "Race", category = "Senior Men") {
  text_tbl %>% 
    filter(str_detect(text, "^\\d|^Pos.*\\d")) %>% 
    filter(results_type == type, cat == category) %>% 
    select(text)
}

############
# Returns all categories found in an xc text table (as output from `xc_label_lines()`)
############
xc_categories <- function(text_tbl) {
  text_tbl %>%
    filter(!is.na(cat)) %>% 
    pull(cat) %>% 
    unique()
}

#--------------
# Input is a table of results text from a single race (from `xc_category_results()` with `type="Race"`).
# Each row may contain results from multiple runners and each runner's results will be given it's own row.
# Warnings are normal where a text row only contains a single runner since this will create an `NA` during 
# the text splitting. `NA` rows are removed before returning.
xc_to_column <- function(race_tbl) {
  race_tbl %>% 
    mutate(text = ifelse(str_detect(text, "^\\d"), 
                         text, 
                         str_remove(text, ".*Time *(?=\\d)"))) %>% 
    separate(text, into = c("col1", "col2"), "(?<=:\\d\\d) ") %>% 
    pivot_longer(everything(), names_to = "col", names_prefix = "col", values_to = "text") %>% 
    select(text) %>% 
    filter(!is.na(text)) %>% 
    mutate(text = str_trim(text))
}

#---------------
# Splits the text of race results into columns with: place, race number, name, cat, cat place, club, time,
# whether the runner was a non-counter.
xc_split_results <- function(place_tbl) {
   place_tbl %>% 
    mutate(non_count = str_detect(text, "\\$")) %>% 
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
}