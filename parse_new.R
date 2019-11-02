# Experiment splitting the pdf by spacing 

file_name <- str_c(path_root, "Results_raw/Kentmere2019.pdf")
# Read pdf
if(is_pdf(file_name)){
  txt <- pdf_text(file_name)
}else{
  txt <- readLines(file_name)
}

# New method
# Works one page at a time
pages <- str_split(txt,"\n")
length(pages)
lines <- pages[[1]]

ent_pos <- lines %>% str_trim() %>% str_detect("^\\d") %>% which()
head_line <- lines[ent_pos[1] - 1]
str_count(str_to_lower(head_line), "place|pos|position|name|club|cat|time")
lines <- lines[(ent_pos[1] - 1) : tail(ent_pos, 1)]

# Find breaks
find_breaks <- function(lines) {
  entlen <- max(str_length(lines))
  cand <- rep(TRUE, entlen)
  for (i in 1 : length(lines)){
    ent <- lines[i]
    if (str_length(ent) > 0){
      locs <- str_locate_all(ent, " ")
      curr <- (1 : entlen) %in% locs[[1]][, 1]
      cand <- curr & cand
    }
  }
  as.integer(cand) %>% str_c(collapse = "")
}

find_breaks(lines)

# split text into table
page_to_table <- function(entries, head_line, spaces){
  starts <- str_locate_all(spaces, "10") %>% .[[1]] %>% .[, 2]
  starts <- c(1, starts)  
  ends <- str_locate_all(spaces, "01") %>% .[[1]] %>% .[, 1] %>% c(., entlen)
  ncols <- length(starts)
  tb <- NULL
  nn <- length(entries)
  vec <- character(nn)
  for (i in 1 : ncols){
    for (j in 1 : nn){
      ent <- entries[j]
      vec[j] <- str_sub(ent, starts[i], ends[i]) %>% str_squish()
    }
    tb[[i]] <- vec
    names(tb)[i] <- str_sub(head_line, starts[i], ends[i]) %>% str_squish()
  }
  as_tibble(tb)  
}
page_to_table(lines[-1], head_line, spaces)

res <- NULL
np <- length(pages)
for (i in 1 : np){
  lines <- pages[[i]]
  ent_pos <- lines %>% str_trim() %>% str_detect("^\\d") %>% which()
  head_line <- lines[ent_pos[1] - 1]
  lines <- lines[(ent_pos[1] - 1) : tail(ent_pos, 1)]
  spaces <- find_breaks(lines)
  tb <- page_to_table(lines[-1], head_line, spaces)
  res <- bind_rows(res, tb)
}
tb
res
debugonce(page_to_table)

# Save as CSV, adding a line for headers 
files_root <- str_sub(vars$name, 1, -5) %>% unlist #remove ".pdf" etc.
csv_name <- str_c(path_root, "Results_CSV/", files_root, ".csv")
headings <- "name,club,category,time"
cat(headings, fr_entries, file=csv_name, sep="\n")

