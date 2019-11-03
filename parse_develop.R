# WORK IN PROGRESS
# Trying a new method of converting PDF text to a table
# Based on identifying lines of white space running down the page and splitting the text based on these.
# There are various issues, some difficult.
#
# * I wrote the method for Kentmere. This has a header on each page, but most just have a header on page 1.
# * I rely on there being a space character in the same x position right down a page (through entries and header), but
#   Arnside breaks this - the start of the "Number" header lines up with the end of the times. Main Issue I think.
#   The prolem comes from the justification of the text and headers which vary through the docs.
# * Headers on Coledale go over two lines (overlaps other text)
# * The columns on different pages don't line up perfectly. I handled this on Kentmere but more difficult when 
#   header is only on one page.
#
# My inclination is to abandon this approach and try the pdftools::pdf_data() approach.

library(pdftools)
library(tidyverse)
source("parse_FUNC.R")

races <- c("ArnsideKnott2016.pdf", "ButtermereTrail2016.pdf", "CloughaPike2014.pdf", 
           "ColedaleHS2017.pdf", "GreatWhernside2016.pdf", "HuttonRoof2015.txt", 
           "Latrigg2017.pdf", "WeasdaleHS2016.pdf", "HuttonRoof2018.txt", "Kentmere2019.pdf")

file_name <- str_c(path_root, "Results_raw/", races[1])
file_name

# Read pdf
if(is_pdf(file_name)){
  txt <- pdf_text(file_name)
}else{
  txt <- readLines(file_name)
}
pages <- str_split(txt, "\n")
#str_split(txt, "\n") %>% unlist

# Test single page
# Issue with Arnside is that it is centre justified
# So far I have been thinking of left justified ie. start of entry matches start of header
lines <- pages[[1]]
ent_pos <- lines %>% str_trim() %>% str_detect("^\\d") %>% which()
head_line <- lines[ent_pos[1] - 1]
lines <- lines[ent_pos]
spaces <- find_breaks(lines)
splits <- get_splits(spaces)
headers(head_line, splits)
i = 1
str_sub(head_line, spl[i] + 1, spl[i + 1]) %>% str_squish()
head(lines)
pages[[1]] %>% head()
splits
spaces
# All pages
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
i
tb
res
debugonce(page_to_table)
head(lines)
str_count(str_to_lower(head_line), "place|pos|position|name|club|cat|time")


page_to_table(lines[-1], head_line, spaces)
View(lines)

# Save as CSV, adding a line for headers 
files_root <- str_sub(vars$name, 1, -5) %>% unlist #remove ".pdf" etc.
csv_name <- str_c(path_root, "Results_CSV/", files_root, ".csv")
headings <- "name,club,category,time"
cat(headings, fr_entries, file=csv_name, sep="\n")
