
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

# split text into table
# Check each col contains a title word.
page_to_table <- function(entries, head_line, spaces){
  splits <- get_splits(spaces)
  heads <- names(splits)[-length(splits)]
  ncols <- length(heads)
  tb <- NULL
  nn <- length(entries)
  vec <- character(nn)
  for (i in 1 : ncols){
    for (j in 1 : nn){
      ent <- entries[j]
      vec[j] <- str_sub(ent, splits[i] + 1, splits[i + 1]) %>% str_squish()
    }
    tb[[i]] <- vec
  }
  names(tb) <- heads
  as_tibble(tb)  
}

get_splits <- function(spaces){
  spl <- str_locate_all(spaces, "10") %>% .[[1]] %>% .[, 1]
  if(str_sub(spaces, 1, 1) == "1"){
    spl[1] <- 0
  }else{
    spl <- c(0, spl)
  }
  c(spl, str_length(spaces) + 1)
}

split_head <- function(spaces, head_line) {
  spl <- str_locate_all(spaces, "10") %>% .[[1]] %>% .[, 1]
  spl <- c(0, spl, str_length(head_line) + 1)
  n <- length(spl) - 1
  splits <- rep(NA_integer_, n)
  for (i in 1 : n){
    nm <- str_sub(head_line, spl[i] + 1, spl[i + 1]) %>% str_squish()
    if(str_detect(nm, "\\S")){
      splits[i] <- spl[i]
      names(splits)[i] <- nm
    }
  }
  c(splits[!is.na(splits)], XXX = tail(spl, 1))
}

headers <- function(head_line, splits) {
  n <- length(splits) - 1
  nm <- character(n)
  for (i in 1 : n){
    nm[i] <- str_sub(head_line, splits[i] + 1, splits[i + 1]) %>% str_squish()
  }
  nm
}

