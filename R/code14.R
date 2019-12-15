library(tidyverse)
data14 <- readLines("data/data14")

unit_lookup <- map(data14, build_unit_lookup)
names(unit_lookup) <- unit_lookup %>% 
  map_chr(~.x[['target']]$kind)

build_unit_lookup <- function(string) {
  splitted1 <- string %>% str_split(" => ") %>% unlist()
  target <- into_unit_kind(splitted1[[length(splitted1)]])
  inputs <- splitted1[[-length(splitted1)]] %>% 
    str_split(", ") %>% 
    unlist() %>% 
    map(into_unit_kind)
  list(target = target, inputs = inputs)
}

into_unit_kind <- function(string) {
  splitted <- unlist(str_split(string, " "))
  list(unit = splitted[1] %>% as.numeric(), kind = splitted[2])
}


calculate_needed <- function(trgt = 'FUEL') {
  trgt_part   <- unit_lookup[[trgt]]
  target_unit <- trgt_part$target$unit
  
}