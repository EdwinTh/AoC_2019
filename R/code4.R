library(tidyverse)
all_nums <- 234208:765869
all_nums_splitted <- split_el(all_nums)

## Part 1
selection <- all_nums_splitted[map_lgl(all_nums_splitted, adjacent_criterium) &
                    map_lgl(all_nums_splitted, increasing_criterium)]
length(selection)

split_el <- function(x) {
  str_split(x, "") %>% 
    map(as.numeric)
}

adjacent_criterium <- function(num_split) {
  any(num_split[2:6] - num_split[1:5] == 0)
}

increasing_criterium <- function(num_split) {
  all(num_split[2:6] - num_split[1:5] >= 0)
}

## Part 2
has_two_adjacent <- function(x) {
  paste(x[2:6] - x[1:5], collapse = "") %>% 
    str_split("[1-9]") %>% 
    map_lgl(~any(nchar(.x) == 1))
}

map_lgl(selection, has_two_adjacent) %>% sum

