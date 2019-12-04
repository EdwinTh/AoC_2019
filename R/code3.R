library(tidyverse)
data3 <- readLines("data/data3") %>% str_split(",")

# Part 1

find_closest_inersection <- function(circ_strings) {
  map_dfr(circ_strings, ~one_circuit(.x) %>% distinct) %>% 
    count(x, y) %>% 
    filter(n > 1)  %>% 
    mutate(manhattan = abs(x) + abs(y)) %>% 
    filter(manhattan == min(manhattan))
}


one_circuit <- function(circ_string) {
  pos <- c(0,0)
  ret <- vector("list", 0)
  for (i in seq_along(circ_string)) {
    circ_string_proc <- circ_string_to_dir_and_nr(circ_string[i])
    ret[[i]] <- add_directions(pos, dir = circ_string_proc$dir, nr = circ_string_proc$nr)
    pos <- tail(ret[[i]], 1) %>% unlist()
  }
  bind_rows(ret) %>%
    mutate(steps_taken = row_number())
}

circ_string_to_dir_and_nr <- function(cs) {
  list(dir = str_sub(cs, 1, 1), nr =  as.numeric(str_sub(cs, 2)))
}

add_directions <- function(pos = c(0, 0),
                           dir = c("U", "D", "L", "R"),
                           nr) {
  sign <- ifelse(dir %in% c("D", "L"), -1, 1)
  nr <- nr * sign
  if (dir %in% c("L", "R")) return(tibble(x = (pos[1] + sign):(pos[1] + nr), y = pos[2]))
  if (dir %in% c("U", "D")) return(tibble(x = pos[1], y = (pos[2] + sign):(pos[2] + nr) ))
}

find_closest_inersection(data3)

# Part 2

find_earliest_intersection <- function(circ_strings) {
  grids <- map(circ_strings, ~one_circuit(.x) %>% distinct)
  inner_join(grids[[1]], grids[[2]], by = c("x", "y")) %>% 
    mutate(total_steps = steps_taken.x + steps_taken.y) %>% 
    filter(total_steps == min(total_steps))
}

find_earliest_intersection(data3)


