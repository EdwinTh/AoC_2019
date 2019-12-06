library(tidyverse)
library(igraph)
data6 <- read_lines("data/data6") %>% 
  str_split("\\)") %>% 
  map_dfr(~tibble(center = .x[1], orbit =.x[2]))

# Part1

find_total_orbits(data6)

find_total_orbits <- function(x) {
  x_leaf <- split_leafs_non_leafs(x)
  gr     <- graph_from_data_frame(data6)
  map_dbl(x_leaf$non_leafs,
          ~find_orbits(gr, .x, x_leaf$leafs)) %>% 
    sum()
}

split_leafs_non_leafs <- function(x) {
  leafs     <- setdiff(x$orbit, x$center)
  non_leafs <- setdiff(x$center, leafs)
  list(leafs = leafs, non_leafs = non_leafs)
}

find_orbits <- function(gr, start, leafs) {
  do.call("c", all_shortest_paths(gr, start, leafs)$res) %>% 
    unique() %>% 
    length() %>% 
    `-`(1)
}

# Part 2

