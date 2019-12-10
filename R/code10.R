library(tidyverse)
data10 <- readLines("data/data10")

line_to_grid <- function(line, row = 1) {
  astroid <- unlist(str_split(line, "")) == "#"
  tibble(row = row, column = 1:length(astroid), astroid = astroid)
}

astroid_grid <- map2_dfr(data10, 1:length(data10), line_to_grid) %>% 
  filter(astroid) %>% select(-astroid)

# part 1
nr_visible_by_pos <- map_int(1:nrow(astroid_grid), ~nr_visible(.x, astroid_grid))
which.max(nr_visible_by_pos) # 293
max(nr_visible_by_pos)

nr_visible <- function(loc, gr) {
  base <- as.numeric(gr[loc, ])
  others <- split(gr[-loc, ], rownames(gr[-loc, ])) %>% 
    map(as.numeric)
  sum(map_lgl(others, ~is_visible(base, .x, gr)))
}

is_visible <- function(c1, c2, gr) {
  all_in_betweens <- find_all_in_betweens(c1, c2)
  astr_in_between <- inner_join(all_in_betweens, 
                                gr, 
                                c("in_between_row" = "row", 
                                  "in_between_col" = "column"))
  nrow(astr_in_between) == 0
}

find_all_in_betweens <- function(coord1, coord2) {
  if (coord1[1] == coord2[1]) return(rows_in_between(coord1, coord2))
  if (coord1[2] == coord2[2]) return(cols_in_between(coord1, coord2))
  all_other_in_betweens(coord1, coord2)
}

rows_in_between <- function(c1, c2) {
  tibble(coord1_row = c1[1], coord1_col = c1[2],
         coord2_row = c2[1], coord2_col = c2[2],
         in_between_row = c1[1], in_between_col = span_between(c1[2], c2[2]) )
}

cols_in_between <- function(c1, c2) {
  tibble(coord1_row = c1[1], coord1_col = c1[2],
         coord2_row = c2[1], coord2_col = c2[2],
         in_between_row = span_between(c1[1], c2[1]), in_between_col = c1[2])
}

all_other_in_betweens <- function(c1, c2) {
  largest_dif <- max(abs(c1[1] -c2[1]), abs(c1[2] - c2[2]))
  col_steps <- seq(c1[2], c2[2], length.out = largest_dif + 1) %>% drop_first_last()
  row_steps <- seq(c1[1], c2[1], length.out = largest_dif + 1) %>% drop_first_last()
  coords <- (row_steps %% 1 == 0) & (col_steps %% 1 == 0)
  
  if (largest_dif == 1) coords <- numeric(0)
  
  tibble(coord1_row = c1[1], coord1_col = c1[2],
         coord2_row = c2[1], coord2_col = c2[2],
         in_between_row = row_steps[coords],
         in_between_col = col_steps[coords])
}

span_between <- function(x, y) {
  start <- min(c(x, y)) + 1
  end   <- max(c(x, y)) - 1
  if (start > end) return(NA)
  start:end
}

drop_first_last <- function(x) x[2:(length(x) -1)]

# part 2
astroid_grid_0 <- astroid_grid %>% 
  mutate(row = row - 1, column = column - 1)

astroid_grid_rebased <- astroid_grid_0 %>% 
  mutate(row = row - 21, column = 20 - column) %>% 
  filter(!(row == 0 & column == 0))

initialise <- function(x) {
  x %>% 
    filter(row == 0, column > 0) %>% 
    filter(column == min(column))
}

drop_next <- function(x, vap) {
  x %>% filter(!(row == vap[[1]] & column == vap[[2]]))
}

vaporize_that_shit <- function(x) {
  vap_df <- initialise(x)
  x <- drop_next(x, vap)
  remove_next_astroid(x, last_vap = as.numeric(vap_df), vap_df)
}

remove_next_astroid <- function(x, last_vap, vap_df) {
  print(vap_df)
  next_astroid <- find_next_astroid(x, last_vap)
  vap_df <- bind_rows(vap_df, tibble(row = next_astroid[1], column = next_astroid[2]))
  x <- drop_next(x, next_astroid)
  if (nrow(x) == 0) return(vap_df %>% mutate(order = row_number()))
  remove_next_astroid(x, next_astroid, vap_df)
}

find_next_astroid <- function(x, cur) {
  cur <- as.numeric(cur)
  splitted <- map(split(x, rownames(x)), as.numeric)
  angles <- map_dbl(splitted, ~find_angle(cur, .x))
  angles_in_scope <- angles[map_lgl(splitted, ~to_the_right(cur, .x))]
  candidate <- splitted[[which.min(angles[angles != 0]) %>% names() %>% as.numeric()]]
  if (is_visible(cur, candidate, x)) return(candidate)
  find_next_astroid(x %>% filter(!(row != candidate[1] & column != candidate[2])), cur)
}

find_angle <- function(c1, c2) {
  mag1 <- sqrt(sum(c1 ^ 2))
  mag2 <- sqrt(sum(c2 ^ 2))
  dot_prod <- c1[1] * c2[1] + c1[2] * c2[2]
  acos(dot_prod / (mag1 * mag2))
}

to_the_right <- function(c1, c2) {
  # https://stackoverflow.com/questions/13221873/
  dot_prod <- c1[1] * -c2[2] + c1[2] * c2[1]
  dot_prod > 0
}


