library(tidyverse)
data10 <- readLines("data/data10")
data10 <- readLines("data/tmp")
line_to_grid <- function(line, row = 1) {
  astroid <- unlist(str_split(line, "")) == "#"
  tibble(x = 0:(length(astroid)-1), y = row, astroid = astroid)
}

astroid_grid <- map2_dfr(data10, 0:(length(data10)-1), line_to_grid) %>% 
  filter(astroid) %>% select(-astroid)

# part 1
nr_visible_by_pos <- map_int(1:nrow(astroid_grid), 
                             ~nr_visible(.x, astroid_grid))
which.max(nr_visible_by_pos) # 293  (206 for tmp set)
max(nr_visible_by_pos)

nr_visible <- function(loc, gr) {
  base <- as.numeric(gr[loc, ])
  others <- split_and_sort(gr[-loc, ]) %>% 
    map(as.numeric)
  map_lgl(others, ~is_visible(base, .x, gr)) %>% sum()
}

is_visible <- function(c1, c2, gr) {
  all_in_betweens <- find_all_in_betweens(c1, c2)
  astr_in_between <- inner_join(all_in_betweens, 
                                gr, 
                                c("in_between_x" = "x", 
                                  "in_between_y" = "y"))
  nrow(astr_in_between) == 0
}

find_all_in_betweens <- function(c1, c2) {
  if (c1[1] == c2[1]) return(y_in_between(c1, c2))
  if (c1[2] == c2[2]) return(x_in_between(c1, c2))
  all_other_in_betweens(c1, c2)
}

x_in_between <- function(c1, c2) {
  tibble(coord1_x = c1[1], coord1_y = c1[2],
         coord2_x = c2[1], coord2_y = c2[2],
         in_between_x = span_between(c1[1], c2[1]), in_between_y = c1[2])
}

y_in_between <- function(c1, c2) {
  tibble(coord1_x = c1[1], coord1_y = c1[2],
         coord2_x = c2[1], coord2_y = c2[2],
         in_between_x = c1[1], in_between_y = span_between(c1[2], c2[2]) )
}

all_other_in_betweens <- function(c1, c2) {
  largest_dif <- max(abs(c1[1] -c2[1]), abs(c1[2] - c2[2]))
  y_steps <- seq(c1[2], c2[2], length.out = largest_dif + 1) %>% drop_first_last()
  x_steps <- seq(c1[1], c2[1], length.out = largest_dif + 1) %>% drop_first_last()
  coords <- (x_steps %% 1 == 0) & (y_steps %% 1 == 0)
  
  if (largest_dif == 1) coords <- numeric(0)
  
  tibble(coord1_x = c1[1], coord1_y = c1[2],
         coord2_x = c2[1], coord2_y = c2[2],
         in_between_x = x_steps[coords],
         in_between_y = y_steps[coords])
}

span_between <- function(x, y) {
  start <- min(c(x, y)) + 1
  end   <- max(c(x, y)) - 1
  if (start > end) return(NA)
  start:end
}

drop_first_last <- function(x) x[2:(length(x) -1)]

split_and_sort <- function(x) {
  x <- as.data.frame(x)
  rownames(x) <- str_sub(paste0("000", rownames(x)), -4)
  split(x, rownames(x))
}
# part 2

astroid_grid_rebased <- astroid_grid %>% 
  mutate(x = x - 11, y = 13 - y) %>% 
  filter(!(x == 0 & y == 0))
astroid_grid_rebased -> df
find_200th(astroid_grid_rebased)

find_200th <- function(df) {
  vap_df <- initialise(df)
  df <- drop_next(df, vap_df)
  last_vap = as.numeric(vap_df)
  remove_next_astroid(df, last_vap = as.numeric(vap_df), vap_df)
}

initialise <- function(df) {
  df %>% 
    filter(y > 0, x == 0) %>% 
    filter(y == min(y)) 
}

drop_next <- function(df, vap) {
  df %>% filter(!(x == vap[[1]] & y == vap[[2]]))
}

remove_next_astroid <- function(df, last_vap, vap_df) {
  next_astroid <- find_next_astroid(df, last_vap)
  vap_df <- bind_rows(vap_df, tibble(x = next_astroid[1], y = next_astroid[2]))
  df <- drop_next(df, next_astroid)
  last_vap = next_astroid
  vap_df %>% map_back_vap_df()
  
  if (nrow(vap_df) == 200) return(vap_df %>% map_back_vap_df())
  remove_next_astroid(df, last_vap = next_astroid, vap_df)
}

find_next_astroid <- function(df, cur) {
  cur <- as.numeric(cur)
  splitted <- split_and_sort(df) %>% map(as.numeric)
  angles <- map_dbl(splitted, ~find_angle(cur, .x))
  angles_in_scope <- angles[map_lgl(splitted, ~to_the_right(cur, .x))]
  candidate <- splitted[[which.min(angles_in_scope[angles_in_scope != 0]) %>% names()]]
  if (is_visible(cur, candidate, df)) return(candidate)
  find_next_astroid(df %>% filter(!(row != candidate[1] & column != candidate[2])), cur)
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

map_back_vap_df <- function(vap_df, x_base = 11, y_base = 13) {
  vap_df %>% 
    mutate(y = y_base - y ,
           x = x_base + x)
}
