library(tidyverse)
data12 <- readLines("data/data12")

data_to_tbl <- function(d) {
  str_split(d, "<|>|=|,") %>% 
    map_dfr(function(x) {
      vals <- x[c(3, 5, 7)] %>% as.numeric()
      tibble(x = vals[1], y = vals[2], z = vals[3])
    }) %>% 
    as.matrix()
}

d <- data_to_tbl(data12)

do_x_steps(d, 1000) %>% 
  get_total_energy

get_total_energy <- function(x) {
  energy <- x %>% 
    map(~abs(.x) %>% rowSums) 
  sum(energy[[1]] * energy[[2]])
}
  

do_x_steps <- function(d, steps, vel = matrix(0, nrow = 4, ncol =3)) {
  if (steps == 0) {
    return(list(d, vel))
  }
  new_vel <- get_gravity(d) + vel
  do_x_steps(d + new_vel, steps - 1, new_vel)
}

get_gravity <- function(d) apply(d, 2, get_gravity_vec)


get_gravity_vec <- function(v) map_dbl(1:4, ~get_gravity_val(.x, v))

get_gravity_val <- function(ind, v) {
  x <- v[ind]
  v_no_x <- v[-ind]
  ifelse(x == v_no_x, 0,
         ifelse(x > v_no_x, -1, 1)) %>% sum()
}
