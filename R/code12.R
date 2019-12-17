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

# Part 2

# honostly, had no idea how to solve this, so looked around
# yes it is cheating, but it is to get smarter
# https://www.youtube.com/watch?v=9UcnA2x5s-U
walk_till_equal <- function(d, v = rep(0, 4) , which = "x") {
  d_or <- d <- d[, which]
  it <- 0
  keep_going <- TRUE
  while(keep_going) {
    it <- it + 1
    v  <- v + get_gravity_vec(d)
    d <- v + d
    keep_going <- !all(d == d_or)
    if (it %% 100 == 0) print(it)
  }
  it + 1
}

gcd <- function(x, y) {
  r <- x %% y
  ifelse(r, gcd(y, r), y)
}

lcm <- function(x, y) {
  x * y / gcd(x, y)
}

cycles_for_x <- walk_till_equal(d, which =  "x")
cycles_for_z <- walk_till_equal(d, which =  "z")
cycles_for_y <- walk_till_equal(d, which =  "y")

lcm(lcm(cycles_for_z, cycles_for_y), cycles_for_x) %>% as.character()

