# Part 1
data1 <- as.numeric(readLines("data/data1"))
get_fuel <- function(x) floor(x / 3) - 2
sum(get_fuel(data1))

# Part2
find_total_fuel <- function(total_mass, 
                            added_fuel = get_fuel(total_mass)) {
  extra_fuel <- get_fuel(added_fuel)
  if (extra_fuel <= 0) {
    return(total_mass + added_fuel)
  } 
  find_total_fuel(total_mass + added_fuel, extra_fuel)
}

sum(purrr::map_dbl(data1, find_total_fuel) - data1)
