library(tidyverse)
data14 <- readLines("data/data14")

unit_lookup <- map(data14, build_unit_lookup)
names(unit_lookup) <- unit_lookup %>% 
  map_chr(~.x[['target']]$kind)
find_the_ore_needed(unit_lookup)

find_the_ore_needed <- function(unit_lookup) {
  units_df <- determine_eval_order(unit_lookup)
  if (eval_order$kind[1] != "FUEL") {
    stop("First to eval is not FUEL, flaw in your thinking Edwin")
  }
  for (i in 2:nrow(units_df)) {
    units_df <- update_units_df(pull(units_df[i, 'kind']), unit_lookup, units_df)
  }
  units_df
}

determine_eval_order <- function(unit_lookup) {
  all_units <- names(unit_lookup)
  tibble(kind = all_units, 
         steps = map_dbl(all_units, max_steps_form_ore)) %>% 
    arrange(steps %>% desc()) %>% 
    bind_rows(tibble(kind = "ORE", steps = 0)) %>% 
    mutate(units_needed = c(1, rep(0, length(all_units))))
}


max_steps_form_ore <- function(name = "FUEL") {
  suppressWarnings(steps_from_ore(name)) %>% 
    unlist() %>% 
    max()
}

steps_from_ore <- function(name = "FUEL", steps = 1) {
  inputs <- unit_lookup[[name]]$inputs %>% map_chr(~.x$kind)
  if (inputs == "ORE") return(steps)
  map(inputs, steps_from_ore, steps = steps + 1)
}

update_units_df <- function(name, unit_lookup, unit_df) {
   has_name <- map(unit_lookup, ~.x$inputs %>% map_chr(~.x$kind)) %>% 
     map_lgl(~name %in% .x)
   input_for <- names(has_name)[has_name]
   tot <- map_dbl(input_for, ~input_needed(unit_lookup[[.x]], unit_df, name)) %>% sum()
   unit_df %>% mutate(units_needed = ifelse(kind == name, tot, units_needed))
}

input_needed <- function(unit_lookup_el, unit_df, name) {
  tgt_units_total <- unit_df %>% 
    filter(kind == unit_lookup_el$target$kind) %>% 
    pull(units_needed)
  el <- which(map_chr(unit_lookup_el$inputs, ~.x$kind) == name)
  input_units <- unit_lookup_el$inputs[[el]]$unit
  tgt_units   <- unit_lookup_el$target$unit
  ceiling(tgt_units_total / tgt_units) * input_units
}


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




  


