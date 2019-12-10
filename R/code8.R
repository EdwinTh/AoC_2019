library(tidyverse)
data8 <- readLines("data/data8") %>% str_split("") %>% unlist() %>% as.numeric()

# part1
full_mat <- matrix(data8, ncol = 25, byrow = TRUE)
splitted <- map(1:(nrow(full_mat) / 6),
                ~full_mat[.x:(.x + 5), ])
lowest_0 <- map_dbl(splitted, ~sum(.x == 0)) %>% which.min()
sum(splitted[[43]] == 1) * sum(splitted[[43]] == 2)

# part2
splitted[[1]]
map2(splitted, 1:100, function(x, y) {
  nm <- quo_name(y)
  tibble(col = t(x) %>% as.double()) %>% rename(!!nm := col)}) %>% 
  bind_cols() %>% 
  mutate(row = rep(1:6, each = 25), col = rep(1:25, 6)) %>% 
  pivot_longer(-c(row, col), names_to = "layer", values_to =  "val") %>% 
  mutate(layer = as.numeric(layer)) %>% 
  arrange(row, col, layer) %>% 
  filter(val != 2) %>% 
  group_by(row, col) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  mutate(row = abs(row - 7)) %>% 
  ggplot(aes(col, row, fill = as.character(val)), color = "black") +
  geom_tile() +
  scale_fill_manual(values = c("black", "white"))

  
  
  
