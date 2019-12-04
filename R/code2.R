library(tidyverse)
data2 <- str_split(readLines("data/data2"), ",")[[1]] %>% as.numeric()
data2[2:3] <- c(12, 2)

# Part 1
take_new_step <- function(x, opcode_pos = 0) {
  action <- evaluate_opcode(x[i(opcode_pos)])
  if (!is.primitive(action)) return(x)
  x <- update_x(x, action, opcode_pos)
  take_new_step(x, opcode_pos + 4)
}

update_x <- function(x, action, opcode_pos) {
  new_val <- action(x[i(x[i(opcode_pos + 1)])], 
                    x[i(x[i(opcode_pos + 2)])]) 
  x[i(x[i(opcode_pos + 3)])] <- new_val
  x
}

i <- function(ind) {ind + 1}

evaluate_opcode <- function(opcode) {
  if (!opcode %in% c(1, 2, 99)) {
    stop("Invalid opcode, program is wrong")
  }
  if (opcode == 1) return(`+`)
  if (opcode == 2) return(`*`)
  "Program ended"
}

# Part 2
data2 <- str_split(readLines("data/data2"), ",")[[1]] %>% as.numeric()

for (noun in 0:(length(data2) - 1)) {
  for (verb in 0:(length(data2) - 1)) {
    data2[2:3] <- c(noun, verb)
    val <- take_new_step(data2)[1]
    if (val == 19690720) print(glue::glue("noun: {noun}, verb: {verb}"))
  }
}


