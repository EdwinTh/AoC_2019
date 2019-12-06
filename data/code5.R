library(tidyverse)
data5 <- str_split(readLines("data/data5"), ",")[[1]] %>% as.numeric()
data2[2:3] <- c(12, 2)

# Part 1
take_new_step <- function(x, input_code_pos = 0) {
  input_code <- create_input_code(x[i(input_code_pos)])
  check_valid_opcode(input_code$opcode)
  if (input_code$opcode == 99) return(x)
  x <- update_x(x, input_code_pos, input_code)
  take_new_step(x, update_input_code_pos(input_code_pos))
}

create_input_code <- function(raw_input) {
  list(
    opcode = str_sub(raw_input, -2) %>% as.numeric(),
    par1   = str_sub(raw_input, -3, -3) %>% as.numeric() %>% zero_if_na(),
    par2   = str_sub(raw_input, -4, -4) %>% as.numeric() %>% zero_if_na(),
    par3   = str_sub(raw_input, -5, -5) %>% as.numeric() %>% zero_if_na()
  )
}

zero_if_na <- function(x) ifelse(is.na(x), 0, x)

update_x <- function(x, input_code_pos, input_code) {
  pars <- x[i(input_code_pos) + 1:3]
  new_val <- action(x[i(x[i(opcode_pos + 1)])], 
                    x[i(x[i(opcode_pos + 2)])]) 
  x[i(x[i(opcode_pos + 3)])] <- new_val
  x
}

process <- function(x, pars, input_code) {
  pars[1]
}

add <- function(x, input_code_pos, input_code) {
  
}

update <- function(x, input_code_pos, input_code) {
  
}

output <- function(x, input_code_pos, input_code) {
  
}

i <- function(ind) {ind + 1}

check_valid_opcode <- function(opcode) {
  if (!opcode %in% c(1, 2, 3, 4, 99)) {
    stop("Invalid opcode, program is wrong")
  }
}
