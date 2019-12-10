library(tidyverse)
data5 <- str_split(readLines("data/data5"), ",")[[1]] %>% as.numeric()
take_new_step(data5, input_value = 5)

# Part 1
take_new_step <- function(x, input_code_pos = 0, input_value) {
  input_code <- create_input_code(x[i(input_code_pos)])
  check_valid_opcode(input_code$opcode)
  if (input_code$opcode == 99) return(x)
  x <- update_x(x, input_code_pos, input_code, input_value)
  input_code_pos <- update_input_code_pos(x, input_code, input_code_pos)
  take_new_step(x, input_code_pos, input_value)
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

update_x <- function(x, input_code_pos, input_code, input_value) {
  pars <- x[i(input_code_pos) + 1:3]
  new_val <- get_new_val(x, input_code, pars)
  if (input_code$opcode %in% 1:2) x[i(pars[3])] <- new_val
  if (input_code$opcode == 3) x[i(pars[1])] <- input_value
  if (input_code$opcode == 4) print(new_val)
  if (input_code$opcode %in% 7:8) x[i(pars[3])] <- new_val
  x
}


get_new_val <- function(x, input_code, pars) {
  val1 <- ifelse(input_code$par1 == 0, x[i(pars[1])], pars[1])
  val2 <- ifelse(input_code$par2 == 0, x[i(pars[2])], pars[2])
  val3 <- ifelse(input_code$par2 == 0, x[i(pars[3])], pars[3])
  switch(input_code$opcode,
         "1" = val1 + val2,
         "2" = val1 * val2,
         "3" = val1,
         "4" = val1,
         "5" = NA,
         "6" = NA,
         "7" = as.numeric(val1 < val2),
         "8" = as.numeric(val1 == val2))
}

i <- function(ind) {ind + 1}

check_valid_opcode <- function(opcode) {
  if (!opcode %in% c(1:8, 99)) {
    stop("Invalid opcode, program is wrong")
  }
}

update_input_code_pos <- function(x, input, pos) { 
  opcode <- input$opcode
  if (opcode %in% c(1:2, 7:8)) return(pos + 4)
  if (opcode %in% 3:4) return(pos + 2)
  pars <- x[i(pos) + 1:3]
  eval_par <- ifelse(input$par1 ==0 , x[i(pars[1])] , pars[1])
  jump_to <- ifelse(input$par2 == 0, x[i(pars[2])], pars[2])
  if (opcode == 5) return(ifelse(eval_par != 0, jump_to, pos + 3))
  if (opcode == 6) return(ifelse(eval_par == 0, jump_to, pos + 3))
}
