source("R/code5.R")
library(tidyverse)
x <- c(3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
       27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5)


data7 <- {readLines("data/data7") %>% str_split(",")}[[1]] %>% 
  as.numeric()

# Part 1
combinat::permn(0:4) %>% 
  map_dbl(~run_all_amplifiers(data7, .x)) %>% 
  max

# Part 2
loop_amplifiers <- function(x, phases, first_in_val = 0) {
  last_out_val <- run_all_amplifiers(x, phases, first_in_val)
  print(last_out_val)
  loop_amplifiers(x, phases, last_out_val)
}

run_all_amplifiers <- function(x, phases, first_in_val = 0) {
  out_val_A <- take_new_step(x, phase_setting = phases[1], input_value = first_in_val)
  out_val_B <- take_new_step(x, phase_setting = phases[2], input_value = out_val_A)
  out_val_C <- take_new_step(x, phase_setting = phases[3], input_value = out_val_B)
  out_val_D <- take_new_step(x, phase_setting = phases[4], input_value = out_val_C)
  take_new_step(x, phase_setting = phases[5], input_value = out_val_D)
}


take_new_step <- function(x, 
                          input_code_pos = 0, 
                          phase_setting, 
                          input_value, 
                          state = 1, 
                          last_out_val = NULL) {
  input_code <- create_input_code(x[i(input_code_pos)])
  check_valid_opcode(input_code$opcode)
  if (input_code$opcode == 99) return(last_out_val)
  if (input_code$opcode == 4) {
    pars <- x[i(input_code_pos) + 1:3]
    last_out_val <- get_new_val(x, input_code, pars)
  } else {
    val_for_update <- ifelse(state == 1, phase_setting, input_value)
    x <- update_x(x, input_code_pos, input_code, val_for_update)
  }
  input_code_pos <- update_input_code_pos(x, input_code, input_code_pos)
  if (input_code$opcode == 3 & state == 1) state <- 2
  take_new_step(x, input_code_pos, phase_setting, input_value, state, last_out_val)
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

