# convert psych object (from psych package) to matrix
psych_to_mat <- function(psych_thing){
  new_mat <- matrix(nrow = dim(psych_thing)[1],
                    ncol = dim(psych_thing)[1],
                    dimnames = list(rownames(psych_thing),
                                    colnames(psych_thing)))
  for (i in 1:dim(psych_thing)[1]){
    new_mat[i,] = psych_thing[i,]
  }
  new_mat
}

tidy_r_to_text_r <- function(tidy_row){
  round_tidy <- tidy_row %>%
    mutate_at(vars(estimate, contains("conf")), round, 2)
  
  p_value <- case_when(pull(round_tidy, p.value) < .0001 ~ "_p_ < .0001",
                       pull(round_tidy, p.value) < .001 ~ "_p_ < .001",
                    #   pull(round_tidy, p.value) < .01 ~ "_p_ < .01",
                       TRUE ~ paste0("_p_ = ", round(pull(round_tidy, p.value),3)))
  
  paste0(
    "_r_(",
    pull(round_tidy, parameter),
    ") = ", 
    pull(round_tidy, estimate),
    " [",
    pull(round_tidy, conf.low),
    ", ",
    pull(round_tidy, conf.high),
    "], ",
    p_value
  )
}

tidy_t_to_text_t <- function(tidy_row){
  
  round_tidy <- tidy_row %>%
    mutate_at(vars(statistic, estimate, contains("conf"), parameter), round, 2)
  
  p_value <- case_when(pull(round_tidy, p.value) < .0001 ~ "_p_ < .0001",
                       pull(round_tidy, p.value) < .001 ~ "_p_ < .001",
                       TRUE ~ paste0("_p_ = ", round(pull(round_tidy, p.value),2)))
  
  
  
  paste0(
    "_M_ = ", 
    pull(round_tidy, estimate),
    " [",
    pull(round_tidy, conf.low),
    ", ",
    pull(round_tidy, conf.high),
    "]; ",
    "_t_(",
    pull(round_tidy, parameter),
    ") = ", 
    pull(round_tidy, statistic),
    ", ",
    p_value
  )
  
  
}


cohen_d_to_text_d <- function(d_obj){
  
  paste0(
    "_d_ = ", 
    round(d_obj$estimate, 2),
    " [",
    round(pluck(d_obj$conf.int,1), 2),
    ", ",
    round(pluck(d_obj$conf.int,2), 2),
    "]"
  )
  
}
