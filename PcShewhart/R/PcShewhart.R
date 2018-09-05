# Получение последовательности символов для анализа структур на особые причины
### Сравнение ">": если больше значение больше операнда - 1, меньше - 0
s_Sequence_up <- function(sample, operand){
  l_Operand <- sample > operand
  b_Operand <- 1 * l_Operand
  fin <- paste0(b_Operand, collapse = "")
  return(fin)
}

### Сравнение "<": если больше значение больше операнда - 0, меньше - 1
s_Sequence_low <- function(sample, operand){
  l_Operand <- sample < operand
  b_Operand <- 1 * l_Operand
  fin <- paste0(b_Operand, collapse = "")
  return(fin)
}

### Вывод значений, находящихся внутри границ: (operand1, operand2)
s_Sequence_in <- function(sample, operand1, operand2){
  l_Operand1 <- sample > operand1 # Последовательность больше нижнего предела
  l_Operand2 <- sample < operand2 # Последовательность меньше верхнего предела
  l_Operand <- l_Operand1 & l_Operand2
  b_Operand <- 1 * l_Operand
  fin <- paste0(b_Operand, collapse = "")
  return(fin)
}

### Разность каждого из членов последовательности

s_Difference <-  function(sample){
  v_diff <- diff(sample)
  l_diff <- v_diff > 0
  b_diff <- 1 * l_diff
  fin <- paste0(b_diff, collapse = "")
  return(fin)
}


### Получение вектора позиций подстроки в строке
#' Evaluates the amount of good honey given the tree
#'
#' Evaluates the amount of good honey given the tree
#'
#' The amount of honey is estimated using the latest mcmc methods.
#'
#' @param tree the tree for which the amount of honey is estimated
#' @return numeric the estimated amount of good honey
#' @export
#' @examples
#' tree <- "Bolshoy Dub"
#' v_Position("expression", "string")
v_Position <- function (exp, string){
  list_pos <- gregexpr(exp, string)
  v_pos <- unlist(list_pos)
  return(v_pos)
}


### Контрольные карты Шухарта ---------------------------------------------------------------


### Критерий 1: Выход точки за предельные границы -------------------------------------------

Shewchart_1 <- function(table, sample, ucl, lcl){
  data_UCL <- NULL
  data_LCL <- NULL
  
  s_UCL <- s_Sequence_up(sample, ucl)
  s_LCL <- s_Sequence_low(sample, lcl)
  
  id_UCL <- v_Position("1", s_UCL)
  if (id_UCL != -1){
    data_UCL <- table[id_UCL, ]
  }
  
  id_LCL <- v_Position("1", s_LCL)
  if (id_LCL != -1){
    data_LCL <- table[id_LCL, ]
  }  
  
  Fin <- do.call(rbind, list(data_LCL, data_UCL))
  return(Fin)
}


### Критерий 2: 9 точек в зоне С или по одну сторону от средней линии -----------------------

Shewchart_2 <- function(table, sample, average, sigma){
  data_zone_C <- NULL
  data_up <- NULL
  data_low <- NULL
  lower_c <- average - sigma
  upper_c <- average + sigma
  
  s_zone_C <- s_Sequence_in(sample, lower_c, upper_c)
  id_zone_C <- v_Position("111111111", s_zone_C)
  
  if (id_zone_C != -1){
    data_zone_C <- table[id_zone_C:(id_zone_C + 8), ]
  }
  
  s_upper_average <- s_Sequence_up(sample, average)
  
  id_up <- v_Position("111111111", s_upper_average)
  if (id_up != -1){
    data_up <- table[id_up:(id_up + 8), ]
  }
  
  id_low <- v_Position("000000000", s_upper_average)
  if (id_low != -1){
    data_low <- table[id_low:(id_low + 8), ]
  }

  Fin <- do.call(rbind, list(data_zone_C, data_up, data_low))
  return(Fin)
}


### Критерий 3: 6 последовательно возрастающих или убывающих точек  -----------------------------------------------


Shewchart_3 <- function(table, sample){
  data_incr <- NULL
  data_decr <- NULL
  incr_decr <- NULL
  s_diff <- s_Difference(sample)
  
  id_diff_incr <- v_Position("11111", s_diff)
  if (id_diff_incr != -1){
    data_incr <- table[(id_diff_incr):(id_diff_incr + 5), ]
  }
  
  id_diff_decr <- v_Position("00000", s_diff)
  if (id_diff_decr != -1){
    data_decr <- table[(id_diff_decr - 1):(id_diff_decr + 5), ]
  }
  check <- id_diff_incr + id_diff_decr
  if (check != -2){
    incr_decr <- do.call(rbind, list(data_incr, data_decr))
  }
  Fin <- incr_decr
  return(Fin)
}


### Критерий 4: 14 попеременно возрастающих и убывающих точек  -----------------------------------------------  

Shewchart_4 <- function(table, sample){
  data_in_dec <- NULL
  s_diff <- s_Difference(sample)
  
  id_in_dec <- v_Position("1010101010101", s_diff)
  if (id_in_dec !=  -1){
    data_in_dec <- table[(id_in_dec - 1):(id_in_dec + 14), ]
    
  }
  Fin <- data_in_dec
  return(Fin)
}


### Критерий 5: 2 из 3 последовательных точек в зоне А или вне её -----------------------------------------------

Shewchart_5 <- function(table, sample, average, sigma){
  data_zone_A_up <- NULL
  data_zone_A_low <- NULL
  data_zone_A_up_2 <- NULL
  data_zone_A_low_2 <- NULL
  
  s_zone_A_up <- s_Sequence_up(sample, average + 2 * sigma)
  s_zone_A_low <- s_Sequence_low(sample, average - 2 * sigma)
  
  id_zone_A_up <- v_Position("11", s_zone_A_up)
  if ( id_zone_A_up != -1){
    data_zone_A_up <- table[id_zone_A_up:(id_zone_A_up + 2), ]
  }
  
  id_zone_A_low <- v_Position("11", s_zone_A_low)
  if ( id_zone_A_low != -1){
    data_zone_A_low <- table[id_zone_A_low:(id_zone_A_low + 2), ]
  }
  
  id_zone_A_up_2 <- v_Position("101", s_zone_A_up)
  if ( id_zone_A_up_2 != -1){
    data_zone_A_up_2 <- table[id_zone_A_up_2:(id_zone_A_up_2 + 2), ]
  }
  
  id_zone_A_low_2 <- v_Position("101", s_zone_A_low)
  if ( id_zone_A_low_2 != -1){
    data_zone_A_low_2 <- table[id_zone_A_low:(id_zone_A_low + 2), ]
  }
  
  Fin <- do.call(rbind, list(data_zone_A_up, data_zone_A_low ,data_zone_A_up_2, data_zone_A_low_2))
  return(Fin)
}


### Критерий 6: 4 из 5 последовательных точек в зоне B или выше её -----------------------------------------------

Shewchart_6 <- function(table, sample, average, sigma){
  data_zone_B_up <- NULL
  data_zone_B_up_2 <- NULL
  data_zone_B_up_3 <- NULL
  data_zone_B_up_4 <- NULL
  data_zone_B_low <- NULL
  data_zone_B_low_2 <- NULL
  data_zone_B_low_3 <- NULL
  data_zone_B_low_4 <-  NULL
  
  s_zone_B_up <- s_Sequence_up(sample, average + sigma)
  s_zone_B_low <- s_Sequence_low(sample, average - sigma)
  
  id_zone_B_up <- v_Position("1111", s_zone_B_up)
  if ( id_zone_B_up != -1){
    data_zone_B_up <- table[id_zone_B_up:(id_zone_B_up + 2), ]
  }
  
  id_zone_B_up_2 <- v_Position("11101", s_zone_B_up)
  if ( id_zone_B_up_2 != -1){
    data_zone_B_up_2 <- table[id_zone_B_up_2:(id_zone_B_up_2 + 2), ]
  }
  
  id_zone_B_up_3 <- v_Position("11011", s_zone_B_up)
  if ( id_zone_B_up_3 != -1){
    data_zone_B_up_3 <- table[id_zone_B_up_3:(id_zone_B_up_3 + 2), ]
  }
  
  id_zone_B_up_4 <- v_Position("10111", s_zone_B_up)
  if ( id_zone_B_up_4 != -1){
    data_zone_B_up_4 <- table[id_zone_B_up_4:(id_zone_B_up_4 + 2), ]
  }
  
  id_zone_B_low <- v_Position("1111", s_zone_B_low)
  if ( id_zone_B_low != -1){
    data_zone_B_low <- table[id_zone_B_low:(id_zone_B_low + 2), ]
  }
  
  id_zone_B_low_2 <- v_Position("11101", s_zone_B_low)
  if ( id_zone_B_low_2 != -1){
    data_zone_B_low_2 <- table[id_zone_B_low_2:(id_zone_B_low_2 + 2), ]
  }
  
  id_zone_B_low_3 <- v_Position("11011", s_zone_B_low)
  if ( id_zone_B_low_3 != -1){
    data_zone_B_low_3 <- table[id_zone_B_low_3:(id_zone_B_low_3 + 2), ]
  }
  
  id_zone_B_low_4 <- v_Position("10111", s_zone_B_low)
  if ( id_zone_B_low_4 != -1){
    data_zone_B_low_4 <- table[id_zone_B_low_4:(id_zone_B_low_4 + 2), ]
  }
  
  Fin <- do.call(rbind, list(data_zone_B_up, data_zone_B_up_2, data_zone_B_up_3, data_zone_B_up_4,
                            data_zone_B_low, data_zone_B_low_2, data_zone_B_low_3, data_zone_B_low_4))
  return(Fin)
}


### Критерий 7: 15 последовательных точек в зоне С -----------------------------------------------

Shewchart_7 <- function(table, sample, average, sigma){
  data_zone_C <- NULL
  lower_c <- average - sigma
  upper_c <- average + sigma
  
  s_zone_C <- s_Sequence_in(sample, lower_c, upper_c)
  id_zone_C <- v_Position("111111111111111", s_zone_C)
  
  if (id_zone_C != -1){
    data_zone_C <- table[id_zone_C:(id_zone_C + 14), ]
    row.names(data_zone_C)  <- c(1:nrow(data_zone_C))
  } 
  Fin <- data_zone_C
  return(Fin)
}


### Критерий 8: 8 последовательных точек НЕ в зоне С -----------------------------------------------    

Shewchart_8 <- function(table, sample, average, sigma){
  data_zone_C <- NULL
  s_zone_C <- s_Sequence_in(sample, average - sigma, average + sigma)
  id_zone_C <- v_Position("00000000", s_zone_C)
  if (id_zone_C != -1){
    data_zone_C <- table[id_zone_C:(id_zone_C + 7), ]
  } 
  Fin <- data_zone_C
  return(Fin)
}
