#compute the regression equation
regressionModel <- function(data_set, dependent_var, independent_var){
  
  if (length(data_set) == 3){
    fit <- lm(dependent_var~ independent_var)
  } else {
    fit <- lm(dependent_var~ . , data = independent_var)
  }
  
  return (fit)
}

#write the litteral equation
getFormula <- function(coef, dependent, independents, data_set, model_type){
  
  if (length(data_set) == 3){
    y <- paste(dependent, "=", independents, "*", "(", round(coef[2, 1], 5), ")", "+", coef[1, 1])
  }else{
    y <- paste(dependent, "=")
    for (i in 1 : length(independents)){
      y <- paste(y, "(",  round(coef[i+1, 1], 5), ")", "*" , independents[i], "+")
    }
    y <- paste(y, "(", round(coef[1, 1], 5), ")")
  }
  
  return (y)
}

#compute the values using the regression equation
getEquation <- function(coef, data, data_set, model_type){
  
  print(length(data_set))
  
  if (length(data_set) == 3){
    y <- coef[1] + data * coef[2]
    print("OK")
  } else {
    y <- coef[1]
    for (i in 1 : length(data)){
      y <- y + coef[i + 1] * data[i]
    }
  }
  
  return (y)
}
