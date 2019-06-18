#compute the regression equation
regressionModel <- function(data_set, dependent_var, independent_var){
  #we make sure that we don't have negative values for the log transformation
  if (!all(dependent_var >= 0)){
    dependent_var <- dependent_var - min(dependent_var)
  }
  dependent_var <- replace(dependent_var, dependent_var == 0, 0.000001)
  if (length(data_set) == 3){
    fit <- lm(log(dependent_var)~ independent_var)
  } else {
    fit <- lm(log(dependent_var)~ . , data = independent_var)
  }
  
  return (fit)
}

#write the litteral equation
getFormula <- function(coef, dependent, independents, data_set, model_type){
  
  if (length(data_set) == 3){
    y <- paste(dependent, "= e ^ (", independents, "*", "(", round(coef[2, 1], 5), ")", "+", coef[1, 1])
  }else{
    y <- paste(dependent, "=", "e ^ (")
    for (i in 1 : length(independents)){
      y <- paste(y, "(",  round(coef[i+1, 1], 5), ")", "*" , independents[i], "+")
    }
    y <- paste(y, "(", round(coef[1, 1], 5), ")")
  }
  
  if (all(data_set[, 2] >= 0)){
    return (paste(y, ")"))
  } else {
    return (paste(y, ")", min(data_set[, 2])))  
  }
  
}

#compute the values using the regression equation
getEquation <- function(coef, data, data_set, model_type){
  
  if (length(data_set) == 3){
    y <- coef[1] + data * coef[2]
  } else {
    y <- coef[1]
    for (i in 1 : length(data)){
      y <- y + coef[i + 1] * data[i]
    }
  }
  if (all(data_set[, 2] >= 0)){
    return (exp(y))
  } else {
    return (exp(y) + min(data_set[, 2]))
  }
  
  
}
