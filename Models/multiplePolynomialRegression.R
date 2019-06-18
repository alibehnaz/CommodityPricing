
#compute the regression equation
regressionModel <- function(data_set, dependent_var, independent_var){
  
  if (length(data_set) == 3){
    fit <- lm(dependent_var ~ I(independent_var^2) + independent_var)
  } else {
    fit <- lm(dependent_var ~ ., data =  I(independent_var^2) + independent_var)
  }
  
  return (fit)
  
}

#write the litteral equation
getFormula <- function(coef, dependent, independents, data_set, model_type){
  
  if (length(data_set) == 3){
    y <- paste(dependent, "(", independents, "^ 2 +", independents, ") *", "(", round(coef[2, 1], 5), ")", "+", coef[1, 1])
  }else{
    y <- paste(dependent, "=")
    for (i in 1 : length(independents)){
      y <- paste(y, "(",  round(coef[i+1, 1], 5), ")", "*" , "(", independents[i], "^ 2 +", independents[i], ")", "+")
    }
    y <- paste(y, "(", round(coef[1, 1], 5), ")")
  }
  
  return (paste(y, ")"))
}

#compute the values using the regression equation
getEquation <- function(coef, data, data_set, model_type){
  
  if (length(data_set) == 3){
    y <- coef[1] + (data^2 + data) * coef[2]
  } else {
    y <- coef[1]
    for (i in 1 : length(data)){
      y <- y + coef[i + 1] * (data[i]^2 + data[i])
    }
  }
  
  return (y)
}
