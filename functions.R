# Xavier de Certaines - Date : 01/06/16#


#retrieve the QUANDL code for the online datasets or the path to the file for the local datasets
getCode <- function(selected, online_names, local_names, codes, files, input){
  final_codes <- c()
  for (i in 1 : length(selected)){
    if (selected[i] %in% online_names){
      index <- match(selected[i], online_names)
      code <- codes[index]
      final_codes <- append(final_codes, code)
    }else if (selected[i] %in% local_names){
      index <- match(selected[i], local_names)
      code <- paste0("Data/", input, "/", files[index])
      final_codes <- append(final_codes, code)
    }
  }
  results <- final_codes
  return(results)
  
}

#retrieve the data from QUANDL and or the files and merge everything in one dataframe
getData <- function(codes, online_codes, files, input){
  data <- NULL
  if (!is.null(codes)){
    for (i in 1 : length(codes)){
      if(i == 1){
        if (codes[i] %in% online_codes ){
          Quandl.api_key("yJM_dSwza7UMCMYznzWt")
          data <- Quandl(codes[i], collapse = "annual")
        } else if (codes[i] %in% paste0("Data/",input,"/",files) ){
          wb <- loadWorkbook(codes[i])
          wb_data <- readWorksheet(wb, sheet = 1)
          wb_data$Date <- as.Date(as.character(wb_data$Date))
          data <- wb_data
        }
      } else {
        if (codes[i] %in% online_codes ){
          Quandl.api_key("yJM_dSwza7UMCMYznzWt")
          temp <- Quandl(codes[i], collapse = "annual")
          colnames(temp)[1] <- "Date"
          data <- merge(data, temp, by = "Date", all = TRUE)
        } else if (codes[i] %in% paste0("Data/",input,"/",files) ){
          wb <- loadWorkbook(codes[i])
          wb_data <- readWorksheet(wb, sheet = 1)
          wb_data$Date <- as.Date(as.character(wb_data$Date))
          data <- merge(data, wb_data, by ="Date", all = TRUE)
        }
      }
     }
  }
  #add case where codes[[2]] isn't null
  return(data)
}

# Compute the regression 
computation <- function(dependent, independents, data_codes, codes, files, input, stepwise, model_type){
  data_set <- getData(data_codes, codes, files, input)
  data_set <- as.data.frame(data_set)
  colnames(data_set) <- c("Date", dependent, independents)
  data_set <- data_set[complete.cases(data_set),]
  dates <- data_set[, 1]
  dependent_var <- data_set[, 2]
  independent_var <- data_set[, 3 : length(data_set)]
  variance_dependent <- var(dependent_var)
  
  if (length(independents) > 1){
    variance_independents <- diag(var(independent_var))
  } else if (length(independents) == 1){
    variance_independents <- var(independent_var)
  }
  
  
  for (i in 1 : length(selectable_models[[1]])){
    if (model_type == selectable_models[[1]][i]){
      source(paste0("Models/", selectable_models[[2]][i]))
    }
  }
  
  fit <- regressionModel(data_set, dependent_var, independent_var)

  #case where stepwise is to be applied
  if (stepwise == 1 || stepwise == 2){
    step <- stepAIC(fit, direction="both")
    global_res <- summary(step)
    selected_independents <- rownames(coefficients(global_res))[2 : length(rownames(coefficients(global_res)))]
    selected_independents <- gsub("`", "", selected_independents)
    selected_independents_var <- data_set[, selected_independents]
    selected_data_set <- data_set[, c("Date", dependent, selected_independents)]
    regression_equation <- getEquation(coefficients(step), selected_independents_var, selected_data_set, model_type)
    formula <- getFormula(coefficients(global_res), dependent, selected_independents, data_set, model_type)
    adjusted_r_squared <- global_res$adj.r.squared
    
    if (length(selected_independents) > 1){
      variance_new_independents <- diag(var(selected_independents_var))
      } else if (length(selected_independents) == 1){
      variance_new_independents <- var(selected_independents_var)
    }
    
    
    
    result <- list(step, global_res, regression_equation, formula, adjusted_r_squared, selected_data_set, selected_independents, variance_dependent, variance_new_independents)
  } else {
    #no stepwise regression
    global_res <- summary(fit)
    regression_equation <- getEquation(coefficients(fit), independent_var, data_set, model_type)
    formula <- getFormula(coefficients(global_res), dependent, independents, data_set, model_type)
    adjusted_r_squared <- global_res$adj.r.squared
    
    result <- list(fit, global_res, regression_equation, formula, adjusted_r_squared, data_set, independents, variance_dependent, variance_independents)
  }
  return(result)
}

#get the inputs values in the forecast tab and computes the new factors values
getListInputs <- function(independents, nameParam, input, last_values){
  inputs_list <- c()
  value_list <- c()
  if (length(independents != 0)){
    for (i in 1 : length(independents)){
      input_name <- paste0(nameParam, i)
      value <- eval(parse(text = paste0("input$",input_name))) / 100
      value_list <- append(value_list, value)
      input_computed <- as.numeric(last_values[i + 2]) + (eval(parse(text = paste0("input$",input_name))) / 100) * as.numeric(last_values[i + 2])
      
      inputs_list <- append(inputs_list, input_computed)
    }
    results <- list(inputs_list, value_list)
    return(results)
  }
}

#computes the maximized and minimzed value for the factors
getInputScenarii <- function(independents, nameParam, input, last_values, scenarii_building_choice, scenarii_building_value, independents_variance){
  normal_inputs <- getListInputs(independents, nameParam, input, last_values)[[1]]
  if (scenarii_building_choice == 1){
    maximized_inputs <- (1 + scenarii_building_value) * normal_inputs
    minimized_inputs <- (1 - scenarii_building_value) * normal_inputs
  } else if (scenarii_building_choice == 2){
    maximized_inputs <-  normal_inputs + (scenarii_building_value * sqrt(independents_variance))
    minimized_inputs <-  normal_inputs - (scenarii_building_value * sqrt(independents_variance))
  }
  

  results <- list(normal_inputs, maximized_inputs, minimized_inputs)
  return (results)
}

#print the low, normal, high scenarii
renderScenario <- function(coef, inputs, data_set, model_type){
  estimates <- c()
  current_value <- data_set[nrow(data_set), 2]
  
  #load right model
  for (i in 1 : length(selectable_models[[1]])){
    if (model_type == selectable_models[[1]][i]){
      source(paste0("Models/", selectable_models[[2]][i]))
    }
  }
  
  for (i in 1 : length(inputs)){
    
    val <- getEquation(coef, inputs[[i]], data_set, model_type)
    estimates <- append(estimates, round(as.numeric(val), 2))
    estimates <- sort(estimates, decreasing = FALSE)
  }
  
  max_scenario <- renderUI({
    estimate <- max(estimates)
    variation <- round(((estimate - current_value) / current_value) * 100, 2)
    list(
      p(paste("Current Value :", round(current_value, 2))),
      p(paste("Forecast Estimate :", estimate)),
      p(paste("Variation :", variation, "%"))
    )
  })
  
  min_scenario <- renderUI({
    estimate <- min(estimates)
    variation <- round(((estimate - current_value) / current_value) * 100, 2)
    list(
      p(paste("Current Value :", round(current_value, 2))),
      p(paste("Forecast Estimate :", estimate)),
      p(paste("Variation :", variation, "%"))
    )
  })
  
  normal_scenario <- renderUI({
    estimate <- estimates[2]
    variation <- round(((estimate - current_value) / current_value) * 100, 2)
    list(
      p(paste("Current Value :", round(current_value, 2))),
      p(paste("Forecast Estimate :", estimate)),
      p(paste("Variation :", variation, "%"))
    )
  })
  
  result <- list(min_scenario, normal_scenario, max_scenario, estimates)
  return(result)
}

#compute the forecasts for the number of years inputted
getForecastScenarii <- function(independents, nameParam, input, last_values, number_simu, scenarii_building_choice, scenarii_building_value, independents_variance, forecast_evolution){
  scenarii <- list()
  for (i in 1 : number_simu){
    if (i == 1){
      scenario <- getInputScenarii(independents, nameParam, input, last_values, scenarii_building_choice, scenarii_building_value, independents_variance)
      scenarii <- append(scenarii, scenario)
    } else {
      
      if (forecast_evolution == 1 ){
        scenario <- lapply(getInputScenarii(independents, nameParam, input, last_values, scenarii_building_choice, scenarii_building_value, independents_variance), function(x){
          x * ((1 + getListInputs(independents, nameParam, input, last_values)[[2]]) ^ (i - 1))
        })
        scenarii <- append(scenarii, scenario)
      }
    }
  }
  return(scenarii)
}

#renders the value of the forecast for each scenario and each year
renderEstimates <- function(number_simu, cases){
  estimate1 <- renderUI({
    lapply(1 : number_simu, function(x){
      list(
        strong(paste("Year", x, ":")),
        uiOutput(paste0(cases[1], x))
        )
    })
  })
  
  estimate2 <- renderUI({
    lapply(1 : number_simu, function(x){
      list(
        strong(paste("Year", x, ":")),
        uiOutput(paste0(cases[2], x))
      )
    })
  })
  
  estimate3 <- renderUI({
    lapply(1 : number_simu, function(x){
      list(
        strong(paste("Year", x, ":")),
        uiOutput(paste0(cases[3], x))
      )
    })
  })
  
  results <- list(estimate1, estimate2, estimate3)
  return(results)
}

#Compute the dates in the future where we are going to forecast
getForecastDates <- function(last_date, number_simu){
  dates <- c()
  for (i in 0 : number_simu){
    if (i == 0){
      dates <- append(dates, last_date[1,1])
    } else {
      new_date <- last_date[1,1] + i * 365
      dates <- append(dates, as.Date(new_date))
    }
  }
  return (dates)
}

#create the dataframe to use for plotting the forcast lines
createPlotDataFrame <- function(dates, maxi, mini, normal, data_set){
  A <- data.frame(Date = dates, max = maxi, min = mini, normal = normal)
  B <- data_set[, 1 : 2]
  df <- merge(A, B, by = "Date", all = TRUE)
  return(df)
}

#we get the datasets avalaible for the country/commodity selected
getSelectableDataSets <- function(countries, commodities){
  local_names <- list()
  local_codes <- list()
  online_names <- list()
  online_codes <- list()
  for ( i in 1 : length(countries)){
    url <- paste0("Data/", countries[i],"/", commodities[i], "/Data.xlsx")
    wb <- loadWorkbook(url)
    online_data <- readWorksheet(wb, sheet = 1, header = TRUE)
    online_names[[i]] <- online_data$Name
    online_codes[[i]] <- online_data$Code
    local_data <- readWorksheet(wb, sheet = 2, header = TRUE)
    local_names[[i]] <- local_data$Name
    local_codes[[i]] <- local_data$Code
  }
  result <- list(online_names, online_codes, local_names, local_codes)
  return(result)
}

getSelectableModels <- function(){
  name <- c()
  file <- c()
  url <- "Models/Models.xlsx"
  wb <- loadWorkbook(url)
  models <- readWorksheet(wb, sheet = 1, header = TRUE)
  name <- models$Name
  file <- models$File
  
  result <- list(name, file)
  
  return(result)
  
}