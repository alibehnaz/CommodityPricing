library(shiny)
library(shinyBS)
library(Quandl)
library(XLConnect)
library(MASS)  
source("functions.R")                   

shinyServer(function(input, output, session) {
  
  ###########################################################################################################
  
  observe({
    ################# Dynamic country / commodity selection ########################
    
    countries <<- c(input$tab1Country, input$tab2Country, input$tab3Country)
    commodities <<- c(input$tab1Commodity, input$tab2Commodity, input$tab3Commodity) 
    
    ################################################################################
                    
    ################# Dynamic data selection according to the country / commodity / model selected ########################
    withProgress(message = "Computing ...", {
      selectable_data_sets <<- getSelectableDataSets(countries, commodities)
      selectable_models <<- getSelectableModels()

      lapply(1:length(selectable_data_sets[[1]]), function(i) {
        total_data <- c(selectable_data_sets[[1]][[i]], selectable_data_sets[[3]][[i]])
        output[[paste0("tab", i, "Selectable")]] <- renderUI({
          list(
            selectInput(paste0("tab", i,"Dependent"), "Choose the Dependent Variable:",choices = total_data),
            hr(),
            selectInput(paste0("tab", i, "Independent"), "Choose the Independent Variables:", choices = total_data, multiple = TRUE),
            hr()
          )   
        })
        
        output[[paste0("tab", i, "Models")]] <- renderUI({
          list(
            selectInput(paste0("tab", i, "ModelType"), "Choose a type of Model", choices = selectable_models[[1]]),
            hr()
          )
        })
      }) 
    })
    
    ################################################################################
    
    #get the number of simulation to run
    number_simu <<- input$number_simu
    
    #get value everytime a button is clicked
    launch1 <<- as.numeric(input$tab1do)
    launch2 <<- as.numeric(input$tab2do)
    launch3 <<- as.numeric(input$tab3do)
    
  })
  
  
  
  
  #################################"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!################################################
  lapply(1 : 3, function(i){
    observeEvent(input[[paste0("tab", i, "do")]], {
      
      withProgress(message = "Computing ...", value = 0, {
        ############ we get the data from dependent and independent variables inputs ##################
        
        total_data <- c(selectable_data_sets[[1]][[i]], selectable_data_sets[[3]][[i]])
        
        assign(paste0("tab", i, "_independents"), input[[paste0("tab", i,"Independent")]], envir = .GlobalEnv)
        assign(paste0("tab", i, "_dependent"), input[[paste0("tab", i,"Dependent")]], envir = .GlobalEnv)
        assign(paste0("tab", i, "_checkbox"), input[[paste0("tab", i,"stepwise")]], envir = .GlobalEnv)
        assign(paste0("tab", i, "_selected_model"), input[[paste0("tab", i,"ModelType")]], envir = .GlobalEnv)
        
        ##############################################################################################
             
        #################### if we apply a stepwise regression on all datasets we make sure to not include the dep variable ###################################
        if (eval(parse(text = paste0("tab", i, "_checkbox"))) == 1){
          
          assign(paste0("tab", i, "_independents"), total_data[total_data != eval(parse(text = paste0("tab", i, "_dependent")))], envir = .GlobalEnv)
          
        }
        
        #######################################################################################################################################################
        
        ############################################### we get the codes / file names that we are going to use to get the data #########################################
        
        codes <- getCode(c(eval(parse(text = paste0("tab", i, "_dependent"))), eval(parse(text = paste0("tab", i, "_independents")))), selectable_data_sets[[1]][[i]], selectable_data_sets[[3]][[i]], selectable_data_sets[[2]][[i]], selectable_data_sets[[4]][[i]], paste0(countries[i], "/", commodities[[i]]))
        assign(paste0("tab", i, "_data_codes"), codes, envir = .GlobalEnv)
        
        ####################################################################################################################################################################
        
        ####################################### We verify that independents values have been inputted and run the computation ####################################################
        if (is.null(eval(parse(text = paste0("tab", i, "_independents")))) && (eval(parse(text = paste0("tab", i, "_checkbox"))) == 3 || eval(parse(text = paste0("tab", i, "_checkbox"))) == 2)){
          
          #TODO Insert Error MSG
          
        } else if (length(eval(parse(text = paste0("tab", i, "_independents")))) == 1 && eval(parse(text = paste0("tab", i, "_checkbox"))) == 2) {
          
          #TODO Insert Error MSG
          
          
        } else { 
          
          incProgress(1, detail = "Building Model") 
          
          ########################### We Compute the model and assign the important values #######################################################################################################
          results <- computation(eval(parse(text = paste0("tab", i, "_dependent"))), eval(parse(text = paste0("tab", i, "_independents"))), eval(parse(text = paste0("tab", i, "_data_codes"))), selectable_data_sets[[2]][[i]], selectable_data_sets[[4]][[i]], paste0(countries[i], "/", commodities[[i]]), eval(parse(text = paste0("tab", i, "_checkbox"))), eval(parse(text = paste0("tab", i, "_selected_model"))))
          assign(paste0("tab", i, "_last_values"), results[[6]][nrow(results[[6]]), ], envir = .GlobalEnv)
          assign(paste0("tab", i, "_coef"), coefficients(results[[1]]), envir = .GlobalEnv)
          assign(paste0("tab", i, "_data_set"), results[[6]], envir = .GlobalEnv)
          assign(paste0("tab", i, "_new_independents"), results[[7]], envir = .GlobalEnv)
          assign(paste0("tab", i, "independents_variance"), results[[9]], envir = .GlobalEnv)
                     
          
          ########################################################################################################################################################################################
          
          incProgress(5, detail = "Preparing Output")
          
          
          ########################### displays the residuals graphs ####################################################
          output[[paste0("tab", i, "Plot")]] <- renderPlot({
            graphics::layout(matrix(c(1,2,3,4),2,2))  
            plot(results[[1]])
            layout
          })
                
          ##############################################################################################################
          
          ################################# renders the range of Data ###################################################
          output[[paste0("tab", i, "Period")]] <- renderText({
            paste("Data from :", results[[6]][1,1],"to", results[[6]][nrow(results[[6]]),1])
          })      
          
          ###############################################################################################################
          
          ###################################### renders the graph : "actual values vs values computed by the model" ###############################################
          output[[paste0("tab", i, "PlotComparison")]] <- renderPlotly({
            data_set <- results[[6]]
            
            if (length(eval(parse(text = paste0("tab", i, "_new_independents")))) == 1){
              computed_value <- results[[3]]
            } else {
              computed_value <- results[[3]][, 1]
            }
            historical_value <- results[[6]][, 2]
            print(historical_value)
            print("###############")
            print(results[[3]])
            regression <- lm(historical_value ~ computed_value ) 
            predicted <- predict(regression, newdata = data_set, interval = "confidence", level = 0.95)
            
            upper_band <- cbind(computed_value, predicted[, 2])
            upper_band <- upper_band[order(upper_band[, 1]),]
            
            lower_band <- cbind(computed_value, predicted[, 3])
            lower_band <- lower_band[order(lower_band[, 1]),]
            
            g <- plot_ly(data_set, x = computed_value, y = historical_value, mode = "markers", showlegend = FALSE) %>%
              add_trace(y = predicted[, 1], x = computed_value, name = "Regression Line", mode = "lines", line = list(color = "#00BFC4"), showlegend = TRUE) %>%
              add_trace(y = upper_band[, 2], x = upper_band[, 1], mode = "lines", line = list(color = "grey"), showlegend = TRUE, name = "Confidence Interval 95%") %>%
              add_trace(y = lower_band[, 2], x = lower_band[, 1], mode = "lines", line = list(color = "grey"), showlegend = FALSE, name = "Confidence Interval 95%") %>%
              layout(title = "Values comparison : Historical vs Computed", yaxis = list(title = eval(parse(text = paste0("tab", i, "_dependent"))), zeroline = FALSE, showline = FALSE), xaxis = list(title = "Computed values", zeroline = FALSE, showline = FALSE), margin = list(l = 70, r = 40, t = 70, b = 50, pad = 4))
            g
          })    
             
          output[[paste0("tab", i, "PlotTimeSeries")]] <- renderPlotly({
            data_set <- results[[6]]
            date <- results[[6]][, 1]
            historical_value <- results[[6]][, 2]
            
            if (length(eval(parse(text = paste0("tab", i, "_new_independents")))) == 1){
              computed_value <- results[[3]]
            } else {
              computed_value <- results[[3]][, 1]
            }
            
            g <- plot_ly(data_set, x = date, y = historical_value, name = "Historical", mode = "lines", line = list(color = "#00BFC4")) %>%
              add_trace(y = computed_value, x = date, name = "Computed", mode = "lines", line = list(color = "#F77D74")) %>%
              layout(title = "Time Series comparison : Historical vs Computed", yaxis = list(title = eval(parse(text = paste0("tab", i, "_dependent"))), zeroline = FALSE, showline = FALSE), xaxis = list(title = "Date"), showlegend = TRUE, margin = list(l = 70, r = 40, t = 70, b = 50, pad = 4))
            g
          })
          
          ###########################################################################################################################################################
          
          ################################## renders the coefficients, P-Value, t-statistics for the model's factors ##############################################
          output[[paste0("tab", i, "Table")]] <- renderTable(digits = 4, display = c("s", "g", "g", "g", "g"), {
            coefficients(results[[2]])
          })
          
          ##########################################################################################################################################################
          
          ################################## renders the ANOVA Table ##############################################
          output[[paste0("tab", i, "ANOVA")]] <- renderTable(digits = 4, display = c("s", "g", "g", "g", "g", "g"), {
            anova(results[[1]])
          })
          
          ##########################################################################################################################################################
          
          ################################## renders the Variance/Covariance matrix of the model paramters ##############################################
          output[[paste0("tab", i, "vcovar")]] <- renderTable(digits = 4, display = c(rep("g", ncol(vcov(results[[1]])) + 1)), {
            vcov(results[[1]])
          })
          
          ##########################################################################################################################################################
          
          ########################### renders the model's formula ############################################################################
          output[[paste0("tab", i, "Formula")]] <- renderText({
            results[[4]]
          })       
          
          ####################################################################################################################################
          
          ############################## renders the R² ################################################
          output[[paste0("tab", i, "rSquared")]] <- renderUI({
            strong(paste("R-squared:", results[[5]]))
          })
          
          ##############################################################################################
          
          ########################################### renders all the slider inputs in the forecast tab ################################################################################
          output[[paste0("tab", i, "Parameters")]] <- renderUI({
            if (!is.null(length(eval(parse(text = paste0("tab", i, "_new_independents")))))){
              list(
                bsCollapse(id = NULL, open = paste(eval(parse(text = paste0("tab", i, "_dependent"))), "parameters"),
                           bsCollapsePanel(strong(paste(eval(parse(text = paste0("tab", i, "_dependent")))), "parameters"),
                                           wellPanel(style = "overflow-y:scroll; max-height: 450px",
                                                     lapply(1 : length(eval(parse(text = paste0("tab", i, "_new_independents")))), function(j){ 
                                                       minimum <- round(((((eval(parse(text = paste0("tab", i, "_last_values")))[j + 2]) - 2 * sqrt(results[[9]][j])) / (eval(parse(text = paste0("tab", i, "_last_values")))[j + 2])) - 1) * 100, 1)
                                                       maximum <- round(((((eval(parse(text = paste0("tab", i, "_last_values")))[j + 2]) + 2 * sqrt(results[[9]][j])) / (eval(parse(text = paste0("tab", i, "_last_values")))[j + 2])) - 1) * 100, 1)
                                              
                                                       if (maximum > minimum){
                                                         sliderInput(paste0("tab", i, "_param", j), paste(strsplit(eval(parse(text = paste0("tab", i, "_new_independents")))[j], "(", fixed = TRUE)[[1]][1], "(% change)"), value = 0, min = minimum, max = maximum, step = 0.1)
                                                       } else {
                                                         sliderInput(paste0("tab", i, "_param", j), paste(strsplit(eval(parse(text = paste0("tab", i, "_new_independents")))[j], "(", fixed = TRUE)[[1]][1], "(% change)"), value = 0, min = maximum, max = minimum, step = 0.1)
                                                         
                                                       }
                                                     })
                                           )
                                           
                           )
                )
                
              )
            }
          })
          ###############################################################################################################################################################################
          
          
          ############################################################## renders the title in the result page which triggers the display ##########################################################
          
          output[[paste0("tab", i, "_displayResults")]] <- renderUI({
            h2("Analysis Results")
          })
          
          ##########################################################################################################################################################################################
             
          ######################################################### Render The download button and create the file to download #######################################################################
          
          output[[paste0("tab", i, "_downloadData")]] <- downloadHandler(filename = function(){
            paste0(eval(parse(text = paste0("tab", i, "_dependent"))), "-Analysis-", Sys.Date(), ".txt")
          },
          content = function(con){
            text <- paste("################################################ INPUT INFORMATION ################################################",
                          "\n",
                          "\n",
                          "Date:", Sys.Date(),
                          "\n",
                          #                           "Dependent Variable:", eval(parse(text= paste0("tab", i, "_dependent"))), 
                          #                           "\n",
                          #                           "Independent Variables:", eval(parse(text= paste0("tab", i, "_independents"))),
                          #                           "\n",
                          #                           "Model Selected:", eval(parse(text= paste0("tab", i, "_selected_model"))),
                          "\n",
                          "################################################ MODEL INFORMATION ################################################",
                          "\n"
                          #                           "\n",
                          #                           "Independent Variables retained by the model:", eval(parse(text= paste0("tab", i, "_new_independents"))),
                          #                           "\n",
                          #                           "----> Model Key figures:",
                          #                           "\n",
                          #                           summary(results[[1]]),
                          #                           "\n",
                          #                           "----> Analysis of Variance",
                          #                           "\n",
                          #                           anova(results[[1]]),
                          #                           "\n",
                          #                           "----> Variance/ covariance Matrix",
                          #                           "\n",
                          #                           vcov(results[[1]])
                          
            )
            write.table(text, con)
          })
          
          #################################################################################################################################
          
          
        }
      })
    })
  })
  
  #############################################!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##################################################"
  
  ############################################ Create the input parameters for the forecast page #############################################################
  
  output$inputParameters <- renderUI({
    lapply(1 : 3, function(i){
      list(
        conditionalPanel(paste0("output.tab", i, "_displayResults"),
                         uiOutput(paste0("tab", i, "Parameters"))
        )  
      )
    })
  })
  
  ##################################################################################################################################################################
  
  
  
  
  
  ############################################ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ####################################################
  
  lapply(1 : 3, function(i){  
    observeEvent(input$tab4do, {
      
      withProgress(message = "Computing ...", value = 0, {
        
        scenarii_building_choice <- input$Extreme_scenarii_building
        
        #get how the inputs evolve overtime in the case of several years of forecast
        forecast_evolution <- input$inputs_evolution
        
        if (scenarii_building_choice == 1){
          scenarii_building_value <- input$distance_normal_scenario_percent / 100
        } else if (scenarii_building_choice == 2) {
          scenarii_building_value <- input$distance_normal_scenario_std
        }
        
        #####################  check whether a model has been computed for the tab and compute the new values of the factors inputted #################################################
        if(eval(parse(text = paste0("launch", i))) != 0 && !is.null(eval(parse(text = paste0("tab", i, "_independents"))))){
          independents_variances <- eval(parse(text = paste0("tab", i, "independents_variance")))
          tab_inputs <- getForecastScenarii(eval(parse(text = paste0("tab", i, "_new_independents"))), paste0("tab", i, "_param"), input, eval(parse(text = paste0("tab", i, "_last_values"))), number_simu, scenarii_building_choice, scenarii_building_value, independents_variances, forecast_evolution)
        } else {         
          tab_inputs <- NULL    
        }
        ####################################################################################################################################################################################
                 
        incProgress(1, detail = "Calculating Estimates")
        
        #########################  runs only if a model has been computed in a tab ########################################################################################################
        if (!is.null(tab_inputs[[1]])){
          tab_max_forecast <- c(eval(parse(text = paste0("tab", i, "_last_values")))[2][1, 1])
          tab_min_forecast <- c(eval(parse(text = paste0("tab", i, "_last_values")))[2][1, 1])
          tab_normal_forecast <- c(eval(parse(text = paste0("tab", i, "_last_values")))[2][1, 1])
          
          ########################  compute the low, normal and high scenarii   #############################################
          for (j in seq (1, length(tab_inputs), 3)){
            scenario_inputs <- list(tab_inputs[[j]], tab_inputs[[j + 1]], tab_inputs[[j + 2]])
            scenarii <- renderScenario(eval(parse(text = paste0("tab", i, "_coef"))), scenario_inputs, eval(parse(text = paste0("tab", i, "_data_set"))), eval(parse(text = paste0("tab", i, "_selected_model"))) )
            tab_max_forecast <- append(tab_max_forecast, max(scenarii[[4]]))
            tab_min_forecast <- append(tab_min_forecast, min(scenarii[[4]]))
            tab_normal_forecast <- append(tab_normal_forecast, scenarii[[4]][2])
              
            
            ######################################################################################################################
            
            incProgress(1, detail = "Preparing Output")
            
            ############################################# renders the low, normal and high Scenarii ##############################################                   
            if(j == 1){ 
              output[[paste0("tab", i, "_low_scenario",j)]] <- scenarii[[1]] 
              output[[paste0("tab", i, "_high_scenario",j)]] <- scenarii[[3]]
              output[[paste0("tab", i, "_normal_scenario",j)]] <- scenarii[[2]]
              
            } else {
              output[[paste0("tab", i, "_low_scenario",((j - 1) / 3) + 1)]] <- scenarii[[1]] 
              output[[paste0("tab", i, "_high_scenario",((j - 1) / 3) + 1)]] <- scenarii[[3]]
              output[[paste0("tab", i, "_normal_scenario",((j - 1) / 3) + 1)]] <- scenarii[[2]]
              
            }
          }
          
          cases_tab <- c(paste0("tab", i, "_low_scenario"), paste0("tab", i, "_high_scenario"), paste0("tab", i, "_normal_scenario"))
          estimates_tab <- renderEstimates(number_simu, cases_tab)  
          output[[paste0("tab", i, "_low_estimates")]] <- estimates_tab[[1]]            
          output[[paste0("tab", i, "_high_estimates")]] <- estimates_tab[[2]]  
          output[[paste0("tab", i, "_normal_estimates")]] <- estimates_tab[[3]]  
          
          ########################################################################################################################################################
                        
          ################################################ Render the forecast Plot #######################################################################
          tab_forecast_dates <- getForecastDates(eval(parse(text = paste0("tab", i, "_last_values")))[1], number_simu)
          tab_df <- createPlotDataFrame(tab_forecast_dates, tab_max_forecast, tab_min_forecast, tab_normal_forecast, eval(parse(text = paste0("tab", i , "_data_set"))))
         
          output[[paste0("plot_", "tab", i, "_forecast")]] <- renderPlotly({
            high_scenario_value <- tab_df[, 2]
            low_scenario_value <- tab_df[, 3]
            normal_scenario_value <- tab_df[, 4]
            historical_value <- tab_df[, 5]
            g <- plot_ly(tab_df, x = Date, y = historical_value, name = "Historical Values", mode = "markers+lines", marker = list(color = "black"), line = list(color = "black")) %>%
              add_trace(y = normal_scenario_value, x = Date, name = "Normal Forecast", mode = "markers+lines", marker = list(color = "blue"), line = list(color = "blue")) %>%
              add_trace(y = low_scenario_value, x = Date, name = "Extreme Low Forecast", mode = "markers+lines", marker = list(color = "green"), line = list(color = "green")) %>%
              add_trace(y = high_scenario_value, x = Date, name = "Extreme High Forecast", mode = "markers+lines", marker = list(color = "red"), line = list(color = "red")) %>%
              layout(title = paste("Forecast -", eval(parse(text = paste0("tab", i, "_dependent")))), yaxis = list(title = eval(parse(text = paste0("tab", i, "_dependent"))), zeroline = FALSE, showline = FALSE), xaxis = list(title = "Date"), showlegend = TRUE, margin = list(l = 70, r = 40, t = 70, b = 50, pad = 4))
            g   
            
          })
          
          #############################################################################################################################################################"
          
          ################################## print title ###################################################
          output[[paste0("tab", i, "_display")]] <- renderUI({
            h3(paste0("Forecast - ", eval(parse(text = paste0("tab", i, "_dependent")))))
          })
          
          #############################################################################################################"
        }
        #     #################################################################################################################################################     
      })
    })
    
    
    
  })
  
  ########################################### Displays the forecast Results ################################################
  
  
  
  output$forecast_results <- renderUI({
    lapply(1 : 3, function(i){
      list(
        uiOutput(paste0("tab", i, "_display")),
        conditionalPanel(paste0("output.", "tab", i, "_display"),
                         tabsetPanel(type = "tabs",
                                     tabPanel("Graph",
                                              h4("Graph"),
                                              plotlyOutput(paste0("plot_", "tab", i, "_forecast")),
                                              hr()
                                     ),
                                     tabPanel("Forecast Scenarii",
                                              column(4,
                                                     h4("Extreme Low Scenario", align = "center"),
                                                     uiOutput(paste0("tab", i, "_low_estimates")),
                                                     hr()
                                              ),
                                              column(4,
                                                     h4("Normal Scenario", align = "center"),
                                                     uiOutput(paste0("tab", i, "_normal_estimates")),
                                                     hr()
                                              ),
                                              column(4,
                                                     h4("Extreme High Scenario", align = "center"),
                                                     uiOutput(paste0("tab", i, "_high_estimates")),
                                                     hr()
                                              )
                                     )
                         )
                         
                         
        )
        
      )
    })
  })
  
  output$forecast_horizon <- renderUI({
    bsCollapse(id = NULL, open = "Forecast Horizon",
               bsCollapsePanel(strong("Forecast Horizon"),
                               sliderInput("number_simu", "Number of years to Forecast", value = 3, min = 1, max = 5)
               )
    )
  })
  
  output$extreme_scenarii_building <- renderUI({
    bsCollapse(id = NULL, open = "Extreme Scenarii Building",
               bsCollapsePanel(strong("Extreme Scenarii Building"),
                               radioButtons("Extreme_scenarii_building", "Distance to the normal scenario in :", choices = c("percentage value" = 1, "number of standard deviations" = 2)),
                               conditionalPanel("input.Extreme_scenarii_building == 1",
                                                sliderInput("distance_normal_scenario_percent", "Percentage Difference between the Extreme inputs and the normal ones", value = 5, min = 0, max = 100, step = 0.1)
                               ),
                               conditionalPanel("input.Extreme_scenarii_building == 2",
                                                sliderInput("distance_normal_scenario_std", "Number of standard deviations between the extreme inputs and normal ones", value = 2, min = 0, max = 3, step = 0.01)
                               )
               )
    )
  })
  
  output$Forecast_Evolution <- renderUI({
    conditionalPanel("input.number_simu != 1",
                     bsCollapse(id = NULL, open = "Forecast Inputs Evolution",
                                bsCollapsePanel(strong("Forecast Inputs Evolution"),
                                                selectInput("inputs_evolution", "Evolution of the inputs through the years", choices = c("Linear" = 1))
                                )
                     )
    )
  })
  
  
  ##########################################################################################################################
  
  ############################################ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ####################################################
  
})
