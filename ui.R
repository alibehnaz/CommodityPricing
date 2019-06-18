library(plotly)

shinyUI(navbarPage("ANZ Hackathon", theme = "style.css",
                   tabPanel("Tab1 Analytics",
                            sidebarLayout(
                              # Sidebar with a slider and selection inputs
                              sidebarPanel(
                                h2("Build a Model"),
                                hr(),
                                selectInput("tab1Country", "Choose a Country", choices = countries),
                                hr(),
                                selectInput("tab1Commodity", "Choose a Commodity", choices = commodities),
                                hr(),
                                uiOutput("tab1Selectable"),
                                uiOutput("tab1Models"),
                                hr(),
                                radioButtons("tab1stepwise", "Stepwise Regression", choices = c("on all datasets" = 1, "on selected datasets" = 2, "no stepwise regression" = 3), selected = 3),
                                hr(),
                                actionButton("tab1do", "Launch")
                              ),
                              
                              # Show Word Cloud
                              mainPanel(
                                uiOutput("tab1_displayResults"),
                                conditionalPanel("output.tab1_displayResults",
                                                 tabsetPanel(type = "tabs",
                                                             tabPanel("Graph",
                                                                      h3("Model results graph"),
                                                                      hr(),
                                                                      h4("Values Comparison"),
                                                                      plotlyOutput("tab1PlotComparison"),
                                                                      h4("Time Series Comparison"),
                                                                      plotlyOutput("tab1PlotTimeSeries"),
                                                                      h4("Period"),
                                                                      textOutput("tab1Period"),
                                                                      hr()
                                                             ),
                                                             tabPanel("Model Information",
                                                                      h3("Regression Statistics"),
                                                                      hr(),
                                                                      h4("Regression Equation"),
                                                                      textOutput("tab1Formula"),
                                                                      br(),
                                                                      h4("Regression Coefficients"),
                                                                      uiOutput("tab1rSquared"),
                                                                      br(),
                                                                      tableOutput("tab1Table"),
                                                                      br(),
                                                                      h4("Analysis of Variance"),
                                                                      tableOutput("tab1ANOVA"),
                                                                      br(),
                                                                      h4("Variance/Covariance Matrix of the model's Parameters"),
                                                                      tableOutput("tab1vcovar"),
                                                                      br(),
                                                                      h4("Download Model Information"),
                                                                      downloadButton("tab1_downloadData", "Download Model Info"),
                                                                      hr()
                                                             ),
                                                             tabPanel("Residuals",
                                                                      h3("Residuals Analysis"),
                                                                      hr(),
                                                                      plotOutput("tab1Plot"),
                                                                      hr()
                                                             )
                                                 )
                                                 
                                                 
                                                 
                                                 
                                )
                                
                              )
                            )
                   ),
                   tabPanel("Tab2 Analytics",
                            sidebarLayout(
                              # Sidebar with a slider and selection inputs
                              sidebarPanel(
                                h2("Build a Model"),
                                hr(),
                                selectInput("tab2Country", "Choose a Country", choices = countries),
                                hr(),
                                selectInput("tab2Commodity", "Choose a Commodity", choices = commodities),
                                hr(),
                                uiOutput("tab2Selectable"),
                                uiOutput("tab2Models"),
                                hr(),
                                radioButtons("tab2stepwise", "Stepwise Regression", choices = c("on all datasets" = 1, "on selected datasets" = 2, "no stepwise regression" = 3), selected = 3),
                                hr(),
                                actionButton("tab2do", "Launch")
                              ),
                              # Show Word Cloud
                              mainPanel(
                                uiOutput("tab2_displayResults"),
                                conditionalPanel("output.tab2_displayResults",
                                                 tabsetPanel(type = "tabs",
                                                             tabPanel("Graph",
                                                                      h3("Model results graph"),
                                                                      hr(),
                                                                      h4("Values Comparison"),
                                                                      plotlyOutput("tab2PlotComparison"),
                                                                      h4("Time Series Comparison"),
                                                                      plotlyOutput("tab2PlotTimeSeries"),
                                                                      h4("Period"),
                                                                      textOutput("tab2Period"),
                                                                      hr()
                                                             ),
                                                             tabPanel("Model Information",
                                                                      h3("Regression Statistics"),
                                                                      hr(),
                                                                      h4("Regression Equation"),
                                                                      textOutput("tab2Formula"),
                                                                      br(),
                                                                      h4("Regression Coefficients"),
                                                                      uiOutput("tab2rSquared"),
                                                                      br(),
                                                                      tableOutput("tab2Table"),
                                                                      br(),
                                                                      h4("Analysis of Variance"),
                                                                      tableOutput("tab2ANOVA"),
                                                                      br(),
                                                                      h4("Variance/Covariance Matrix of the model's Parameters"),
                                                                      tableOutput("tab2vcovar"),
                                                                      br(),
                                                                      h4("Download Model Information"),
                                                                      downloadButton("tab2_downloadData", "Download Model Info"),
                                                                      hr()
                                                             ),
                                                             tabPanel("Residuals",
                                                                      h3("Residuals Analysis"),
                                                                      hr(),
                                                                      plotOutput("tab2Plot"),
                                                                      hr()
                                                             )
                                                 )
                                )
                              )
                            )
                   ),
                   tabPanel("Tab3 Analytics",
                            sidebarLayout(
                              # Sidebar with a slider and selection inputs
                              sidebarPanel(
                                h2("Build a Model"),
                                hr(),
                                selectInput("tab3Country", "Choose a Country", choices = countries),
                                hr(),
                                selectInput("tab3Commodity", "Choose a Commodity", choices = commodities),
                                hr(),
                                uiOutput("tab3Selectable"),
                                uiOutput("tab3Models"),
                                hr(),
                                radioButtons("tab3stepwise", "Stepwise Regression", choices = c("on all datasets" = 1, "on selected datasets" = 2, "no stepwise regression" = 3), selected = 3),
                                hr(),
                                actionButton("tab3do", "Launch")
                              ),
                              # Show Word Cloud
                              mainPanel(
                                uiOutput("tab3_displayResults"),
                                conditionalPanel("output.tab3_displayResults",
                                                 tabsetPanel(type = "tabs",
                                                             tabPanel("Graph",
                                                                      h3("Model results graph"),
                                                                      hr(),
                                                                      h4("Values Comparison"),
                                                                      plotlyOutput("tab3PlotComparison"),
                                                                      h4("Time Series Comparison"),
                                                                      plotlyOutput("tab3PlotTimeSeries"),
                                                                      h4("Period"),
                                                                      textOutput("tab3Period"),
                                                                      hr()
                                                             ),
                                                             tabPanel("Model Information",
                                                                      h3("Regression Statistics"),
                                                                      hr(),
                                                                      h4("Regression Equation"),
                                                                      textOutput("tab3Formula"),
                                                                      br(),
                                                                      h4("Regression Coefficients"),
                                                                      uiOutput("tab3rSquared"),
                                                                      br(),
                                                                      tableOutput("tab3Table"),
                                                                      br(),
                                                                      h4("Analysis of Variance"),
                                                                      tableOutput("tab3ANOVA"),
                                                                      br(),
                                                                      h4("Variance/Covariance Matrix of the model's Parameters"),
                                                                      tableOutput("tab3vcovar"),
                                                                      br(),
                                                                      h4("Download Model Information"),
                                                                      downloadButton("tab3_downloadData", "Download Model Info"),
                                                                      hr()
                                                             ),
                                                             tabPanel("Residuals",
                                                                      h3("Residuals Analysis"),
                                                                      hr(),
                                                                      plotOutput("tab3Plot"),
                                                                      hr()
                                                             )
                                                 )
                                )
                              )
                            )
                   ),
                   tabPanel("Forecast data",
                            sidebarLayout(
                              # Sidebar with a slider and selection inputs
                              sidebarPanel(
                                h2("Forecast Parameters"),
                                hr(),
                                h4("Simulation Parameters"),
                                uiOutput("forecast_horizon"),
                                uiOutput("Forecast_Evolution"),
                                uiOutput("extreme_scenarii_building"),
                                hr(),
                                h4("Analyctics Parameters"),
                                uiOutput("inputParameters"),
                                conditionalPanel("output.tab1Parameters || output.tab2Parameters || output.tab3Parameters",
                                                 hr(),
                                                 actionButton("tab4do", "Launch"))
                              ),
                              mainPanel(
                                conditionalPanel("input.tab4do",
                                                 h2("Forecast Results"),
                                                 hr()
                                ),
                                uiOutput("forecast_results")
                              )
                            )
                   )
)

)

