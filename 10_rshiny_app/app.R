##-----------------------------------------------------------------
# 1. Import data, functions and other variables
## ----------------------------------------------------------------

## Source functions
source("code/00_source_function.R")

# how long are we forecasting for
forecast_length <- 5

ui <- fluidPage(
  ## App title ------
  titlePanel(h1('Mortality Patterns in Somalia: Scenario based Forecasting.', align='center')),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(position = "left",


    # Sidebar panel for inputs ----
    sidebarPanel(
      tabsetPanel(
        tabPanel("Home",
                 fluidRow(
                   wellPanel(
                     strong("Introduction"),
                     p('Welcome to the Predictive Mortality Trend Visualization App! 
                     This app is designed to help you explore and visualize predicted mortality trends based on scenarios. 
                       Follow the steps below to use the App.'),
                     strong("Import Data Tab"),
                     div("Ensure  you have the necessary data files before starting:", style = "color:navy"),
                     p("1.'Upload stratum level' containing geographical level data of the country."),
                     p("2.'Upload Predicting data' containing data with the different predictors per month and geographical level."),
                     p("3. Upload GLMM/GLMM U5 model contaning parameters of the different GLMM model."),
                     div("Upload the Data:", style = "color:navy"),
                     p("Use the 'Browse' button to upload the required data files."),
                     strong("Visualization Tab"),
                     p("Set Visualization Parameters by choosing the following parameters: date of visualization, age groups and geographic level of the plot."),
                     p("Then Click on the 'Go!' button to initiate the visualization process."),
                     p("The first curve displayed corresponds to the forecast mortality trend under default scenarios (median and 97.5% percentile of last 6 months predictor data)."),
                     strong("Scenarios Tab"),
                     p("In the last tab, 'Scenarios,' you can modify the values of different predictors to see their impact on the forecasted mortality trend"),
                     div("For example, if you set the 'Incidence of Malnutrition Admissions' to increase by 0.5, the incident will increase by 0.1 each month, reaching a total increase of 0.5 by the last month of the forecasting period.", style = "color:green")
                   )
                   )),
        tabPanel("Import data", 
            fluidRow(
            wellPanel(
              fileInput("strata", "Upload Geographical level (xlsx) file", accept = ".xlsx") ,
              fileInput("preds_df", "Upload Predicting data (rds) file", accept = ".rds"),
              fileInput("glm_model", "Upload GLMM model (rds) file", accept = ".rds"),
              fileInput("glm_u5_model", "Upload GLMM U5 model (rds) file", accept = ".rds")
            ))),
        tabPanel("Visualization Parameters",
          wellPanel(
            uiOutput('start_date')),
            wellPanel(radioButtons("type_plot", "Age Groups:",
                         c("All Age" = "overall",
                           "Children Under 5y old" = "under5"))
            ),
          wellPanel(
              radioButtons("level_plot", "Geographic Level:",
                                 c("National" = "national",
                                   "District" = "district")),
                    
                    
              uiOutput('list_district'),
              actionButton("goButton", "Go!", class = "btn-success")
              )),
      
        tabPanel("User Scenarios",
      
          wellPanel(
            # Input: Slider for the number of bins ----
            sliderInput(inputId = "perc_measles",
                        label = "Measles Incidence (per 100,000 pop.)",
                        min = -1,
                        max = 1,
                        value = 0,
                        step=0.1),
            textOutput('measles_rate'),
            br(),
            sliderInput(inputId = "perc_malaria",
                        label = "Malaria Incidence (per 100,000 pop.)",
                        min = -1,
                        max = 1,
                        value = 0,
                        step=0.1),
            
            textOutput('malaria_rate'),
            br(),
            sliderInput(inputId = "perc_sam",
                        label = "Incidence of Malnutrition Admissions (per 100,000 pop.):",
                        min = -1,
                        max = 1,
                        value = 0,
                        step=0.1),
            textOutput('sam_rate'),
            br(),
            sliderInput(inputId = "perc_acled",
                        label = "Exposure to Armed Conflict/Insecurity (per 100,000 pop.):",
                        min = -1,
                        max = 1,
                        value = 0,
                        step=0.1),
            
            textOutput('acled_rate'),
            br(),
            sliderInput(inputId = "perc_water",
                        label = "Water Price (Price of 200L Drum of water)",
                        min = -1,
                        max = 1,
                        value = 0,
                        step=0.1),
            
            textOutput('water_rate'),
            br(),
            sliderInput(inputId = "perc_tot_wage",
                        label = "Terms of Trade Purchasing Power Index",
                        min = -1,
                        max = 1,
                        value = 0,
                        step=0.1),
            textOutput('wage_rate'),
            br(),
            sliderInput(inputId = "perc_dep",
                        label = "IDP Departure Rate (per 100,000 pop.)",
                        min = -1,
                        max = 1,
                        value = 0,
                        step=0.1),
            
            textOutput('perc_rate'),
            br(),
            sliderInput(inputId = "perc_prop",
                        label = "Proportion of IDPs",
                        min = -1,
                        max = 1,
                        value = 0,
                        step=0.1),
            textOutput('perc_prop_rate'),
            
            actionButton("goForecast", "Go!", class = "btn-success")
          )),
        tabPanel("About",
                     fluidRow(
                       wellPanel(
                         strong('Authors'),
                         p("Yamna Ouchtar, Chris Grundy, Francesco Checchi"),
                         p("Department of Infectious Disease Epidemiology, Faculty of Epidemiology and Population Health, London School of Hygiene and Tropical Medicine"),
                         strong('Aknowledgments'),
                         p('This App is an output from a project funded by the United Nations Children’s Fund (UNICEF)’s East Africa Regional Office and the World Health Organisation’s Somalia country office, in collaboration with SIMAD University. 
                           However, the views expressed and information contained in it are not necessarily those of or endorsed by these entities, which can accept no responsibility for such views or information or for any reliance placed on them. '),
                         tags$img(src = "LSHTM_Logo.jpg", width = 100),
                         tags$img(src = "SIMAD_Logo.jpg", width = 100), 
                         tags$img(src = 'WHO_Logo.jpg', width = 100), 
                         tags$img(src = 'UNICEF_Logo.jpg', width = 100)
                       )
                     ))
      
      )
      
      ),

  mainPanel(
    fluidRow(
      box(
        title='Forecast Mortality Trend: Default Scenarios.', 
        width=10,
        status = 'primary', 
        solidHeader = TRUE,
        br(), 
        shinycssloaders::withSpinner(plotOutput('glm_forecast'))
      ),
      
      box(
        title='Forecast Mortality Trend: User Specified Scenarios.', 
        width=10,
        shinycssloaders::withSpinner(plotOutput('glm_forecast_corr'))
      )
    )
  )
  
)

)

# Define server logic ----
server <- function(input, output) {
  
  # ADMIN2 and TIME_UNIT
  analysis_strata <- eventReactive(input$strata, {rio::import(input$strata$datapath) })
  time_unit_data <- reactive({f_generate_time_units(
    y_analysis_start = 2010, y_analysis_end = year(end_date_forecast()),
    m_analysis_start = 1, m_analysis_end = month(end_date_forecast())
  )})
  
  
  ## Import data
  preds_df <- eventReactive(input$preds_df, 
                              {rio::import(input$preds_df$datapath) |>
                                  dplyr::filter(date >= "2015-01-01")}
                            )
  
  # Import Model Overall and Under 5
  glm_model <- eventReactive(input$glm_model, {rio::import(input$glm_model$datapath) })
  glm_model_u5 <- eventReactive(input$glm_u5_model, {rio::import(input$glm_u5_model$datapath) })
  
  output$list_district <- renderUI({
    input$strata
    if(input$level_plot == 'national'){
      selectInput('district_level', "If District Level choose the district you want to plot:", 
                  c(character(0)))
      
    }else{
      selectInput('district_level', "If District Level choose the district you want to plot:", 
                  c(sort(analysis_strata()$district)))
      }
  
  
  })
    
    
    
  
  # Forecast Date and Lenght
  DATE_FORECAST <- eventReactive(input$preds_df, {
    max(as.Date(preds_df()$date))})
  
  end_date_forecast <- eventReactive(input$preds_df, {max(as.Date(preds_df()$date)) + months(forecast_length) })
  
  output$start_date <- renderUI({
    input$preds_df
    dateInput2("start_date", "Date to start visualization:", 
             startview = "year", minview = "months", maxview = "decades",
             value = "2021-06-01",
             min = "2015-01-01", 
             max = DATE_FORECAST())})
  
  
  output$measles_rate <- renderText({paste("Last measured Measles Incidence value (on ", 
                                           DATE_FORECAST(), ") is ",
                                           as.character(round(preds_df_aggregate() |> dplyr::filter(date == DATE_FORECAST()) 
                                                              |> dplyr::select(measles_cases_rate), 1)),
                                           '. In general, values are between ',
                                           ' [',as.character(round(
                                             min( preds_df_aggregate() |>
                                                    dplyr::select(measles_cases_rate)), 1)), '; ',
                                           as.character( round(
                                             max(preds_df_aggregate() |>
                                                   dplyr::select(measles_cases_rate)),1)), '].',
                                           ' The value you are setting is ',
                                           as.character(round(preds_df_aggregate() |> dplyr::filter(date == DATE_FORECAST()) 
                                                              |> dplyr::select(measles_cases_rate), 1)*(1+input$perc_measles)),
                                           '.', sep="")})
  
  output$sam_rate <- renderText({paste("Last measured Manultrition Incident value (on ", 
                                       as.character(DATE_FORECAST()), ") is ",
                                       as.character( round( preds_df_mean() |>
                                                  dplyr::filter(date == DATE_FORECAST()) |>
                                                  dplyr::select(sam_admissions_rate_lag1_scn ), 3) ),
                                       '. In general, values are between ',
                                       ' [',as.character(round(min( preds_df_mean() |>
                                                                       dplyr::select(sam_admissions_rate_lag1_scn)), 3)), 
                                       '; ', as.character( round(max(preds_df_mean() |>
                                                                       dplyr::select(sam_admissions_rate_lag1_scn)),3)), ' ].',
                                       ' The value you are setting is ',
                                       as.character(round(preds_df_mean() |> dplyr::filter(date == DATE_FORECAST()) 
                                                          |> dplyr::select(sam_admissions_rate_lag1_scn), 1)*(1+input$perc_sam)),
                                       '.', sep="")})
  
  output$malaria_rate <- renderText({paste("Last measured Malaria Incidence value (on ", as.character(DATE_FORECAST()), ") is ",
                                           as.character(round(preds_df_aggregate() |> dplyr::filter(date == DATE_FORECAST())
                                                              |> dplyr::select(malaria_cases_rate), 1)),
                                           '. In general, values are between ',
                                           ' [',as.character(round(
                                             min( preds_df_aggregate() |>
                                                    dplyr::select(malaria_cases_rate)), 1)), '; ',
                                           as.character( round(
                                             max(preds_df_aggregate() |>
                                                   dplyr::select(malaria_cases_rate)),1)), '].',
                                           ' The value you are setting is ',
                                           as.character(round(preds_df_aggregate() |> dplyr::filter(date == DATE_FORECAST()) 
                                                              |> dplyr::select(malaria_cases_rate), 1)*(1+input$perc_malaria)),
                                           '.', sep="")})
  
  output$acled_rate <- renderText({paste("Last measured Exposure value (on ", as.character(DATE_FORECAST()), " is ",
                                         as.character(round(preds_df_aggregate() |> dplyr::filter(date == DATE_FORECAST())
                                                            |> dplyr::select(acled_event_rate), 1)),
                                         '. In general, values are between ',
                                         ' [',as.character(round(
                                           min( preds_df_aggregate() |>
                                                  dplyr::select(acled_event_rate)), 1)), '; ',
                                         as.character( round(
                                           max(preds_df_aggregate() |>
                                                 dplyr::select(acled_event_rate)),1)), '].',
                                         ' The value you are setting is ',
                                         as.character(round(preds_df_aggregate() |> dplyr::filter(date == DATE_FORECAST()) 
                                                            |> dplyr::select(acled_event_rate), 1)*(1+input$perc_acled)),
                                         '.', sep="")})
  
  output$water_rate <- renderText({paste("Last measured Water Price value (on ", 
                                         as.character(DATE_FORECAST()), ") is ",
                                         as.character( round( preds_df_mean() |>
                                                                dplyr::filter(date == DATE_FORECAST()) |>
                                                                dplyr::select(water_price_smooth_lag2_scn ), 3) ),
                                         '. In general, values are between ',
                                         ' [',as.character(round(min( preds_df_mean() |>
                                                                        dplyr::select(water_price_smooth_lag2_scn)), 3)), 
                                         '; ', as.character( round(max(preds_df_mean() |>
                                                                         dplyr::select(water_price_smooth_lag2_scn)),3)), ' ].',
                                         ' The value you are setting is ',
                                         as.character(round(preds_df_mean() |> dplyr::filter(date == DATE_FORECAST()) 
                                                            |> dplyr::select(water_price_smooth_lag2_scn), 3)*(1+input$perc_water)),
                                         '.', sep="")})
    
  output$wage_rate <- renderText({paste("Last measured Terms of Trade value (on ", 
                                        as.character(DATE_FORECAST()), ") is ",
                                        as.character( round( preds_df_mean() |>
                                                               dplyr::filter(date == DATE_FORECAST()) |>
                                                               dplyr::select(tot_wage_cereal_smooth_lag3_scn ), 3) ),
                                        '. In general, values are between ',
                                        ' [',as.character(round(min( preds_df_mean() |>
                                                                       dplyr::select(tot_wage_cereal_smooth_lag3_scn)), 3)), 
                                        '; ', as.character( round(max(preds_df_mean() |>
                                                                        dplyr::select(tot_wage_cereal_smooth_lag3_scn)),3)), ' ].',
                                        ' The value you are setting is ',
                                        as.character(round(preds_df_mean() |> dplyr::filter(date == DATE_FORECAST()) 
                                                           |> dplyr::select(tot_wage_cereal_smooth_lag3_scn), 3)*(1+input$perc_tot_wage)),
                                        '.', sep="")})
  
  output$perc_rate <- renderText({paste("Last measured Departure Rate value (on ", 
                                        as.character(DATE_FORECAST()), ") is ",
                                        as.character( round( preds_df_mean() |>
                                                               dplyr::filter(date == DATE_FORECAST()) |>
                                                               dplyr::select(dep_rate_sqt_scn ), 3) ),
                                        '. In general, values are between ',
                                        ' [',as.character(round(min(preds_df_mean() |>
                                                                       dplyr::select(dep_rate_sqt_scn)), 3)), 
                                        '; ', as.character( round(max(preds_df_mean() |>
                                                                        dplyr::select(dep_rate_sqt_scn)),3)), ' ].',
                                        ' The value you are setting is ',
                                        as.character(round(preds_df_mean() |> dplyr::filter(date == DATE_FORECAST()) 
                                                           |> dplyr::select(dep_rate_sqt_scn), 3)*(1+input$perc_dep)),
                                        '.', sep="")})
  
  output$perc_prop_rate <- renderText({paste("Last measured Proportion IDP value (on ", 
                                             as.character(DATE_FORECAST()), ") is ",
                                             as.character( round( preds_df_mean() |>
                                                                    dplyr::filter(date == DATE_FORECAST()) |>
                                                                    dplyr::select(prop_idp_scn ), 3) ),
                                             '. In general, values are between ',
                                             ' [',as.character(round(min(preds_df_mean() |>
                                                                           dplyr::select(prop_idp_scn)), 3)), 
                                             '; ', as.character( round(max(preds_df_mean() |>
                                                                             dplyr::select(prop_idp_scn)),3)), ' ].',
                                             ' The value you are setting is ',
                                             as.character(round(preds_df_mean() |> dplyr::filter(date == DATE_FORECAST()) 
                                                                |> dplyr::select(prop_idp_scn), 3)*(1+input$perc_prop)),
                                             '.', sep="")})
  
  ##------------------------------------------------------------------
  # 2. Forecast Data 
  ## -----------------------------------------------------------------
  forecast_data <- reactive({f_forecast_using_simple_model(preds_df(), 
                                                           forecast_length, 
                                                           time_unit_data(), 
                                                           analysis_strata())})
  static_data <- reactive({forecast_data()[[1]]})
  pessimistic_data <- reactive({forecast_data()[[2]]})
  optimistic_data <- reactive({forecast_data()[[3]]})
  
  ##-------------------------------------------------------------------
  # 3. Generate plot when the data are not modified
  ##-------------------------------------------------------------------
  # First for Overall
  static_data_pred <- eventReactive(
    input$goButton,{
    f_pred_forecast(static_data(), glm_model(), name_output='static_scen',
                    level_plot = input$level_plot, district_ = input$district_level)}
    )
  pessimistic_data_pred <- eventReactive(input$goButton,
    {f_pred_forecast(pessimistic_data(), glm_model(), name_output='pessimistic_scen',
                    level_plot = input$level_plot, district_ = input$district_level)
    })
  optimistic_data_pred <- eventReactive(input$goButton,{
    f_pred_forecast(optimistic_data(), glm_model(), name_output='optmistic_scen',
                    level_plot = input$level_plot, district_ = input$district_level)
  })
  
  # Same for u5
  static_data_pred_u5 <- eventReactive(input$goButton,{
    static_data_pred <- NULL
    f_pred_forecast(static_data(), glm_model_u5(), name_output='static_scen',
                    level_plot = input$level_plot, district_ = input$district_level)
    }
  )
  pessimistic_data_pred_u5 <- eventReactive(input$goButton,
    {
      pessimistic_data_pred <- NULL
      f_pred_forecast(pessimistic_data(), glm_model_u5(), name_output='pessimistic_scen',
                    level_plot = input$level_plot, district_ = input$district_level)}
  )
  optimistic_data_pred_u5 <- eventReactive(input$goButton,
    {
      optimistic_data_pred <- NULL
      f_pred_forecast(optimistic_data(), glm_model_u5(), name_output='optmistic_scen',
                    level_plot = input$level_plot, district_ = input$district_level)}
  )
  
  preds_df_aggregate <- eventReactive(input$goButton,
    if(input$level_plot == 'national'){
      aggregate(preds_df()[, c('water_price_smooth_lag2_scn', 'tot_wage_cereal_smooth_lag3_scn', 
                                                             'dep_rate_sqt_scn', 'prop_idp_scn','acled_event_rate', 'cdi_lag5_scn',
                                                             'sam_admissions_rate_lag1_scn', 'measles_cases_rate', 'malaria_cases_rate')], 
                                              by=list(date = preds_df()$date), FUN=sum)
      }
    else{
      pred <- preds_df() |> dplyr::filter(district == input$district_level)
      aggregate(pred[, c('water_price_smooth_lag2_scn', 'tot_wage_cereal_smooth_lag3_scn', 
                       'dep_rate_sqt_scn', 'prop_idp_scn','acled_event_rate', 'cdi_lag5_scn',
                       'sam_admissions_rate_lag1_scn', 'measles_cases_rate', 'malaria_cases_rate')], 
              by=list(date = pred$date), FUN=sum)}
    )
    
    preds_df_mean <- eventReactive(input$goButton,
      if(input$level_plot == 'national'){
        aggregate(preds_df()[, c('water_price_smooth_lag2_scn', 'tot_wage_cereal_smooth_lag3_scn', 
                                                        'dep_rate_sqt_scn', 'prop_idp_scn','acled_event_rate', 'cdi_lag5_scn',
                                                        'sam_admissions_rate_lag1_scn', 'measles_cases_rate', 'malaria_cases_rate')], 
                                         by=list(date = preds_df()$date), FUN=mean)}
      else{
        pred <- preds_df() |> dplyr::filter(district == input$district_level)
        aggregate(pred[, c('water_price_smooth_lag2_scn', 'tot_wage_cereal_smooth_lag3_scn', 
                           'dep_rate_sqt_scn', 'prop_idp_scn','acled_event_rate', 'cdi_lag5_scn',
                           'sam_admissions_rate_lag1_scn', 'measles_cases_rate', 'malaria_cases_rate')], 
                  by=list(date = pred$date), FUN=mean)
      }
      )
  
  ##---------------------------------------------------------------------
  # 6.Correlation Calculation
  ##---------------------------------------------------------------------
  
  MODIFIED_FEATURES <- eventReactive(input$goForecast, {c('measles_cases_rate', 'sam_admissions_rate_lag1_scn', 
                         'malaria_cases_rate', 'acled_event_rate', 'water_price_smooth_lag2_scn',
                         'tot_wage_cereal_smooth_lag3_scn', 'dep_rate_sqt_scn', 'prop_idp_scn')})
  # browser()
  PERCENT_MODIFICATIONS <- eventReactive(input$goForecast, {c(input$perc_measles, input$perc_sam, input$perc_malaria, input$perc_acled, 
                                       input$perc_water, input$perc_tot_wage, input$perc_dep, input$perc_prop)})
  
  
  res_corr <- eventReactive(c(input$goButton, input$goForecast), {f_return_data_corr_pred(MODIFIED_FEATURES(), PERCENT_MODIFICATIONS(), 
                                                static_data(), pessimistic_data(), optimistic_data(), 
                                                DATE_FORECAST(), preds_df(), glm_model(), glm_model_u5(),
                                                level_plot = input$level_plot, district_ = input$district_level)})
  
  ##---------------------------------------------------------------------
  # 6. Plots
  ##---------------------------------------------------------------------
  
  output$glm_forecast <- renderPlot({
    input$goButton
    f_plot_forecasting(static_data_pred(), pessimistic_data_pred(), optimistic_data_pred(),
                       static_data_pred_u5(), pessimistic_data_pred_u5(), optimistic_data_pred_u5(),
                       DATE_FORECAST(),
                       start_period=input$start_date, type_plot = input$type_plot)
  })
  
  
  output$glm_forecast_corr <- renderPlot({
    input$goButton
    input$goForecast
    f_plot_forecasting(stat_data=res_corr()[[1]], pess_data=res_corr()[[2]], opt_data=res_corr()[[3]], 
                       stat_data_u5=res_corr()[[4]], pess_data_u5=res_corr()[[5]], opt_data_u5=res_corr()[[6]],
                       DATE_FORECAST(),
                       start_period=input$start_date, type_plot = input$type_plot)
  })

}

# Run the app ----
shinyApp(ui = ui, server = server)

