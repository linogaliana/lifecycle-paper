# Define server logic required to generate and plot a random distribution

server <- function(input, output) {
  
  # Expression that generates a plot of the distribution. The expression
  # is wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically 
  #     re-executed when inputs change
  #  2) Its output type is a plot 
  #
  population2 <- data.table::copy(population)
  
  # bys <- reactive(
  #   if (input$momentsby == ''){
  #     input$momentsby
  #   } else{
  #     NULL
  #   }
  # )
  bys <- reactive(
    if (!is.null(input$momentsby) && (nchar(input$momentsby)>1)){
      input$momentsby
    } else{
      NULL
    }
  )
  
  data_prediction_augm2 <- reactive(
    capitulation::life_cycle_model(
      population2,
      wealthvar_survey = "K_observed",
      r = input$r/100,
      beta = input$beta,
      gamma = input$gamma,
      observation_year = 2009,
      income_var = "revenu",
      Hgiven_var = "hg",
      Hreceived_var = "hr",
      return_last = FALSE,
      get_capital_income = TRUE, additional_vars = c("tr_age_2015", input$momentsby))
  )
  
  p_K <- reactive(
    capitulation::plot_K_age(data_prediction_augm2())
  )
  
  p_rK <- reactive(
    capitulation::plot_rK_age(data_prediction_augm2())
  )
  
  p_gini <- reactive(
    capitulation::plot_gini(data_prediction_augm2(),
                            vars = c("revenu", "wealth", "Y"))
  )
  
  p_lorenz <- reactive(
    capitulation:::plot_lorenz(data_prediction_augm2(),
                               year = input$year_lorenz)
  )
  
  p_share <- reactive(
    capitulation:::plot_top_share(data_prediction_augm2(),
                              threshold = (1 - input$topx/100))
  ) 
  
  
  p_moment1 <- reactive(
    if (is.null(bys())){
      wealthyR:::plot_moment_age_facet(EP_2015, EP_2018, simulations = data_prediction_augm2(),
                                       by_survey = "AGE", by_simulation = "age",
                                       scale = "log",
                                       facets_vars = bys())[[1]]
    } else{
      wealthyR:::plot_moment_age_facet(EP_2015, EP_2018, simulations = data_prediction_augm2(),
                                       by_survey = "AGE", by_simulation = "age",
                                       scale = "log",
                                       facets_vars = bys())      
    }
  )
  
  
  p_moment1b <- reactive(
    wealthyR::plot_moment_pic(EP_2015 = EP_2015,
                              EP_2018 = EP_2018,
                              simulations = data_prediction_augm2(),
                              scale = "log")
  )
  
  
  p_moment2 <- reactive(
    wealthyR:::plot_moment_dK_facet(
      EP_lon = EP_lon, simulations = data_prediction_augm2(),
      scale = "log", xaxis = "tr_age_2015", facets_vars = bys())
  )
  
  
  output$Kplot <- renderPlot({
    p_K()
  })
  output$rKplot <- renderPlot({
    p_rK()
  })
  output$distPlot <- renderPlot({
    p_gini()
  })
  output$lorenzPlot <- renderPlot({
    p_lorenz()
  })
  output$moment1 <- renderPlot({
    p_moment1()
  })
  output$moment1b <- renderPlot({
    p_moment1b()
  })
  output$moment2 <- renderPlot({
    p_moment2()
  })
  output$sharePlot <- renderPlot({
    p_share()
  })  
  
  
}