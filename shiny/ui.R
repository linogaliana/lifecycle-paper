# Define UI for application that plots random distributions 

ui = fluidPage(
  
  titlePanel("Aide au diagnostic pour le choix des paramètres"),
  
  # Application title
  headerPanel("Modèle d'accumulation du patrimoine"),
  
  
  sidebarLayout(  
    
    sidebarPanel(
      numericInput("r", label = "Taux d'intérêt r (%)", value = 3),
      numericInput("gamma", label = "\u03B3", value = 0.8189066),
      numericInput("beta", label = "\u03B2", value = 0.975661)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Moments",
                           selectInput("momentsby",
                                       "By :",
                                       choices = c("",colnames(population)),
                                       selected = "",
                                       multiple = TRUE
                           ) ,
                           plotOutput("moment1"),
                           plotOutput("moment1b"),
                           plotOutput("moment2")),
                  tabPanel("Patrimoine par âge", plotOutput("Kplot")),
                  tabPanel("Revenu patrimoine par âge", plotOutput("rKplot")),
                  tabPanel("Indice de Gini", 
                           numericInput("topx",
                                        label = "Part richesse, revenu, etc. du top x% ",
                                        value = 10),
                           plotOutput("sharePlot"),
                           plotOutput("distPlot")),
                  tabPanel("Courbe de Lorenz",
                           numericInput("year_lorenz", label = h3("Année"), value = 2020),
                           plotOutput("lorenzPlot"))
      )      
    )
  )
  
)

