library(data.table)
library(capitulation)
library(ggplot2)

source("import_functions.R")

r <- 0.03
beta <- 0.975
gamma <- 0.8

epseps <- report_epsilon(
  # theta = c(r = r,
  #           beta = beta,
  #           gamma = gamma),
  # model_function = wealthyR:::model_capitulation,
  r = r,
  beta = beta,
  gamma = gamma,
  population = population,
  EP_2015 = EP_2015,
  EP_2018 = EP_2018,
  EP_lon = EP_lon,
  verbose = TRUE)
epseps






#### brouillon ------------

theta <- c(beta = '0.975', gamma = '0.6')


betas <- seq(0.9, 1, by = 0.005)
gammas <- seq(3, 6, by = 0.1)

grid <- data.frame(
  expand.grid(beta = betas, gamma = gammas)
)

grid <- split(grid, 1:nrow(grid))
#grid <- grid[201:length(grid)]


system.time(
plot_theta <- lapply(grid, function(theta){
  epseps <- report_epsilon(
    # theta = c(r = r,
    #           beta = beta,
    #           gamma = gamma),
    r = r,
    beta = theta$beta,
    gamma = theta$gamma,
    # model_function = wealthyR:::model_capitulation,
    population = population,
    EP_2015 = EP_2015,
    EP_2018 = EP_2018,
    EP_lon = EP_lon,
    verbose = FALSE)
  return(
    data.frame(beta = theta$beta,
               gamma = theta$gamma,
               loss = epseps)
  )
})
)

results <- data.table::rbindlist(plot_theta)


#results2 <- data.table::copy(results)
results2 <- data.table::rbindlist(list(results, results2), fill = TRUE, use.names = TRUE)
results3 <- unique(results2)
library(plotly)

results4 <- data.table::copy(results3)
results4[,'labels' := paste0('beta: ', get("beta"),
                             '<br>',
                             'gamma: ',  get("gamma"),
                             '<br>',
                             'loss: ',  get("loss")
                             )] 


fig <- plot_ly(
  data = results4,
  x = ~beta, 
  y = ~gamma, 
  z = ~loss,
  intensity=~loss,
  text=~labels,
  type = "mesh3d",
  colorscale = "Hot",
  hoverinfo = 'text'
)

fig
