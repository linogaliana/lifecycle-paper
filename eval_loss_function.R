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
gammas <- seq(0.4,0.6, by = 0.02)

grid <- data.frame(
  expand.grid(beta = betas, gamma = gammas)
)

grid <- split(grid, 1:nrow(grid))
grid <- grid[201:length(grid)]


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

contingency_table <- xtabs(loss ~ beta + gamma, data = results3)



plot_ly(
  x = as.numeric(rownames(contingency_table)), 
  y = as.numeric(colnames(contingency_table)), 
  z = contingency_table,
  text = labels
  ) %>% 
  add_surface(colorscale='Hot') %>%
  layout(
    title = "",
    scene = list(
      xaxis = list(title = "beta"),
      yaxis = list(title = "gamma"),
      zaxis = list(title = "loss"),
      camera = list(eye = list(x = 1.95, y = -1.25, z = 1.25))
    ))  

# fig <- fig %>% layout(
#   scene = list(
#     camera=list(
#       eye = list(x=0.905, y=0.41, z=0.01)
#     )
#   )
# )

fig
