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



theta <- c(beta = '0.975', gamma = '0.6')


betas <- seq(0.9, 1, by = 0.005)
gammas <- seq(0.4,0.6, by = 0.02)

grid <- data.frame(
  expand.grid(beta = betas[1:2], gamma = gammas[1:2])
)

grid <- split(grid, 1:nrow(grid))

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


results <- do.call(rbind, plot_theta)

library(plotly)

fig <- plot_ly(z = ~as.matrix(results)) %>% add_surface(
  contours = list(
    z = list(
      show=TRUE,
      usecolormap=TRUE,
      highlightcolor="#ff0000",
      project=list(z=TRUE)
    )
  )
)
# fig <- fig %>% layout(
#   scene = list(
#     camera=list(
#       eye = list(x=0.905, y=0.41, z=0.01)
#     )
#   )
# )

fig
