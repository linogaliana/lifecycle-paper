library(data.table)
library(capitulation)
library(ggplot2)

source("import_functions.R")

r <- 0.03
beta <- 0.95
gamma <- 0.5

epseps <- report_epsilon(
  # theta = c(r = r,
  #           beta = beta,
  #           gamma = gamma),
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
  expand.grid(beta = betas[1:5], gamma = gammas[1:5])
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

