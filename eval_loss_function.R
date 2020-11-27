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


results <- lapply(seq(0.9, 0.92, by = 0.01), function(b){
  epseps <- report_epsilon(
    # theta = c(r = r,
    #           beta = beta,
    #           gamma = gamma),
    # model_function = wealthyR:::model_capitulation,
    r = r,
    beta = b,
    gamma = gamma,
    population = population,
    EP_2015 = EP_2015,
    EP_2018 = EP_2018,
    EP_lon = EP_lon,
    verbose = TRUE)
  return(
    data.frame('loss' = epseps,
               'beta' = b,
               'gamma' = gamma,
               'r' = r
    )
  )
})
results <- do.call(rbind, results)
