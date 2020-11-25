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
