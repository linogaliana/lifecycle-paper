set.seed(12345)

library(data.table)

data <- readRDS("./data.rds")
EP_2015 <- data[['EP_2015']]
EP_2018 <- data[['EP_2018']]
EP_lon <- data[['EP_lon']]

menages_structural2 <- readRDS("./tempfile.rds")


number_moments <- 2L
scale_wealth <- "log"
select_moments <- NULL
estimation_method <- "two_step"
parameters_estimation <- list("number_moments" = number_moments,
                              "scale_wealth" = scale_wealth,
                              "select_moments" = select_moments,
                              "method" = estimation_method)

beta <- 0.98
r <- 0.03
gamma <- 2
gamma_0 <- 1.5
beta_0 <- 0.9
outfile = "out_r2.txt"





menages_structural2[,'AGE' := age]


simulations_free <- capitulation::life_cycle_model(data = menages_structural2,
                                                   r = 0.03,
                                                   gamma = gamma,
                                                   beta = beta,
                                                   scale_model = "level",
                                                   income_var = "y_indiv",
                                                   weight_var = NULL,
                                                   wealthvar_survey = "K_observed",
                                                   Hgiven_var = "hg",
                                                   Hreceived_var = "hr",
                                                   additional_vars = c("AGE", "tr_age_2015"))
simulations_2018_free <- simulations_free 


simulations_moment_free <- wealthyR:::create_moment(EP_2015 = simulations_free,
                                                    EP_2018 = simulations_2018_free,
                                                    by = c("AGE", "tr_age_2015"),
                                                    wealth_var = 'wealth',
                                                    age_var = "age",
                                                    N_moments = 180,
                                                    moment_var = "moment_data",
                                                    scale_variable_moment1 = "asinh",
                                                    scale_variable_moment2 = "log",
                                                    scale_moment1 = "level",
                                                    moment1 = "level",
                                                    stat_moment2 = "difference",
                                                    ages = c(30,65),
                                                    stats = c("mean","sd")
)


simulations_fixed <- capitulation::life_cycle_model(data = menages_structural2,
                                                    r = 0.03,
                                                    gamma = gamma,
                                                    beta = beta,
                                                    scale_model = "level",
                                                    income_var = "y_indiv",
                                                    weight_var = NULL,
                                                    wealthvar_survey = "K_observed",
                                                    Hgiven_var = "hg",
                                                    Hreceived_var = "hr",
                                                    additional_vars = c("AGE", "tr_age_2015"))
simulations_2018_fixed <- simulations_fixed 


simulations_moment_fixed <- wealthyR:::create_moment(EP_2015 = simulations_fixed,
                                                     EP_2018 = simulations_2018_fixed,
                                                     by = c("AGE", "tr_age_2015"),
                                                     wealth_var = 'wealth',
                                                     age_var = "age",
                                                     N_moments = 180,
                                                     moment_var = "moment_data",
                                                     scale_variable_moment1 = "asinh",
                                                     scale_variable_moment2 = "log",
                                                     scale_moment1 = "level",
                                                     moment1 = "level",
                                                     stat_moment2 = "difference",
                                                     ages = c(30,65),
                                                     stats = c("mean","sd")
)

simulations_fixed2 <- capitulation::life_cycle_model(data = menages_structural2,
                                                     r = 0.03,
                                                     gamma = gamma,
                                                     beta = beta,
                                                     scale_model = "level",
                                                     income_var = "y_indiv",
                                                     weight_var = NULL,
                                                     wealthvar_survey = "K_observed",
                                                     Hgiven_var = "hg",
                                                     Hreceived_var = "hr",
                                                     additional_vars = c("AGE", "tr_age_2015"))
simulations_2018_fixed2 <- simulations_fixed2 


simulations_moment_fixed2 <- wealthyR:::create_moment(EP_2015 = simulations_fixed2,
                                                      EP_2018 = simulations_2018_fixed2,
                                                      by = c("AGE", "tr_age_2015"),
                                                      wealth_var = 'wealth',
                                                      age_var = "age",
                                                      N_moments = 180,
                                                      moment_var = "moment_data",
                                                      scale_variable_moment1 = "asinh",
                                                      scale_variable_moment2 = "log",
                                                      scale_moment1 = "level",
                                                      moment1 = "level",
                                                      stat_moment2 = "difference",
                                                      ages = c(30,65),
                                                      stats = c("mean","sd")
)

# 4 METHODES AVEC BETA(1+R)=1 ---------------------------

## BENCHMARK ===================

print("Model benchmark")

output_1 <- mindist::estimation_theta(
  theta_0 = c("beta" = {if(!is.null(beta)) beta_0 else NULL},
              "gamma" = 4,
              "r" = {if(is.null(r)) 0.03 else NULL}
  ),
  # beta = beta,
  r = 0.03,
  # gamma = gamma,
  approach = "one_step",
  compute_standard_errors = FALSE,
  prediction_function = wealthyR:::model_capitulation,
  select_moments = select_moments,
  EP_2015 = EP_2015,
  EP_lon = EP_lon,
  EP_2018 = EP_2018,
  data_microsimulated = menages_structural2,
  observed_moment_data = simulations_moment_fixed,
  N_moments = 180,
  wealth_var = "PATRI_NET",
  by = c("AGE", "tr_age_2015"),
  scale_model = "level",
  scale_variable_moment1 = "asinh",
  scale_variable_moment2 = "log",
  stat_moment2 = 'difference',
  moment1 = "level",
  moments_weights = "weight",
  verbose = TRUE,
  Hgiven_var = "hg",
  Hreceived_var = "hr",
  method = "Nelder-Mead"
)

output_2 <- mindist::estimation_theta(
  theta_0 = c("beta" = 0.9,
              "gamma" = {if(!is.null(gamma)) gamma_0 else NULL},
              "r" = {if(is.null(r)) 0.03 else NULL}
  ),
  # beta = beta,
  r = 0.03,
  # gamma = gamma,
  approach = "one_step",
  compute_standard_errors = FALSE,
  prediction_function = wealthyR:::model_capitulation,
  select_moments = select_moments,
  EP_2015 = EP_2015,
  EP_lon = EP_lon,
  EP_2018 = EP_2018,
  data_microsimulated = menages_structural2,
  observed_moment_data = simulations_moment_fixed,
  N_moments = 180,
  wealth_var = "PATRI_NET",
  by = c("AGE", "tr_age_2015"),
  scale_model = "level",
  scale_variable_moment1 = "asinh",
  scale_variable_moment2 = "log",
  stat_moment2 = 'difference',
  moment1 = "level",
  moments_weights = "weight",
  verbose = TRUE,
  Hgiven_var = "hg",
  Hreceived_var = "hr",
  method = "Nelder-Mead"
)

output_3 <- mindist::estimation_theta(
  theta_0 = c("beta" = 0.7,
              "gamma" = 3.5,
              "r" = {if(is.null(r)) 0.03 else NULL}
  ),
  # beta = beta,
  r = 0.03,
  # gamma = gamma,
  approach = "one_step",
  compute_standard_errors = FALSE,
  prediction_function = wealthyR:::model_capitulation,
  select_moments = select_moments,
  EP_2015 = EP_2015,
  EP_lon = EP_lon,
  EP_2018 = EP_2018,
  data_microsimulated = menages_structural2,
  observed_moment_data = simulations_moment_fixed,
  N_moments = 180,
  wealth_var = "PATRI_NET",
  by = c("AGE", "tr_age_2015"),
  scale_model = "level",
  scale_variable_moment1 = "asinh",
  scale_variable_moment2 = "log",
  stat_moment2 = 'difference',
  moment1 = "level",
  moments_weights = "weight",
  verbose = TRUE,
  Hgiven_var = "hg",
  Hreceived_var = "hr",
  method = "Nelder-Mead"
)


output_4 <- mindist::estimation_theta(
  theta_0 = c("beta" = 1.3,
              "gamma" = 3.5,
              "r" = {if(is.null(r)) 0.03 else NULL}
  ),
  # beta = beta,
  r = 0.03,
  # gamma = gamma,
  approach = "one_step",
  compute_standard_errors = FALSE,
  prediction_function = wealthyR:::model_capitulation,
  select_moments = select_moments,
  EP_2015 = EP_2015,
  EP_lon = EP_lon,
  EP_2018 = EP_2018,
  data_microsimulated = menages_structural2,
  observed_moment_data = simulations_moment_fixed,
  N_moments = 180,
  wealth_var = "PATRI_NET",
  by = c("AGE", "tr_age_2015"),
  scale_model = "level",
  scale_variable_moment1 = "asinh",
  scale_variable_moment2 = "log",
  stat_moment2 = 'difference',
  moment1 = "level",
  moments_weights = "weight",
  verbose = TRUE,
  Hgiven_var = "hg",
  Hreceived_var = "hr",
  method = "Nelder-Mead"
)


output_5 <- mindist::estimation_theta(
  theta_0 = c("beta" = 0.98,
              "gamma" = 3.5,
              "r" = {if(is.null(r)) 0.03 else NULL}
  ),
  # beta = beta,
  r = 0.03,
  # gamma = gamma,
  approach = "one_step",
  compute_standard_errors = FALSE,
  prediction_function = wealthyR:::model_capitulation,
  select_moments = select_moments,
  EP_2015 = EP_2015,
  EP_lon = EP_lon,
  EP_2018 = EP_2018,
  data_microsimulated = menages_structural2,
  observed_moment_data = simulations_moment_fixed,
  N_moments = 180,
  wealth_var = "PATRI_NET",
  by = c("AGE", "tr_age_2015"),
  scale_model = "level",
  scale_variable_moment1 = "asinh",
  scale_variable_moment2 = "log",
  stat_moment2 = 'difference',
  moment1 = "level",
  moments_weights = "weight",
  verbose = TRUE,
  Hgiven_var = "hg",
  Hreceived_var = "hr",
  method = "Nelder-Mead"
)

setNames(
  data.table::rbindlist(
    list(data.frame(t(cbind(c(beta, gamma, beta_0, 4, output_1$estimates$par)))),
         data.frame(t(cbind(c(beta, gamma, 0.9, gamma_0, output_2$estimates$par)))),
         data.frame(t(cbind(c(beta, gamma, 0.7, 3.5, output_3$estimates$par)))),
         data.frame(t(cbind(c(beta, gamma, 1.3, 3.5, output_4$estimates$par)))),
         data.frame(t(cbind(c(beta, gamma, 0.98, 3.5, output_5$estimates$par))))
         
    )
  ), c("beta_true","gamma_true","beta_0", "gamma_0", "beta_hat", "gamma_hat")
)
