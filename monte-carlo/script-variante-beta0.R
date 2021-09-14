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
beta_0 <- 1.2
outfile = "out_gamma0917.txt"


menages_structural2[,'AGE' := age]


simulations_free <- capitulation::life_cycle_model(data = menages_structural2,
                                                   r = r,
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
                                                    by = c("tr_age_2015", "tr_age_2015"),
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

simulations_moment_fixed <- simulations_moment_free
simulations_2018_fixed <- simulations_2018_free
simulations_fixed <- simulations_free


# simulations_fixed <- capitulation::life_cycle_model(data = menages_structural2,
#                                                     r = NULL,
#                                                     gamma = gamma,
#                                                     beta = beta,
#                                                     scale_model = "level",
#                                                     income_var = "y_indiv",
#                                                     weight_var = NULL,
#                                                     wealthvar_survey = "K_observed",
#                                                     Hgiven_var = "hg",
#                                                     Hreceived_var = "hr",
#                                                     additional_vars = c("AGE", "tr_age_2015"))
# simulations_2018_fixed <- simulations_fixed 


# simulations_moment_fixed <- wealthyR:::create_moment(EP_2015 = simulations_fixed,
#                                                      EP_2018 = simulations_2018_fixed,
#                                                      by = c("tr_age_2015", "tr_age_2015"),
#                                                      wealth_var = 'wealth',
#                                                      age_var = "age",
#                                                      N_moments = 180,
#                                                      moment_var = "moment_data",
#                                                      scale_variable_moment1 = "asinh",
#                                                      scale_variable_moment2 = "log",
#                                                      scale_moment1 = "level",
#                                                      moment1 = "level",
#                                                      stat_moment2 = "difference",
#                                                      ages = c(30,65),
#                                                      stats = c("mean","sd")
# )






print("Model benchmark")

output_1 <- mindist::estimation_theta(
  theta_0 = c("beta" = {if(!is.null(beta)) beta_0 else NULL},
              # "gamma" = {if(is.null(gamma)) gamma_0 else NULL},
              "r" = {if(is.null(r)) 0.03 else NULL}
  ),
  # beta = beta,
  r = r,
  gamma = gamma,
  approach = "one_step",
  compute_standard_errors = FALSE,
  prediction_function = wealthyR:::model_capitulation,
  select_moments = select_moments,
  EP_2015 = EP_2015,
  EP_lon = EP_lon,
  EP_2018 = EP_2018,
  data_microsimulated = menages_structural2,
  observed_moment_data = simulations_moment_free,
  N_moments = 180,
  wealth_var = "PATRI_NET",
  by = c("tr_age_2015", "tr_age_2015"),
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


simulations_fixed2 <- capitulation::life_cycle_model(data = menages_structural2,
                                                     r = r,
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


simulations_moment_fixed2 <- wealthyR:::create_moment(EP_2015 = simulations_fixed,
                                                      EP_2018 = simulations_2018_fixed,
                                                      by = c("tr_age_2015", "tr_age_2015"),
                                                      wealth_var = 'wealth',
                                                      age_var = "age",
                                                      N_moments = 180,
                                                      moment_var = "moment_data",
                                                      scale_variable_moment1 = "asinh",
                                                      scale_variable_moment2 = "log",
                                                      scale_moment1 = "level",
                                                      moment1 = "level",
                                                      stat_moment2 = "difference",
                                                      ages = c(40,60),
                                                      stats = c("mean","sd")
)

output_1b <- mindist::estimation_theta(
  theta_0 = c("beta" = {if(!is.null(beta)) beta_0 else NULL},
              # "gamma" = {if(is.null(gamma)) gamma_0 else NULL},
              "r" = {if(is.null(r)) 0.08 else NULL}
  ),
  beta = beta,
  r = r,
  gamma = gamma,
  approach = "one_step",
  ages = c(40,60),
  compute_standard_errors = FALSE,
  prediction_function = wealthyR:::model_capitulation,
  select_moments = select_moments,
  EP_2015 = EP_2015,
  EP_lon = EP_lon,
  EP_2018 = EP_2018,
  data_microsimulated = menages_structural2,
  observed_moment_data = simulations_moment_fixed2,
  N_moments = 180,
  wealth_var = "PATRI_NET",
  by = c("tr_age_2015", "tr_age_2015"),
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



# FIRST MOMENT ONLY =======================



output_1d <- mindist::estimation_theta(
  theta_0 = c("beta" = {if(!is.null(beta)) beta_0 else NULL},
              # "gamma" = {if(is.null(gamma)) gamma_0 else NULL},
              "r" = {if(is.null(r)) 0.08 else NULL}
  ),
  beta = beta,
  r = r,
  gamma = gamma,
  approach = "one_step",
  compute_standard_errors = FALSE,
  prediction_function = wealthyR:::model_capitulation,
  select_moments = select_moments,
  EP_2015 = EP_2015,
  EP_lon = EP_lon,
  EP_2018 = EP_2018,
  data_microsimulated = menages_structural2,
  observed_moment_data = simulations_moment_fixed,
  N_moments = 13,
  wealth_var = "PATRI_NET",
  by = c("tr_age_2015", "tr_age_2015"),
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


# SECOND MOMENT ONLY =======================


output_1e <- mindist::estimation_theta(
  theta_0 = c("beta" = {if(!is.null(beta)) beta_0 else NULL},
              # "gamma" = {if(is.null(gamma)) gamma_0 else NULL},
              "r" = {if(is.null(r)) 0.08 else NULL}
  ),
  beta = beta,
  r = r,
  gamma = gamma,
  approach = "one_step",
  compute_standard_errors = FALSE,
  prediction_function = wealthyR:::model_capitulation,
  EP_2015 = EP_2015,
  EP_lon = EP_lon,
  EP_2018 = EP_2018,
  data_microsimulated = menages_structural2,
  observed_moment_data = simulations_moment_fixed,
  N_moments = 180,
  select_moments = 14:21,
  wealth_var = "PATRI_NET",
  by = c("tr_age_2015", "tr_age_2015"),
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



# SECOND MOMENT ONLY (40-60 ans) =======================


output_1f <- mindist::estimation_theta(
  theta_0 = c("beta" = {if(!is.null(beta)) beta_0 else NULL},
              # "gamma" = {if(is.null(gamma)) gamma_0 else NULL},
              "r" = {if(is.null(r)) 0.08 else NULL}
  ),
  beta = beta,
  r = r,
  gamma = gamma,
  approach = "one_step",
  compute_standard_errors = FALSE,
  prediction_function = wealthyR:::model_capitulation,
  EP_2015 = EP_2015,
  EP_lon = EP_lon,
  EP_2018 = EP_2018,
  data_microsimulated = menages_structural2,
  observed_moment_data = simulations_moment_fixed,
  ages = c(40,60),
  N_moments = 180,
  select_moments = 14:21,
  wealth_var = "PATRI_NET",
  by = c("tr_age_2015", "tr_age_2015"),
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

# FIRST MOMENT ONLY (40-60 ans) =======================

simulations_fixed2 <- capitulation::life_cycle_model(data = menages_structural2,
                                                     r = r,
                                                     gamma = gamma,
                                                     beta = beta,
                                                     scale_model = "level",
                                                     income_var = "y_indiv",
                                                     weight_var = NULL,
                                                     wealthvar_survey = "K_observed",
                                                     Hgiven_var = "hg",
                                                     Hreceived_var = "hr",
                                                     additional_vars = c("tr_age_2015", "tr_age_2015"))
simulations_2018_fixed2 <- simulations_fixed2

simulations_moment_fixed5 <- wealthyR:::create_moment(EP_2015 = simulations_fixed2,
                                                      EP_2018 = simulations_2018_fixed2,
                                                      by = c("AGE", "tr_age_2015"),
                                                      wealth_var = 'wealth',
                                                      age_var = "age",
                                                      N_moments = 180,
                                                      ages_cross_section = c(40, 60),
                                                      moment_var = "moment_data",
                                                      scale_variable_moment1 = "level",
                                                      scale_variable_moment2 = "level",
                                                      scale_moment1 = "level",
                                                      moment1 = "level",
                                                      stat_moment2 = "growth",
                                                      ages = c(40,60),
                                                      stats = c("mean","sd")
)


output_1g <- mindist::estimation_theta(
  theta_0 = c("beta" = {if(!is.null(beta)) beta_0 else NULL},
              # "gamma" = {if(is.null(gamma)) gamma_0 else NULL},
              "r" = {if(is.null(r)) 0.08 else NULL}
  ),
  beta = beta,
  r = r,
  gamma = gamma,
  approach = "one_step",
  compute_standard_errors = FALSE,
  prediction_function = wealthyR:::model_capitulation,
  ages_cross_section = c(40,60),
  EP_2015 = EP_2015,
  EP_lon = EP_lon,
  EP_2018 = EP_2018,
  data_microsimulated = menages_structural2,
  observed_moment_data = simulations_moment_fixed,
  ages = c(40,60),
  N_moments = 180,
  select_moments = 1:13,
  wealth_var = "PATRI_NET",
  by = c("tr_age_2015", "tr_age_2015"),
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


print_out <- function(output, cible){
  print("-------------- Estimated beta -----------------\n")
  print(output$estimates$par)
  print("---------------- Nelder Mean output ---------\n")
  print(output$estimates)
  print("---------- Moments ----------------\n")
  print(output$moments)
  print("\n\n")
}


x = capture.output({
  print(toupper("============== Modèle de référence =============="))
  print_out(output_1)
  print(toupper("============== Moment 2 entre 40 et 60 ans =============="))
  print_out(output_1b)
  # print(toupper("============== Moment 2 entre 40 et 60 ans & K/Y =============="))
  # print_out(output_1c)
  print(toupper("============== Modèle de référence (uniquement moment 1) =============="))
  print_out(output_1d)
  print(toupper("============== Modèle de référence (uniquement moment 1 sur 40-60 ans) =============="))
  print_out(output_1g)
  # print(toupper("============== Modèle de référence (uniquement moment 1 sur 40-60 ans & K/Y) =============="))
  # print_out(output_1h)
  print(toupper("============== Modèle de référence (uniquement moment 2) =============="))
  print_out(output_1e)
  print(toupper("============== Modèle de référence (uniquement moment 2 sur 40-60 ans) =============="))
  print_out(output_1f)
  # print(toupper("============== Modèle de référence (uniquement moment 2 sur 40-60 ans & K/Y) =============="))
  # print_out(output_1j)
})



outfile <- "gamma_fixed.txt"

cat(x, file = here::here("monte-carlo", outfile), sep = "\n", useBytes=TRUE)





env <- list(
  output_1,
  output_1b,
  # output_1c,
  output_1d,
  output_1g,
  # output_1h,
  output_1e,
  output_1f#,
  # output_1j
)

saveRDS(env, file = here::here("monte-carlo", "gamma_fixed.Rds"))



#------------------------------------------------------
# OLD ----------------------------------------------------------




# 4 METHODES AVEC BETA(1+R)=1 ---------------------------

## BENCHMARK ===================

print("Model benchmark")

output_1 <- mindist::estimation_theta(
  theta_0 = c("beta" = {if(!is.null(beta)) beta_0 else NULL},
              # "gamma" = {if(is.null(gamma)) gamma_0 else NULL},
              "r" = {if(is.null(r)) 0.08 else NULL}
  ),
  # beta = beta,
  r = NULL,
  gamma = gamma,
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
  by = c("tr_age_2015", "tr_age_2015"),
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

## CHANGE AGE ==========================

simulations_fixed2 <- capitulation::life_cycle_model(data = menages_structural2,
                                                     r = NULL,
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


simulations_moment_fixed2 <- wealthyR:::create_moment(EP_2015 = simulations_fixed,
                                                      EP_2018 = simulations_2018_fixed,
                                                      by = c("tr_age_2015", "tr_age_2015"),
                                                      wealth_var = 'wealth',
                                                      age_var = "age",
                                                      N_moments = 180,
                                                      moment_var = "moment_data",
                                                      scale_variable_moment1 = "asinh",
                                                      scale_variable_moment2 = "log",
                                                      scale_moment1 = "level",
                                                      moment1 = "level",
                                                      stat_moment2 = "difference",
                                                      ages = c(40,60),
                                                      stats = c("mean","sd")
)

output_1b <- mindist::estimation_theta(
  theta_0 = c("beta" = {if(!is.null(beta)) beta_0 else NULL},
              # "gamma" = {if(is.null(gamma)) gamma_0 else NULL},
              "r" = {if(is.null(r)) 0.08 else NULL}
  ),
  beta = beta,
  r = NULL,
  gamma = gamma,
  approach = "one_step",
  ages = c(40,60),
  compute_standard_errors = FALSE,
  prediction_function = wealthyR:::model_capitulation,
  select_moments = select_moments,
  EP_2015 = EP_2015,
  EP_lon = EP_lon,
  EP_2018 = EP_2018,
  data_microsimulated = menages_structural2,
  observed_moment_data = simulations_moment_fixed2,
  N_moments = 180,
  wealth_var = "PATRI_NET",
  by = c("tr_age_2015", "tr_age_2015"),
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


# FIRST MOMENT ONLY =======================


simulations_moment_fixed3 = simulations_moment_fixed[Nmoment < 62]

output_1d <- mindist::estimation_theta(
  theta_0 = c("beta" = {if(!is.null(beta)) beta_0 else NULL},
              # "gamma" = {if(is.null(gamma)) gamma_0 else NULL},
              "r" = {if(is.null(r)) 0.08 else NULL}
  ),
  beta = beta,
  r = NULL,
  gamma = gamma,
  approach = "one_step",
  compute_standard_errors = FALSE,
  prediction_function = wealthyR:::model_capitulation,
  select_moments = select_moments,
  EP_2015 = EP_2015,
  EP_lon = EP_lon,
  EP_2018 = EP_2018,
  data_microsimulated = menages_structural2,
  observed_moment_data = simulations_moment_fixed3,
  N_moments = 13,
  wealth_var = "PATRI_NET",
  by = c("tr_age_2015", "tr_age_2015"),
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


# SECOND MOMENT ONLY =======================


output_1e <- mindist::estimation_theta(
  theta_0 = c("beta" = {if(!is.null(beta)) beta_0 else NULL},
              # "gamma" = {if(is.null(gamma)) gamma_0 else NULL},
              "r" = {if(is.null(r)) 0.08 else NULL}
  ),
  beta = beta,
  r = NULL,
  gamma = gamma,
  approach = "one_step",
  compute_standard_errors = FALSE,
  prediction_function = wealthyR:::model_capitulation,
  EP_2015 = EP_2015,
  EP_lon = EP_lon,
  EP_2018 = EP_2018,
  data_microsimulated = menages_structural2,
  observed_moment_data = simulations_moment_fixed,
  N_moments = 180,
  select_moments = 13:21,
  wealth_var = "PATRI_NET",
  by = c("tr_age_2015", "tr_age_2015"),
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

# SECOND MOMENT ONLY (40-60 ans) =======================


output_1f <- mindist::estimation_theta(
  theta_0 = c("beta" = {if(!is.null(beta)) beta_0 else NULL},
              # "gamma" = {if(is.null(gamma)) gamma_0 else NULL},
              "r" = {if(is.null(r)) 0.08 else NULL}
  ),
  beta = beta,
  r = NULL,
  gamma = gamma,
  approach = "one_step",
  compute_standard_errors = FALSE,
  prediction_function = wealthyR:::model_capitulation,
  EP_2015 = EP_2015,
  EP_lon = EP_lon,
  EP_2018 = EP_2018,
  data_microsimulated = menages_structural2,
  observed_moment_data = simulations_moment_fixed,
  ages = c(40,60),
  N_moments = 180,
  select_moments = 13:21,
  wealth_var = "PATRI_NET",
  by = c("tr_age_2015", "tr_age_2015"),
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

# FIRST MOMENT ONLY (40-60 ans) =======================

simulations_fixed2 <- capitulation::life_cycle_model(data = menages_structural2,
                                                     r = NULL,
                                                     gamma = gamma,
                                                     beta = beta,
                                                     scale_model = "level",
                                                     income_var = "y_indiv",
                                                     weight_var = NULL,
                                                     wealthvar_survey = "K_observed",
                                                     Hgiven_var = "hg",
                                                     Hreceived_var = "hr",
                                                     additional_vars = c("tr_age_2015", "tr_age_2015"))
simulations_2018_fixed2 <- simulations_fixed2

simulations_moment_fixed5 <- wealthyR:::create_moment(EP_2015 = simulations_fixed2,
                                                      EP_2018 = simulations_2018_fixed2,
                                                      by = c("AGE", "tr_age_2015"),
                                                      wealth_var = 'wealth',
                                                      age_var = "age",
                                                      N_moments = 180,
                                                      ages_cross_section = c(40, 60),
                                                      moment_var = "moment_data",
                                                      scale_variable_moment1 = "level",
                                                      scale_variable_moment2 = "level",
                                                      scale_moment1 = "level",
                                                      moment1 = "level",
                                                      stat_moment2 = "growth",
                                                      ages = c(40,60),
                                                      stats = c("mean","sd")
)


output_1g <- mindist::estimation_theta(
  theta_0 = c("beta" = {if(!is.null(beta)) beta_0 else NULL},
              # "gamma" = {if(is.null(gamma)) gamma_0 else NULL},
              "r" = {if(is.null(r)) 0.08 else NULL}
  ),
  beta = beta,
  r = 0.08,
  gamma = gamma,
  approach = "one_step",
  compute_standard_errors = FALSE,
  prediction_function = wealthyR:::model_capitulation,
  ages_cross_section = c(40,60),
  EP_2015 = EP_2015,
  EP_lon = EP_lon,
  EP_2018 = EP_2018,
  data_microsimulated = menages_structural2,
  observed_moment_data = simulations_moment_fixed,
  ages = c(40,60),
  N_moments = 180,
  select_moments = 1:12,
  wealth_var = "PATRI_NET",
  by = c("tr_age_2015", "tr_age_2015"),
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

## CHANGE TO K/Y ==========================

# simulations_fixed3a <- capitulation::life_cycle_model(data = menages_structural2,
#                                                       r = NULL,
#                                                       gamma = gamma,
#                                                       beta = beta,
#                                                       scale_model = "level",
#                                                       income_var = "y_indiv",
#                                                       weight_var = NULL,
#                                                       wealthvar_survey = "K_observed",
#                                                       Hgiven_var = "hg",
#                                                       Hreceived_var = "hr",
#                                                       additional_vars = c("AGE", "tr_age_2015"),
#                                                       get_capital_income = TRUE)
# simulations_fixed3a[,'k_y' := get('wealth')/get('Y')]
# simulations_fixed3a <- simulations_fixed3a[is.finite(k_y)]
# simulations_2018_fixed3a <- simulations_fixed3a
# 
# 
# simulations_moment_fixed3a <- wealthyR:::create_moment(EP_2015 = simulations_fixed3a,
#                                                        EP_2018 = simulations_2018_fixed3a,
#                                                        by = c("tr_age_2015", "tr_age_2015"),
#                                                        wealth_var = 'k_y',
#                                                        age_var = "age",
#                                                        N_moments = 180,
#                                                        moment_var = "moment_data",
#                                                        scale_variable_moment1 = "level",
#                                                        scale_variable_moment2 = "level",
#                                                        scale_moment1 = "level",
#                                                        moment1 = "level",
#                                                        stat_moment2 = "growth",
#                                                        ages = c(40,60),
#                                                        stats = c("mean","sd")
# )
# 
# output_1c <- mindist::estimation_theta(
#   theta_0 = c("beta" = {if(!is.null(beta)) beta_0 else NULL},
#               # "gamma" = {if(is.null(gamma)) gamma_0 else NULL},
#               "r" = {if(is.null(r)) 0.08 else NULL}
#   ),
#   beta = beta,
#   r = NULL,
#   gamma = gamma,
#   approach = "one_step",
#   ages = c(40,60),
#   compute_standard_errors = FALSE,
#   prediction_function = wealthyR:::model_capitulation,
#   select_moments = select_moments,
#   EP_2015 = EP_2015,
#   EP_lon = EP_lon,
#   EP_2018 = EP_2018,
#   data_microsimulated = menages_structural2,
#   observed_moment_data = simulations_moment_fixed3a,
#   N_moments = 180,
#   wealth_var = "k_y",
#   by = c("tr_age_2015", "tr_age_2015"),
#   scale_variable_moment1 = "level",
#   scale_variable_moment2 = "level",
#   scale_moment1 = "level",
#   moment1 = "level",
#   stat_moment2 = "growth",
#   moments_weights = "weight",
#   verbose = TRUE,
#   Hgiven_var = "hg",
#   Hreceived_var = "hr",
#   method = "Nelder-Mead"
# )
# 
# 
# # FIRST MOMENT ONLY (40-60 ans) & K/Y =======================
# 
# simulations_moment_fixed6 <- wealthyR:::create_moment(EP_2015 = simulations_fixed3a,
#                                                       EP_2018 = simulations_2018_fixed3a,
#                                                       by = c("AGE", "tr_age_2015"),
#                                                       wealth_var = 'k_y',
#                                                       age_var = "age",
#                                                       N_moments = 180,
#                                                       ages_cross_section = c(40, 60),
#                                                       moment_var = "moment_data",
#                                                       scale_variable_moment1 = "level",
#                                                       scale_variable_moment2 = "level",
#                                                       scale_moment1 = "level",
#                                                       moment1 = "level",
#                                                       stat_moment2 = "growth",
#                                                       ages = c(40,60),
#                                                       stats = c("mean","sd")
# )
# 
# output_1h <- mindist::estimation_theta(
#   theta_0 = c("beta" = {if(!is.null(beta)) beta_0 else NULL},
#               # "gamma" = {if(is.null(gamma)) gamma_0 else NULL},
#               "r" = {if(is.null(r)) 0.08 else NULL}
#   ),
#   beta = beta,
#   r = NULL,
#   gamma = gamma,
#   approach = "one_step",
#   compute_standard_errors = FALSE,
#   prediction_function = wealthyR:::model_capitulation,
#   ages_cross_section = c(40,60),
#   EP_2015 = EP_2015,
#   EP_lon = EP_lon,
#   EP_2018 = EP_2018,
#   data_microsimulated = menages_structural2,
#   observed_moment_data = simulations_moment_fixed6,
#   ages = c(40,60),
#   N_moments = 180,
#   select_moments = 1:12,
#   wealth_var = "k_y",
#   by = c("AGE", "tr_age_2015"),
#   scale_model = "level",
#   scale_variable_moment1 = "asinh",
#   scale_variable_moment2 = "log",
#   stat_moment2 = 'difference',
#   moment1 = "level",
#   moments_weights = "weight",
#   verbose = TRUE,
#   Hgiven_var = "hg",
#   Hreceived_var = "hr",
#   method = "Nelder-Mead"
# )
# 
# 
# # MOMENT 2 40-60 ANS & K/Y -----------------------------------
# 
# output_1j <- mindist::estimation_theta(
#   theta_0 = c("beta" = {if(!is.null(beta)) beta_0 else NULL},
#               # "gamma" = {if(is.null(gamma)) gamma_0 else NULL},
#               "r" = {if(is.null(r)) 0.08 else NULL}
#   ),
#   beta = beta,
#   r = NULL,
#   gamma = gamma,
#   approach = "one_step",
#   compute_standard_errors = FALSE,
#   prediction_function = wealthyR:::model_capitulation,
#   ages_cross_section = c(40,60),
#   EP_2015 = EP_2015,
#   EP_lon = EP_lon,
#   EP_2018 = EP_2018,
#   data_microsimulated = menages_structural2,
#   observed_moment_data = simulations_moment_fixed6,
#   ages = c(40,60),
#   N_moments = 180,
#   select_moments = 13:21,
#   wealth_var = "k_y",
#   by = c("AGE", "tr_age_2015"),
#   scale_model = "level",
#   scale_variable_moment1 = "asinh",
#   scale_variable_moment2 = "log",
#   stat_moment2 = 'difference',
#   moment1 = "level",
#   moments_weights = "weight",
#   verbose = TRUE,
#   Hgiven_var = "hg",
#   Hreceived_var = "hr",
#   method = "Nelder-Mead"
# )



print_out <- function(output, cible){
  print("-------------- Estimated beta -----------------\n")
  print(output$estimates$par)
  print("---------------- Nelder Mean output ---------\n")
  print(output$estimates)
  print("---------- Moments ----------------\n")
  print(output$moments)
  print("\n\n")
}


x = capture.output({
  print(toupper("============== Modèle de référence =============="))
  print_out(output_1)
  print(toupper("============== Moment 2 entre 40 et 60 ans =============="))
  print_out(output_1b)
  # print(toupper("============== Moment 2 entre 40 et 60 ans & K/Y =============="))
  # print_out(output_1c)
  print(toupper("============== Modèle de référence (uniquement moment 1) =============="))
  print_out(output_1d)
  print(toupper("============== Modèle de référence (uniquement moment 1 sur 40-60 ans) =============="))
  print_out(output_1g)
  # print(toupper("============== Modèle de référence (uniquement moment 1 sur 40-60 ans & K/Y) =============="))
  # print_out(output_1h)
  print(toupper("============== Modèle de référence (uniquement moment 2) =============="))
  print_out(output_1e)
  print(toupper("============== Modèle de référence (uniquement moment 2 sur 40-60 ans) =============="))
  print_out(output_1f)
  # print(toupper("============== Modèle de référence (uniquement moment 2 sur 40-60 ans & K/Y) =============="))
  # print_out(output_1j)
})



outfile <- "gamma_fixed_diff.txt"

cat(x, file = here::here("monte-carlo", outfile), sep = "\n", useBytes=TRUE)





env <- list(
  output_1,
  output_1b,
  # output_1c,
  output_1d,
  output_1g,
  # output_1h,
  output_1e,
  output_1f#,
  # output_1j
)

saveRDS(env, file = here::here("monte-carlo", "gamma_fixed_diff.Rds"))


# 4 METHODES AVEC BETA(1+R)=/=1 ---------------------------


output_2 <- mindist::estimation_theta(
  theta_0 = c("beta" = {if(is.null(beta)) beta_0 else NULL},
              # "gamma" = {if(is.null(gamma)) gamma_0 else NULL},
              "r" = {if(is.null(r)) 0.08 else NULL}
  ),
  beta = beta,
  r = 0.08,
  gamma = gamma,
  approach = "one_step",
  compute_standard_errors = FALSE,
  prediction_function = wealthyR:::model_capitulation,
  select_moments = select_moments,
  EP_2015 = EP_2015,
  EP_lon = EP_lon,
  EP_2018 = EP_2018,
  data_microsimulated = menages_structural2,
  observed_moment_data = simulations_moment,
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


output_2b <- mindist::estimation_theta(
  theta_0 = c("beta" = {if(is.null(beta)) 0.9 else NULL},
              # "gamma" = {if(is.null(gamma)) gamma_0 else NULL},
              "r" = {if(is.null(r)) 0.08 else NULL}
  ),
  beta = beta,
  r = 0.08,
  gamma = gamma,
  approach = "one_step",
  compute_standard_errors = FALSE,
  prediction_function = wealthyR:::model_capitulation,
  select_moments = select_moments,
  EP_2015 = EP_2015,
  EP_lon = EP_lon,
  EP_2018 = EP_2018,
  data_microsimulated = menages_structural2,
  observed_moment_data = simulations_moment,
  N_moments = 180,
  wealth_var = "PATRI_NET",
  by = c("AGE", "tr_age_2015"),
  scale_model = "level",
  scale_variable_moment1 = "log",
  scale_variable_moment2 = "log",
  stat_moment2 = 'difference',
  moment1 = "level",
  moments_weights = "weight",
  verbose = TRUE,
  Hgiven_var = "hg",
  Hreceived_var = "hr",
  method = "Nelder-Mead"
)



# VARIANTES SCENARIO ------

simulations_moment_age <- wealthyR:::create_moment(EP_2015 = simulations,
                                                   EP_2018 = simulations_2018,
                                                   by = c("AGE", "tr_age_2015"),
                                                   wealth_var = 'wealth',
                                                   age_var = "age",
                                                   N_moments = 180,
                                                   moment_var = "moment_data",
                                                   scale_variable_moment1 = "log",
                                                   scale_variable_moment2 = "log",
                                                   scale_moment1 = "level",
                                                   moment1 = "level",
                                                   stat_moment2 = "difference",
                                                   ages = c(40, 60),
                                                   stats = c("mean","sd")
)

output_1_var2a <- mindist::estimation_theta(
  theta_0 = c("beta" = {if(is.null(beta)) beta_0 else NULL},
              # "gamma" = {if(is.null(gamma)) gamma_0 else NULL},
              "r" = {if(is.null(r)) 0.08 else NULL}
  ),
  beta = beta,
  r = NULL,
  gamma = gamma,
  approach = "one_step",
  compute_standard_errors = FALSE,
  prediction_function = wealthyR:::model_capitulation,
  ages = c(40,60),
  select_moments = select_moments,
  EP_2015 = EP_2015,
  EP_lon = EP_lon,
  EP_2018 = EP_2018,
  data_microsimulated = menages_structural2,
  observed_moment_data = simulations_moment,
  N_moments = 180,
  wealth_var = "PATRI_NET",
  by = c("AGE", "tr_age_2015"),
  scale_model = "level",
  scale_variable_moment1 = "log",
  scale_variable_moment2 = "log",
  stat_moment2 = 'difference',
  moment1 = "level",
  moments_weights = "weight",
  verbose = TRUE,
  Hgiven_var = "hg",
  Hreceived_var = "hr",
  method = "Nelder-Mead"
)

output_2_var2a <- mindist::estimation_theta(
  theta_0 = c("beta" = {if(is.null(beta)) beta_0 else NULL},
              # "gamma" = {if(is.null(gamma)) gamma_0 else NULL},
              "r" = {if(is.null(r)) 0.08 else NULL}
  ),
  beta = beta,
  r = 0.08,
  gamma = gamma,
  approach = "one_step",
  compute_standard_errors = FALSE,
  prediction_function = wealthyR:::model_capitulation,
  ages = c(40,60),
  select_moments = select_moments,
  EP_2015 = EP_2015,
  EP_lon = EP_lon,
  EP_2018 = EP_2018,
  data_microsimulated = menages_structural2,
  observed_moment_data = simulations_moment,
  N_moments = 180,
  wealth_var = "PATRI_NET",
  by = c("AGE", "tr_age_2015"),
  scale_model = "level",
  scale_variable_moment1 = "log",
  scale_variable_moment2 = "log",
  stat_moment2 = 'difference',
  moment1 = "level",
  moments_weights = "weight",
  verbose = TRUE,
  Hgiven_var = "hg",
  Hreceived_var = "hr",
  method = "Nelder-Mead"
)
