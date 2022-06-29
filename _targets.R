library(targets)

source("functions.R")

tar_option_set(
  packages = c("tablelight"),
  imports = c("capitulation", "wealthyR", "REtage", "oglm")
)


bounds <- c(3,8,15,30,60,100,150,200,250)*1000
lbounds <- log(bounds)
path_data <- ".."
path_mortalite = "../probas_survie/ciblesDemographie_FE_dev.xls"

# parametres
selection_model = NULL
time_0 = "birth"
# debt_wealthSurvey = "MTDETTES",
taille_tr_age = 5
taille_tr_agfinetu = 2
path_data_suffix = "/Destinie2120"
extension = ".rda"
wealthvar_survey = "PATRI_NET"
debt_wealthSurvey = NULL
drawK = TRUE

beta  <- 0.992
gamma <- 1
r <- 0.03


list(
  
  tar_target(
    EP_data,
    wealthyR:::prepare_inheritance_sample(
      path_survey =  "../Enquete Patrimoine"
    )[(MER1E == 3 & PER1E==3) | (!is.na(MTHER))],
    format = "fst_dt" # Set format = "aws_qs" in targets <= 0.10.0.
  ),
  
  tar_target(
    inheritance_data,
    REtage::prepare_estimation(EP_data),
    format = "fst_dt" # Set format = "aws_qs" in targets <= 0.10.0.
  ),
  
  tar_target(
    estim_data,
    create_estim_data(inheritance_data),
    format = "fst_dt" # Set format = "aws_qs" in targets <= 0.10.0.
  ),
  
  tar_target(
    inheritance_model_temp,
    oglm::oglmx(
      data = data.frame(estim_data[MTHER>0]),
      link = "probit",
      formulaMEAN = "MTHER ~ factor(SEXE) + lw + factor(tr_age) + factor(tr_agfinetu)",
      constantSD = TRUE,
      threshparam = lbounds,
      start_method = "search"
    )
  ),
  
  tar_target(
    probit_model,
    glm(inherited ~ factor(tr_age) + factor(tr_agfinetu),
        family = binomial(link = "probit"), 
        data = estim_data)
  ),  
  
  tar_target(
    inheritance_model,
    tweak_oglm(inheritance_model_temp)
  ),
  
  tar_target(
    enquetes_patrimoine,
    construct_EP(path_data),
    format = "qs"
  ),
  
  tar_target(
    EP_2015_temp,
    enquetes_patrimoine[['EP_2015']],
    format = "fst_dt" # Set format = "aws_qs" in targets <= 0.10.0.
  ),
  
  tar_target(
    EP_2015,
    data.table::copy(EP_2015_temp)[,'tr_age_2015' := floor(get("AGE")/5)*5],
    format = "fst_dt" # Set format = "aws_qs" in targets <= 0.10.0.
  ),
  
  tar_target(
    EP_2018,
    enquetes_patrimoine[['EP_2018']],
    format = "fst_dt" # Set format = "aws_qs" in targets <= 0.10.0.
  ),
  
  tar_target(
    EP_lon,
    enquetes_patrimoine[['EP_lon']],
    format = "fst_dt" # Set format = "aws_qs" in targets <= 0.10.0.
  ),
  
  # tar_target(
  #   data_prediction,
  #   capitulation::prepare_data(
  #     path_data = "..",
  #     inheritance_model = inheritance_model,
  #     selection_model = NULL,
  #     time_0 = "birth",
  #     # debt_wealthSurvey = "MTDETTES",
  #     taille_tr_age = 5,
  #     taille_tr_agfinetu = 2,
  #     path_data_suffix = "/Destinie2120",
  #     extension = ".rda",
  #     wealthvar_survey = "PATRI_NET"
  #   ),
  #   format = "qs"
  # ),
  
  tar_target(
    mortality,
    create_mortality_table(path_mortalite)
  ),
  
  # ------------------------------
  # BREAK INTO PIECES
  
  tar_target(
    simul,
    capitulation::import_Destinie(path_data = paste0(path_data, path_data_suffix),
                                  format = "data.table", extension = ".rda")
  ),
  
  tar_target(
    tables,
    capitulation::arrange_Destinie(simul)
  ),
  
  # Put those tables into global environment rather than inside a list
  #list2env(tables,globalenv())
  
  
  # =========================================
  #       II - TRANSFORM INDIVIDUAL TRAJECTORY
  #           INTO HOUSEHOLD LEVEL DATA
  # =========================================
  
  tar_target(
    pre_indiv,
    capitulation::prepare(table_indiv = tables$pre_indiv,
                          pensions = tables$pensions,
                          descript = tables$description)
  ),
  
  tar_target(
    indiv_pre1,
    capitulation::household_composition(table_indiv = pre_indiv,
                                        simul = tables,
                                        descript =  tables$description,
                                        call.Rcpp = TRUE),
    format = "fst_dt"
  ),
  
  tar_target(
    data_event,
    capitulation::death_event(indiv),
    format = "fst_dt"
  ),
  
  tar_target(
    indiv_pre2,
    capitulation::income_household(table_indiv = indiv_pre1),
    format = "fst_dt"
  ),
  
  tar_target(
    indiv,
    capitulation::match_wealthSurvey(indiv_pre2,
                                     path_survey =  path_data,
                                     drawK = drawK,
                                     time_0 = time_0,
                                     wealth_wealthSurvey = wealthvar_survey,
                                     debt_wealthSurvey = debt_wealthSurvey),
    format = "fst_dt"
  ),
  
  tar_target(
    macro,
    simul$macro[,.SD,.SDcols = c('annee','Prix')],
    format = "fst_dt"
  ),
  
  tar_target(
    pre_household_table,
    arrange_household_table(indiv, macro, wealthvar_survey = wealthvar_survey),
    format = "fst_dt"
  ),
  
  tar_target(
    pre_household_table2,
    capitulation:::income_group2(pre_household_table,
                                 quantile = quantile,
                                 groupvar = groupvar,
                                 incomevar = "y_indiv",
                                 yearvar = "annee"
    ),
    format = "fst_dt"
  ),
  
  tar_target(
    household_table,
    data.table::copy(pre_household_table2)[is.nan(get("K_observed")), "K_observed" := 0L]
  ),
  
  
  tar_target(
    household_table_mortality,
    merge(household_table, mortality, by = c("age","annee","sexe"), all.x = TRUE)
  ),
  
  tar_target(
    pre_prediction1,
    capitulation:::arrange_inheritance(
      household_table_mortality,
      data_event = data_event,
      tables = tables,
      time_0 = time_0,
      taille_tr_age = taille_tr_age,
      taille_tr_agfinetu = taille_tr_agfinetu
    )
  ),
  
  tar_target(
    pre_prediction2,
    data.table::copy(pre_prediction1)[,'SEXE' := factor(get('sexe.x'))]
  ),
  
  tar_target(
    pre_prediction3,
    capitulation:::apply_inheritance_model(pre_prediction2, inheritance_model,
                                           selection_model = NULL)
  ),
  
  tar_target(
    pre_prediction3_selection,
    capitulation:::apply_inheritance_model(pre_prediction2, inheritance_model,
                                           selection_model = probit_model)
  ),  
  
  tar_target(
    data_prediction,
    finalize_prediction_table(pre_prediction3)
  ),
  
  tar_target(
    data_prediction_selection,
    finalize_prediction_table(pre_prediction3_selection)
  ),
  # 
  # tar_target(
  #   menages_structural2,
  #   readRDS('start_estimation.rds'),
  #   format = "fst_dt"
  # ),
  
# 
#   tar_target(
#     data_prediction_selection2,
#     merge(menages_structural2,
#           data_prediction_selection[,.SD,.SDcols = c("Id","annee", "proba_survie")],
#           by = c("Id","annee")),
#     format = "fst_dt"
#   ),
  
  
  tar_target(
    simulations2,
    capitulation::life_cycle_model(
      data_prediction[age>findet],
      wealthvar_survey = "K_observed",
      r = r,
      beta = beta,
      gamma = gamma,
      observation_year = 2009,
      income_var = "revenu",
      Hgiven_var = "hg",
      Hreceived_var = "hr",
      probability_survival_var = "proba_survie",
      return_last = FALSE,
      get_capital_income = TRUE,
      additional_vars = c("tr_age_2015","tr_age","SEXE","findet","ageliq", "id_household", "UC"))
  ),
  
  
  tar_target(
    simulations,
    capitulation::life_cycle_model(
      data_prediction[age>findet],
      wealthvar_survey = "K_observed",
      r = r,
      beta = beta,
      gamma = gamma,
      observation_year = 2009,
      income_var = "revenu",
      Hgiven_var = "hg",
      Hreceived_var = "hr",
      return_last = FALSE,
      get_capital_income = TRUE,
      additional_vars = c("tr_age_2015","tr_age","SEXE","findet","ageliq", "id_household", "UC"))
  ),
  
  tar_target(
    test_moments,
    wealthyR:::create_moment_data(EP_2015 = EP_2015, EP_2018 = EP_2018,
                                  EP_lon = EP_lon, 
                                  data_microsimulated = data_prediction_selection,
                                  observed_moment_data = NULL,
                                  r = r,
                                  gamma = gamma,
                                  beta = as.numeric(
                                    output$estimates$theta_hat
                                  ),
                                  r.parameters = NULL,
                                  gamma.parameters = NULL,
                                  beta.parameters = NULL,
                                  r_low = r,
                                  r_high = r,
                                  N_moments = 180,
                                  wealth_var = "PATRI_NET",
                                  age_var_simulations = "age",
                                  normalize = FALSE,
                                  scale_model = "log",
                                  scale_variable_moment1 = "asinh",
                                  scale_variable_moment2 = "log",
                                  scale_moment1 = "level",                               
                                  moment1 = "level",
                                  stat_moment2 = "difference",
                                  ages = c(30,65),
                                  exclude_negative = FALSE,
                                  Hgiven_var = "hg",
                                  Hreceived_var = "hr",
                                  additional_vars = c("tr_age","SEXE","tr_agfinetu","findet"),
                                  by = c("tr_age_2015", "tr_age_2015"))  
  ),
  
  tar_target(
    output,
    mindist::estimation_theta(
      theta_0 = c("beta" = 0.9,
                  "gamma" = {if(is.null(gamma)) gamma_0 else NULL},
                  "r" = {if(is.null(r)) 0.03 else NULL}
      ),
      beta = NULL,
      r = r,
      gamma = gamma,
      approach = "two_step",
      prediction_function = wealthyR:::model_capitulation,
      non_ricardian = FALSE,
      # non_ricardian_var = "non_ricardian",
      EP_2015 = EP_2015,
      EP_lon = EP_lon,
      EP_2018 = EP_2018,
      data_microsimulated = data_prediction,
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
      method = "Nelder-Mead",
      additional_vars = c("tr_age","SEXE","tr_agfinetu","findet")
    )
  ),
  tar_target(
    output_uncertainty,
    mindist::estimation_theta(
      theta_0 = c("beta" = 0.9,
                  "gamma" = {if(is.null(gamma)) gamma_0 else NULL},
                  "r" = {if(is.null(r)) 0.03 else NULL}
      ),
      beta = NULL,
      r = r,
      gamma = gamma,
      probability_survival_var = "proba_survie",
      approach = "two_step",
      prediction_function = wealthyR:::model_capitulation,
      non_ricardian = FALSE,
      # non_ricardian_var = "non_ricardian",
      EP_2015 = EP_2015,
      EP_lon = EP_lon,
      EP_2018 = EP_2018,
      data_microsimulated = data_prediction,
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
      method = "Nelder-Mead",
      additional_vars = c("tr_age","SEXE","tr_agfinetu","findet")
    )
  ),
  
  tar_target(
    output_selection,
    mindist::estimation_theta(
      theta_0 = c("beta" = 0.9,
                  "gamma" = {if(is.null(gamma)) gamma_0 else NULL},
                  "r" = {if(is.null(r)) 0.03 else NULL}
      ),
      beta = NULL,
      r = r,
      gamma = gamma,
      approach = "two_step",
      prediction_function = wealthyR:::model_capitulation,
      non_ricardian = FALSE,
      # non_ricardian_var = "non_ricardian",
      EP_2015 = EP_2015,
      EP_lon = EP_lon,
      EP_2018 = EP_2018,
      data_microsimulated = data_prediction_selection,
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
      method = "Nelder-Mead",
      additional_vars = c("tr_age","SEXE","tr_agfinetu","findet")
    )
  ),
  
  tar_target(
    output_uncertainty_selection,
    mindist::estimation_theta(
      theta_0 = c("beta" = 0.9,
                  "gamma" = {if(is.null(gamma)) gamma_0 else NULL},
                  "r" = {if(is.null(r)) 0.03 else NULL}
      ),
      beta = NULL,
      r = r,
      gamma = gamma,
      probability_survival_var = "proba_survie",
      approach = "two_step",
      prediction_function = wealthyR:::model_capitulation,
      non_ricardian = FALSE,
      # non_ricardian_var = "non_ricardian",
      EP_2015 = EP_2015,
      EP_lon = EP_lon,
      EP_2018 = EP_2018,
      data_microsimulated = data_prediction_selection,
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
      method = "Nelder-Mead",
      additional_vars = c("tr_age","SEXE","tr_agfinetu","findet")
    )
  ),
  
  tar_target(
    test_moments_uncertainty,
    wealthyR:::create_moment_data(EP_2015 = EP_2015, EP_2018 = EP_2018,
                                  EP_lon = EP_lon, 
                                  data_microsimulated = data_prediction,
                                  observed_moment_data = NULL,
                                  probability_survival_var = "proba_survie",
                                  r = r,
                                  gamma = gamma,
                                  beta = as.numeric(
                                    output_uncertainty$estimates$theta_hat
                                    ),
                                  r.parameters = NULL,
                                  gamma.parameters = NULL,
                                  beta.parameters = NULL,
                                  r_low = r,
                                  r_high = r,
                                  N_moments = 180,
                                  wealth_var = "PATRI_NET",
                                  age_var_simulations = "age",
                                  normalize = FALSE,
                                  scale_model = "log",
                                  scale_variable_moment1 = "asinh",
                                  scale_variable_moment2 = "log",
                                  scale_moment1 = "level",                               
                                  moment1 = "level",
                                  stat_moment2 = "difference",
                                  ages = c(30,65),
                                  exclude_negative = FALSE,
                                  Hgiven_var = "hg",
                                  Hreceived_var = "hr",
                                  additional_vars = c("tr_age","SEXE","tr_agfinetu","findet"),
                                  by = c("tr_age_2015", "tr_age_2015"))  
  ),
  
  tar_target(
    test_moments_uncertainty_selection,
    wealthyR:::create_moment_data(EP_2015 = EP_2015, EP_2018 = EP_2018,
                                  EP_lon = EP_lon, 
                                  data_microsimulated = data_prediction_selection,
                                  observed_moment_data = NULL,
                                  probability_survival_var = "proba_survie",
                                  r = r,
                                  gamma = gamma,
                                  beta = as.numeric(
                                    output_uncertainty_selection$estimates$theta_hat
                                  ),
                                  r.parameters = NULL,
                                  gamma.parameters = NULL,
                                  beta.parameters = NULL,
                                  r_low = r,
                                  r_high = r,
                                  N_moments = 180,
                                  wealth_var = "PATRI_NET",
                                  age_var_simulations = "age",
                                  normalize = FALSE,
                                  scale_model = "log",
                                  scale_variable_moment1 = "asinh",
                                  scale_variable_moment2 = "log",
                                  scale_moment1 = "level",                               
                                  moment1 = "level",
                                  stat_moment2 = "difference",
                                  ages = c(30,65),
                                  exclude_negative = FALSE,
                                  Hgiven_var = "hg",
                                  Hreceived_var = "hr",
                                  additional_vars = c("tr_age","SEXE","tr_agfinetu","findet"),
                                  by = c("tr_age_2015", "tr_age_2015"))  
  ),

  tar_target(
    test_moments_selection,
    wealthyR:::create_moment_data(EP_2015 = EP_2015, EP_2018 = EP_2018,
                                  EP_lon = EP_lon, 
                                  data_microsimulated = data_prediction_selection,
                                  observed_moment_data = NULL,
                                  probability_survival_var = "proba_survie",
                                  r = r,
                                  gamma = gamma,
                                  beta = as.numeric(
                                    output_selection$estimates$theta_hat
                                  ),
                                  r.parameters = NULL,
                                  gamma.parameters = NULL,
                                  beta.parameters = NULL,
                                  r_low = r,
                                  r_high = r,
                                  N_moments = 180,
                                  wealth_var = "PATRI_NET",
                                  age_var_simulations = "age",
                                  normalize = FALSE,
                                  scale_model = "log",
                                  scale_variable_moment1 = "asinh",
                                  scale_variable_moment2 = "log",
                                  scale_moment1 = "level",                               
                                  moment1 = "level",
                                  stat_moment2 = "difference",
                                  ages = c(30,65),
                                  exclude_negative = FALSE,
                                  Hgiven_var = "hg",
                                  Hreceived_var = "hr",
                                  additional_vars = c("tr_age","SEXE","tr_agfinetu","findet"),
                                  by = c("tr_age_2015", "tr_age_2015"))  
  ),
  
  
    
  tar_target(
    plots_moments_uncertainty, 
    plot_moment_age_wide(test_moments_uncertainty)
  ),
  tar_target(
    plots_moments, 
    plot_moment_age_wide(test_moments)
  )
  
  
  
)
