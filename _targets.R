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
alpha <- 2.5e-6

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
      formulaMEAN = "MTHER ~ factor(SEXE) + lw",
      constantSD = TRUE,
      threshparam = lbounds
    )
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
    data_prediction,
    finalize_prediction_table(pre_prediction3)
  ),
  
  tar_target(
    simulations,
    capitulation::life_cycle_model(
      data_prediction,
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
  )
  
  
  
  
)