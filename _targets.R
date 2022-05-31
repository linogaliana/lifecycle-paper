library(targets)

source("functions.R")

tar_option_set(
  packages = c("tablelight"),
  imports = c("capitulation", "wealthyR", "REtage", "oglm")
)

bounds <- c(3,8,15,30,60,100,150,200,250)*1000
lbounds <- log(bounds)
path_data <- ".."

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
    EP_2015,
    enquetes_patrimoine[['EP_2015']],
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
  
  tar_target(
    data_prediction,
    capitulation::prepare_data(
      path_data = "..",
      inheritance_model = inheritance_model,
      selection_model = NULL,
      time_0 = "birth",
      # debt_wealthSurvey = "MTDETTES",
      taille_tr_age = 5,
      taille_tr_agfinetu = 2,
      path_data_suffix = "/Destinie2120", 
      extension = ".rda",
      wealthvar_survey = "PATRI_NET"
    ),
    format = "fst_dt"
  )
  
  
  
)