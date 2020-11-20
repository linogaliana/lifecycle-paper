create_inheritance_model <- function(path_survey =  "~/Enquete Patrimoine",
                                     formula = "MTHER ~ lw + age + I((age^2)/100) + AGFINETU + I((AGFINETU^2)/100)"){
  
  EP_data <- wealthyR:::prepare_inheritance_sample(
    path_survey =  "~/Enquete Patrimoine"
  )
  
  # MODEL 1: INTERVAL REGRESSION ---------------
  
  inheritance_data <- REtage::prepare_estimation(EP_data)
  
  
  bounds <- c(3,8,15,30,60,100,150,200,250)*1000
  lbounds <- log(bounds)
  
  estim_data <- inheritance_data[get('revenu')>0]
  
  estim_data[,'MTHER' := as.numeric(as.character(MTHER))]
  estim_data <- estim_data[order(MTHER)]
  estim_data[,'age' := get('AGE')]
  # estim_data[,'findet' := get('AGFINETU')]
  
  inheritance_model <- REtage::ordered_model_threshold(
    data = data.frame(estim_data[order(MTHER)]),
    formula = "MTHER ~ lw + age + I((age^2)/100) + AGFINETU + I((AGFINETU^2)/100)",
    link = "probit",
    constantSD = TRUE,
    thresholds = lbounds
  )
  
  return(inheritance_model)
}

construct_EP <- function(path_data = "~"){
  
  macro <- capitulation::macro
  
  
  EP_2018 <- wealthyR::individualize_EP(path_data = "~", year = 2018)
  EP_2015 <- wealthyR::individualize_EP(path_data = "~", year = 2015)
  
  
  # +++++++++++++++++++++++++++++++++++++++++++++++
  # B/ CREATE LONGITUDINAL INDIVIDUAL DATA ========
  # +++++++++++++++++++++++++++++++++++++++++++++++
  
  EP_2015[,'labor_income' := get("ZSALAIRES_I") + get("ZRETRAITES_I") + get("ZCHOMAGE_I")]
  EP_2015[,'top_10' := as.numeric(get('labor_income') > quantile(get('labor_income'), probs = 0.1, na.rm = TRUE))]
  
  EP_lon <- wealthyR::longitudinal_survey(macro = macro,
                                          path_data = path_data,
                                          EP_2015 = EP_2015,
                                          EP_2018 = EP_2018)
  EP_lon[,'tr_age_2015' := floor(get("AGEPR_2015")/5)*5]
  
  EP_lon[,'labor_income' := get("ZSALAIRES_I") + get("ZRETRAITES_I") + get("ZCHOMAGE_I")]
  EP_lon[,'top_10' := as.numeric(get('labor_income') > quantile(get('labor_income'), probs = 0.1, na.rm = TRUE))]
  
  data <- list(
    'EP_2015' = EP_2015,
    'EP_2018' = EP_2018,
    'EP_lon' = EP_lon
  )
  
  return(data)  
}

