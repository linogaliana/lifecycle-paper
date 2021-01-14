create_inheritance_model <- function(path_survey =  "~/Enquete Patrimoine",
                                     formula = "MTHER ~ lw + tr_age + SEXE + tr_agfinetu",
                                     search_iter = 10){
  
  EP_data <- wealthyR:::prepare_inheritance_sample(
    path_survey =  path_survey
  )
  
  
  # MODEL 1: INTERVAL REGRESSION ---------------
  
  inheritance_data <- REtage::prepare_estimation(EP_data)
  
  
  bounds <- c(3,8,15,30,60,100,150,200,250)*1000
  lbounds <- log(bounds)
  
  estim_data <- inheritance_data[get('income')>0]
  
  estim_data[,'MTHER' := as.numeric(as.character(MTHER))]
  estim_data <- estim_data[order(MTHER)]
  estim_data[,'age' := get('AGE')]
  # estim_data[,'findet' := get('AGFINETU')]
  
  inheritance_model <- oglm::oglmx(
    formulaMEAN = formula,
    formulaSD = NULL,
    data = estim_data,
    threshparam = lbounds,
    start_method = "search",
    search_iter = search_iter
  )
  
  return(inheritance_model)
}

construct_EP <- function(path_data = "~"){
  
  macro <- capitulation::macro
  
  
  EP_2018 <- wealthyR::individualize_EP(path_data = path_data, year = 2018)
  EP_2015 <- wealthyR::individualize_EP(path_data = path_data, year = 2015)
  
  
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


report_loss_function <- function(r, beta, gamma,
                                 population,
                                 EP_2015, EP_2018, EP_lon,
                                 verbose = FALSE){
  
  output <- mindist:::calibration_theta(
    theta = c("beta" = beta,
              "gamma" = gamma,
              "r" = r),
    beta = beta,
    r = r,
    gamma = gamma,
    model_function = mindist:::loss_function,
    prediction_function = wealthyR:::model_capitulation,
    EP_2015 = EP_2015,
    EP_lon = EP_lon,
    EP_2018 = EP_2018,
    data_microsimulated = population,
    N_moments = 180,
    by = c("AGE", "tr_age_2015"),
    # moment1 = "share",
    scale = "log",
    moments_weights = "weight",
    verbose = TRUE,
    Hgiven_var = "hg",
    Hreceived_var = "hr"
  )
  
  epseps <- sum((output$moments$moment_optimum$weight*output$moments$moment_optimum$epsilon)^2)
  epsWeps <- as.numeric(
    t(output$moments$moment_optimum$weight*output$moments$moment_first_step$epsilon) %*% output$estimates$W_1 %*% (output$moments$moment_optimum$weight*output$moments$moment_first_step$epsilon)
  )
  l_theta <- epsWeps/(length(output$moments$moment_optimum$epsilon)^2)
  
  
  if (isTRUE(verbose)){
    message(sprintf("\\epsilon' \\epsilon: %s", epseps))
    message(sprintf("\\epsilon' W \\epsilon: %s", epsWeps))
    message(sprintf("(\\epsilon' W \\epsilon)/(M^2): %s", l_theta))
  }
  
  
  
  return(
    list(
      "epseps" = epseps,
      "epsWeps" = epsWeps,
      "l_theta" = l_theta
    )
  )
  
  
}

r= 0.03
gamma= 0.7610854
beta= 0.9760395

report_epsilon <- function(r, beta, gamma,
                           population,
                           EP_2015, EP_2018, EP_lon,
                           verbose = FALSE){
  
  
  output <- wealthyR:::model_capitulation(
    theta = c("beta" = beta,
              "gamma" = gamma,
              "r" = r),
    # beta = beta,
    # r = r,
    # gamma = gamma,
    prediction_function = wealthyR:::model_capitulation,
    EP_2015 = EP_2015,
    EP_lon = EP_lon,
    EP_2018 = EP_2018,
    data_microsimulated = population,
    N_moments = 180,
    by = c("AGE", "tr_age_2015"),
    # moment1 = "share",
    scale = "log",
    moments_weights = "weight",
    verbose = TRUE,
    Hgiven_var = "hg",
    Hreceived_var = "hr"
  )
  
  epseps <- sum((output$weight*output$epsilon)^2)
  
  if (isTRUE(verbose)){
    message(sprintf("\\epsilon' \\epsilon: %s", epseps))
  }
  
  return(
    epseps
  )
}
