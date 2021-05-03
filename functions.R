create_inheritance_model <- function(path_survey =  "~/Enquete Patrimoine",
                                     formula = "MTHER ~ lw + tr_age + SEXE + tr_agfinetu",
                                     selection = NULL,
                                     search_iter = 10,
                                     taille_tr_age = 5,
                                     taille_tr_agfinetu = 2){
  
  EP_data <- wealthyR:::prepare_inheritance_sample(
    path_survey =  path_survey
  )
  
  
  # MODEL 1: INTERVAL REGRESSION ---------------
  
  inheritance_data <- REtage::prepare_estimation(EP_data,
                                                 taille_tr_age = taille_tr_age,
                                                 taille_tr_agfinetu = taille_tr_agfinetu)
  
  
  bounds <- c(3,8,15,30,60,100,150,200,250)*1000
  lbounds <- log(bounds)
  
  estim_data <- inheritance_data[get('income')>0]
  
  estim_data[,'MTHER' := as.numeric(as.character(MTHER))]
  estim_data[is.na(get("MTHER")), c("MTHER") := min(get("MTHER"))]
  estim_data <- estim_data[order(MTHER)]
  estim_data[,'age' := get('AGE')]
  # estim_data[,'findet' := get('AGFINETU')]
  
  estim_data <- estim_data[get('age') < 80]
  
  if (is.null(selection)){
    estim_data <- estim_data[get('inherited')]
  } else{
    estim_data$inherited <- factor(as.numeric(estim_data$inherited))
    # estim_data <- na.omit(estim_data, cols = c("MTHER", "lw", "tr_age", "SEXE", "tr_agfinetu",
    #                              "inherited"))
  }
  
  
  if (is.null(selection)){  
    inheritance_model <- oglm::oglmx(
      formulaMEAN = as.formula("MTHER ~ lw + tr_agfinetu"),
      formulaSD = NULL,
      selection = NULL,
      data = estim_data,
      threshparam = lbounds,
      # start = inheritance_model$start,
      # gradient = "numerical"
      start_method = "search",
      search_iter = search_iter
    )
    return(inheritance_model)
  } else{
    inheritance_model <- sampleSelection::selection(
      selection = as.formula('inherited ~ income_group'),
      outcome = as.formula("MTHER ~ lw  + tr_agfinetu"),
      boundaries = c(-Inf, lbounds, Inf),
      data = estim_data
    )
    
  }
  
  # library(sampleSelection)
  # probs0 <- data.frame(
  #   p0 = as.numeric(predict(inheritance_model, type = "response", part = "selection")),
  #   predict_outcome = as.numeric(predict(inheritance_model, type = "conditional", part = 'outcome'))
  # )
  # equivalent to : as.numeric(predict(oglm_model, type = "P[y == 0|Z]", newdata = dat))
  # equivalent to : predict(oglm_model, type = "E[y|X,y>0]", newdata = dat)
  
  return(inheritance_model)
}

construct_EP <- function(path_data = "~"){
  
  macro <- capitulation::macro
  
  
  EP_2018 <- wealthyR::individualize_EP(path_data = path_data, year = 2018,
                                        individualize_income = TRUE)
  EP_2015 <- wealthyR::individualize_EP(path_data = path_data, year = 2015,
                                        individualize_income = TRUE)
  
  
  # +++++++++++++++++++++++++++++++++++++++++++++++
  # B/ CREATE LONGITUDINAL INDIVIDUAL DATA ========
  # +++++++++++++++++++++++++++++++++++++++++++++++
  
  EP_2015[,'labor_income' := get("ZSALAIRES") + get("ZRETRAITES") + get("ZCHOMAGE")]
  EP_2015[,'top_10' := as.numeric(get('labor_income') > quantile(get('labor_income'), probs = 0.1, na.rm = TRUE))]
  EP_2018[,'labor_income' := get("ZSALAIRES") + get("ZRETRAITES") + get("ZCHOMAGE")]
  EP_2018[,'top_10' := as.numeric(get('labor_income') > quantile(get('labor_income'), probs = 0.1, na.rm = TRUE))]
  
  EP_lon <- wealthyR::longitudinal_survey(macro = macro,
                                          path_data = path_data,
                                          EP_2015 = EP_2015,
                                          EP_2018 = EP_2018)
  EP_lon[,'tr_age_2015' := floor(get("AGE_2015")/5)*5]
  
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


clean_data <- function(data, sex_var = "sexe",
                       diploma_var = "findet",
                       labor_income_var = "revenu",
                       total_income_var = "Y",
                       statut_var = NULL,
                       year = 2015){
  
  
  data[, c('SEXE') := data.table::fifelse(get(sex_var)==1,
                                          'Male',
                                          'Female')]
  data[, c('tr_diplome') := cut(get(diploma_var),
                                breaks = c(0, 16,18,21,25,
                                           max(get(diploma_var), na.rm = TRUE)),
                                include.lowest = TRUE)]
  
  
  data[, c(labor_income_var) := get(labor_income_var) + exp(rnorm(nrow(data)))]
  data[, c(total_income_var) := get(total_income_var) + exp(rnorm(nrow(data)))]
  
  data[, c('decile_w') := cut(get(labor_income_var),
                              quantile(get(labor_income_var),
                                       probs= 0:10/10, na.rm = TRUE),
                              labels = 1:10, include.lowest = TRUE
  )]
  data[, c('decile_y') := cut(get(total_income_var),
                              quantile(get(total_income_var),
                                       probs= 0:10/10, na.rm = TRUE),
                              labels = 1:10, include.lowest = TRUE
  )]
  
  if (is.null(statut_var)) return(data)
  
  data[,'retired' := data.table::fifelse(get(statut_var) == "5",
                                         "retired","active")]
  
  return(data)  
}





estim_model <- function(form = "MTHER ~ lw + age + I((age^2)/100) + AGFINETU + I((AGFINETU^2)/100)",
                        formulaSD = NULL,
                        optmeth = "NR",
                        thresholds = lbounds,
                        search_iter = 50){
  
  estim_data <- estim_data[order(MTHER)] 
  
  # inheritance_model <- REtage::ordered_model_threshold(
  #   data = data.frame(estim_data),
  #   formula = form,
  #   formulaSD = formulaSD,
  #   link = "probit",
  #   constantSD = TRUE,
  #   thresholds = lbounds,
  #   optmeth = optmeth
  # )
  
  inheritance_model <- oglm::oglmx(
    formulaMEAN = form,
    formulaSD = formulaSD,
    data = estim_data,
    threshparam = thresholds,
    start_method = "search",
    search_iter = search_iter
  )
  
  
  estim_data[, pred := predict(inheritance_model, estim_data,
                               type ="latent")$y_latent_pred]
  estim_data[, pred_cut := cut(pred, c(-Inf, lbounds, Inf),
                               labels = order(unique(MTHER)))]
  
  # Label des intervalles
  estim_data[, pred_label := cut(exp(pred)/1000, c(0, bounds/1000, Inf))]
  labels_h <- unique(estim_data[,.SD,.SDcols = c("pred_cut","pred_label")])
  labels_h[,pred_cut := as.numeric(as.character(pred_cut))]
  estim_data <- merge(estim_data, labels_h, by.x = "MTHER", by.y = "pred_cut")
  data.table::setnames(estim_data, old = c("pred_label.x", "pred_label.y"),
                       new = c("predicted","observed")
  )
  
  
  library(ggplot2)
  
  confusion_matrix <- as.data.frame(table(estim_data$pred_cut, estim_data$MTHER))
  
  p1 <- ggplot(data = confusion_matrix,
               mapping = aes(x = Var1,
                             y = Var2)) +
    geom_tile(aes(fill = Freq)) +
    geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
    scale_fill_viridis_c(trans = "log", labels = scales::number_format(accuracy = 1),
                         option = "plasma") +
    scale_y_discrete(limits = rev(levels(confusion_matrix$Var2))) +
    labs(x = "Predicted", y = "Actual")
  
  
  confusion_matrix2 <- as.data.frame(table(estim_data$pred_cut, estim_data$MTHER))
  data.table::setDT(confusion_matrix2)
  confusion_matrix2[,'Freq2' := as.numeric(Freq)/sum(as.numeric(Freq)), by = Var2]
  
  p2 <- ggplot(data = confusion_matrix2,
               mapping = aes(x = Var1,
                             y = Var2)) +
    geom_tile(aes(fill = Freq2)) +
    geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
    scale_fill_viridis_c(trans = "log", labels = scales::percent_format(accuracy = 1),
                         option = "plasma") +
    scale_y_discrete(limits = rev(levels(confusion_matrix$Var2))) +
    labs(x = "Predicted", y = "Actual")
  
  
  tempdata <- estim_data[,lapply(.SD, function(x) as.numeric(as.character(x))),.SDcols = c("MTHER", "pred_cut")]
  tempdata <- data.table::melt(tempdata)
  tempdata <- merge(tempdata, na.omit(labels_h), by.x = "value", by.y = "pred_cut")
  tempdata[,'variable' := data.table::fifelse(
    get("variable") == "MTHER", "observed", "predicted"
  )]
  # tempdata[pred_label == "(-Inf,3]", pred_label := "Less than 3"]
  # tempdata[pred_label == "(250, Inf]", pred_label := "More than 250"]
  
  p3 <- ggplot(tempdata) +
    geom_histogram(aes(x = pred_label, fill = variable), stat = "count",
                   position = position_dodge(preserve = "single")) +
    scale_fill_viridis_d() +
    theme(axis.text.x=element_text(angle=50, hjust=1),
          legend.position = "bottom") +
    labs(x = "Inherited wealth bracket")
  
  
  accur <- sum(confusion_matrix2[Var1 == Var2]$Freq)/sum(confusion_matrix2$Freq)
  # print(sprintf("Accuracy: %1.2f%%", 100*accur))
  
  return(list(p1,p2,p3,'model' = inheritance_model, "accuracy" = accur))
}



get_quantiles <- function(dt, yvar, pondvar = NULL){
  
  if (is.null(pondvar)){
    x <- dt[,quantile(get(yvar), probs = c(1:9, 9.5, 9.9)/10)]
  } else{
    x <- dt[,Hmisc::wtd.quantile(get(yvar), probs = c(1:9, 9.5, 9.9)/10,
                                 weights = get(pondvar))]
  }
  
  return(as.numeric(x))
}



share_total <- function(simulations, yvar = "wealth", jitterize = FALSE){
  
  year_2015 <- data.table::copy(simulations)
  
  if (jitterize){
    year_2015[, c(yvar) := get(yvar) + exp(rnorm(nrow(year_2015)))]
  }
  
  year_2015[, c('decile_wealth') := cut(get(yvar),
                                        quantile(get(yvar),
                                                 probs= 0:10/10),
                                        labels = 1:10, include.lowest = TRUE
  )]
  year_2015[, c('percentile_wealth') := cut(get(yvar),
                                            quantile(get(yvar),
                                                     probs= 0:100/100),
                                            labels = 1:100, include.lowest = TRUE
  )]
  year_2015 <- year_2015[,.(sh_wealth = sum(get(yvar))),
                         by = "percentile_wealth"]
  
  year_2015[,c('top_10','top_1') := list(as.numeric(as.character(get('percentile_wealth')))>90,
                                         as.numeric(as.character(get('percentile_wealth'))) == 100)]
  
  top10 <- year_2015[,.(sh_wealth = sum(sh_wealth)), by = top_10]
  top1 <- year_2015[,.(sh_wealth = sum(sh_wealth)), by = top_1]
  
  top10[,sh_wealth := sh_wealth/sum(sh_wealth)]
  top1[,sh_wealth := sh_wealth/sum(sh_wealth)]
  
  top10 <- 100*as.numeric(top10[get("top_10")][['sh_wealth']])
  top1 <- 100*as.numeric(top1[get("top_1")][['sh_wealth']])
  
  df <- data.table::data.table("Group" = c("Top 10 \\%", "Top 1 \\%"),
                               "yvar" = c(top10, top1))
  
  data.table::setnames(df, old = "yvar", new = yvar)
  
  return(
    df
  )
}

