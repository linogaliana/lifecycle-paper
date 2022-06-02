finalize_prediction_table <- function(household_table){
  dt <- data.table::copy(household_table)
  if ('non_ricardian' %in% colnames(dt)){
    dt[is.na(non_ricardian), c("non_ricardian") := get("H_given")>0]
  }
  dt[,'hg' := get('H_given')]
  dt[,'hr' := get('H_received')]
  dt[,'tr_age_2015' := floor(get("age")/5)*5]
  return(dt)
}

arrange_household_table <- function(indiv, macro,
                                    id_var = "Id",
                                    income_var = "salaire",
                                    age_var = "age",
                                    ageliq_var = "ageliq",
                                    matrimonial_var = "matri",
                                    wealthvar_survey = "PATFISOM",
                                    debt_wealthSurvey = NULL,
                                    findet_var = "findet",
                                    weight_var = NULL,
                                    deflate = TRUE,
                                    time_0 = c("birth","graduation")){
  
  time_0 <- match.arg(time_0)
  
  # GET REAL VALUES
  household_table <- merge(indiv, macro, by = 'annee')
  household_table[,'y_real' := get('salaire_tot')/get('Prix')]
  
  
  # WEALTH CONCEPT: NOMINAL OR REAL
  if (deflate)
    household_table[, (wealthvar_survey) := get(wealthvar_survey)/get('Prix')]
  
  # KEEP ONLY OBSERVATION AFTER STARTING WORKING LIFE
  if (time_0 == "graduation") household_table <- household_table[get(age_var)>=get(findet_var)]
  
  
  household_table[,`:=` ('y_indiv' = get('y_real')/get('nspouses'),
                         'K_observed' = get(wealthvar_survey)/get('nspouses')
  )]
  
  household_table[, c("K_observed") := mean(get("K_observed"),na.rm=TRUE), by = c('Id')]
  
  if (is.null(weight_var)){
    household_table[, "weight" := 1L]
    weight_var <- "weight"
  }
  
  id_household <- as.character(
    household_table[, paste0(do.call(pmin, .SD), "_", do.call(pmax, .SD)), .SDcols = c("Id",'conjoint')]
  )
  household_table[, 'id_household' := id_household]
  
  
  household_table <- household_table[,.SD,
                                     .SDcols = c(id_var,"annee", 'id_household',
                                                 'y_indiv',
                                                 'K_observed', age_var,
                                                 ageliq_var,
                                                 "tt", #weight_var,
                                                 "sexe", "UC"
                                                 #findet_var
                                     )]  
  
  return(household_table)
}

read_mortality_table_sex <- function(path_mortalite, sex = "F"){
  
  data_wide <- readxl::read_excel(
    path_mortalite, sheet = paste0("deces", sex),
    skip = 1
  )
  data.table::setDT(data_wide)
  data.table::setnames(data_wide, old = colnames(data_wide)[1], new = "age")
  # remove end of excel
  data_wide <- data_wide[age %in% as.character(seq(0, 120))]
  colnames(data_wide) <- gsub(" \\(p\\)", "", colnames(data_wide))
  
  data_long <- data.table::melt(data_wide, id.vars = "age",
                                variable.name = "annee",
                                value.name = "deces")
  data_long[, c('sexe') := sex]
  data_long[, annee := as.numeric(as.character(annee))]
  
  # gere les cohortes avant 1962 et celles après 2070
  start <- min(data_long$annee, na.rm = TRUE)
  end <- max(data_long$annee, na.rm = TRUE)
  tweak_survival <- function(year, data_long, anchor){
    tmp = data.table::copy(data_long[annee == anchor])
    tmp[ , c('annee') := year]
    return(tmp)
  } 
  tempdf_before = data.table::rbindlist(
    lapply(1900:(start-1), tweak_survival, data_long = data_long, anchor = start)
  )
  tempdf_after = data.table::rbindlist(
    lapply((end+1):2120, tweak_survival, data_long = data_long, anchor = start)
  )
  
  data_long <- data.table::rbindlist(
    list(tempdf_before, data_long, tempdf_after)
  )
  
  data_long[, c('cohorte') := annee - as.numeric(age)]
  
  
  data_long[, c('total_deces') := cumsum(deces), by = c("cohorte")]
  data_long[, c('survivants') := sum(deces) - total_deces, by = c("cohorte")]
  data_long[, c('proba_deces') := deces/sum(deces), by = "cohorte"]
  data_long[, c('proba_survie') := 1 - cumsum(proba_deces), by = c("cohorte")]
  
  data_long[, .SD,.SDcols = c("age","annee","sexe","cohorte","proba_survie")]
  
  
  return(data_long)
}

create_mortality_table <- function(path_mortalite){
  mortality_women <- read_mortality_table_sex(path_mortalite)
  mortality_men <- read_mortality_table_sex(path_mortalite, "H")
  mortality <- data.table::rbindlist(
    list(mortality_men, mortality_women)
  )
  mortality[, 'sexe' := data.table::fifelse(sexe == "H", 1, 2)]
  mortality[,'age' := as.numeric(age)]
  return(mortality)
}


tweak_oglm <- function(inheritance_model){
  class(inheritance_model) <- c("oglm","oglmx")
  return(inheritance_model)
}

create_estim_data <- function(inheritance_data){
  
  estim_data <-data.table::copy(inheritance_data)#[get('income')>0]
  
  estim_data[,'MTHER' := as.numeric(as.character(MTHER))]
  estim_data[,'age' := get('AGE')]
  
  
  estim_data <- estim_data[get('age') < 80]
  estim_data <- estim_data[get('income')>0]
  
  
  estim_data[, c('N_heritiers') := .N, by = c("annee","IDENT","IDENTTRANS")]
  
  
  #data.table::fwrite(estim_data, "./modele-heritage/estimsample.csv")
  
  estim_data[is.na(get("MTHER")), c("MTHER") := 0]
  estim_data[, inherited := (MTHER != 0) ]
  estim_data$inherited <- factor(as.numeric(estim_data$inherited))
  estim_data <- estim_data[order(MTHER)]
  
  estim_data <- na.omit(estim_data, cols = c("inherited","tr_age","tr_agfinetu","SEXE", "lw", "MTHER"))
  
  return(estim_data)
}

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
                       year = 2015){
  
  
  data[, c('SEXE') := data.table::fifelse(get(sex_var)==1,
                                          'Male',
                                          'Female')]
  data[, c('tr_diplome') := cut(get(diploma_var), breaks = c(min(get(diploma_var)), 16,18,21,25, max(get(diploma_var))), include.lowest = TRUE)]
  
  if (year != 2015) return(data)  
  
  data[, c('decile_w') := cut(get(labor_income_var),
                              quantile(get(labor_income_var),
                                       probs= 0:10/10) + seq_along(0:10)*.Machine$double.eps,
                              labels = 1:10, include.lowest = TRUE
  )]
  data[, c('decile_y') := cut(get(total_income_var),
                              quantile(get(total_income_var),
                                       probs= 0:10/10)  + seq_along(0:10)*.Machine$double.eps,
                              labels = 1:10, include.lowest = TRUE
  )]
  
  return(data)
  
}

clean_data2 <- function(data, sex_var = "SEXE",
                        diploma_var = "findet",
                        labor_income_var = "revenu",
                        total_income_var = "Y",
                        statut_var = NULL,
                        year = 2015){
  
  
  data[, c('SEXE') := data.table::fifelse(get(sex_var)==1,
                                          'Male',
                                          'Female')]
  data[, c('tr_diplome') := cut(get(diploma_var), breaks = c(min(get(diploma_var)), 16,18,21,25, max(get(diploma_var))), include.lowest = TRUE)]
  
  if (year != 2015) return(data)
  
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


plot_K_age2 <- function(simulations, observed_data,
                        lang = c("eng","fr"),
                        has_ricardian = FALSE,
                        weight_observed_data = "POND",
                        wealth_var = "wealth",
                        trans = NULL,
                        trans_survey = NULL,
                        wealth_var_survey = 'PATRI_NET',
                        year_var = "annee",
                        age_var = "age",
                        age_var_survey = "AGE",
                        graduation_var = "findet",
                        observation_year = 2015,
                        start_year = 2009,
                        final_year = 2040,
                        method = "smooth"){
  
  lang <- match.arg(lang)
  
  simulations2 <- simulations[get(year_var) %between% c(start_year,final_year)]
  simulations2 <- simulations2[get(age_var)>get(graduation_var)]
  observed_data2 <- data.table::copy(observed_data)
  
  if (has_ricardian) simulations2 <- simulations2[!(non_ricardian)]
  
  if (!is.null(trans)){
    
    if (trans == "log"){
      simulations2[,c(wealth_var) := log(get(wealth_var))]
    }
    if (trans == "exp"){
      simulations2[,c(wealth_var) := exp(get(wealth_var))]
    }
    
    if (trans == "div_1000"){
      simulations2[, c(wealth_var) := get(wealth_var)/1000]
    }
  }  
  
  if (!is.null(trans_survey)){
    
    observed_data2 <- observed_data2[get(wealth_var_survey)>0]
    
    if (trans_survey == "log"){
      observed_data2[,c(wealth_var_survey) := log(get(wealth_var_survey))]
    }
    if (trans_survey == "exp"){
      observed_data2[,c(wealth_var_survey) := exp(get(wealth_var_survey))]
    }
    if (trans == "div_1000"){
      observed_data2[,c(wealth_var_survey) := get(wealth_var_survey)/1000]
    }
    
  }  
  
  if (method != "smooth"){
    simulations2 <- simulations2[, .("wealth" = median(get(wealth_var), na.rm = TRUE)),
                                 by = c(year_var, age_var)]
    simulations2[,'source' := "simulation"]
  }
  
  if (is.null(weight_observed_data)){
    observed_data2 <- observed_data2[,.("wealth" =  median(get(wealth_var_survey), na.rm = TRUE)),
                                     by = age_var_survey]
  } else{
    observed_data2 <- observed_data2[,.("wealth" =  Hmisc::wtd.quantile(get(wealth_var_survey), weights = get(weight_observed_data),
                                                                        na.rm = TRUE, probs = .5)),
                                     by = age_var_survey]
  }
  observed_data2[, c(year_var) := observation_year]
  observed_data2[,'source' := "survey"]
  data.table::setnames(observed_data2, old = age_var_survey, new = age_var)
  
  
  dataframes <- data.table::rbindlist(list(simulations2, observed_data2),
                                      use.names = TRUE, fill = TRUE)
  dataframes[,'size' := .5 + 0.5*as.numeric(get("source") == "survey")]
  
  dataframes <- split(dataframes, by = c("annee", "source"))
  dataframes <- data.table::rbindlist(
    lapply(dataframes, function(d) d[,predict := predict(loess(wealth~age, data = d))])
  )
  
  if (wealth_var == "wealth"){
    ylab <- ifelse(lang == "eng", 
                   "Simulated wealth \n(thousands euros)",
                   "Patrimoine simulé \n(en milliers d'euros)")
  } else{
    ylab <- ifelse(lang == "eng", 
                   "Simulated capital income \n(% total income)",
                   "Revenus du capital simulés \n(% du revenu total)")
  }
  if (lang == "eng"){
    labelsource <- c("Simulated", 'Observed (wealth survey 2015)')
    labelcurve <- "Wealth survey 2015"
    labelcurve2 <- "Simulation in 2015"
  } else{
    labelsource <-  c("Simulée", "Observée (patrimoine 2015)")
    labelcurve <- "Enquête patrimoine 2015"
    labelcurve2 <- "Simulation en 2015"
  }
  
  temp <- ggplot(dataframes[age %between% c(30,75) & source == "survey"], aes(x = age, y = wealth)) +
    geom_smooth(color = "red", linetype = "dashed",
                se = FALSE)
  temp2 <- ggplot(dataframes[age %between% c(30,75) & source != "survey"], aes(x = age, y = wealth)) +
    geom_smooth(color = "red", linetype = "dashed",
                se = FALSE)
  
  blues_fun <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(32,"RdYlGn"))
  
  p <- ggplot(dataframes[age %between% c(30,75) & source != "survey"],
              aes(x = age, y = wealth, color = factor(annee))) +
    geom_smooth(se = FALSE) 
  
  p <- p + scale_color_manual(values = blues_fun(32)) +
    geom_smooth(data = dataframes[age %between% c(30,75) & source == "survey"], aes(x = age, y = wealth), color = "black", linetype = "dashed",
                se = FALSE)
  

  p <- p + annotate("segment", x = 60, xend = 55,
                y = temp$data[age == 55]$predict - (max(temp$data$predict) - min(temp$data$predict))/2,
                yend = temp$data[age == 55]$predict,
             colour = "black", size = 1, arrow = arrow(length = unit(2, "mm"))) +
     annotate(geom = "text", x = 56, y = temp$data[age == 55]$predict - (max(temp$data$predict) - min(temp$data$predict))/1.9,
              label = labelcurve, hjust = "left", color = "black", size = 5)

  p <- p + annotate("segment", x = 60, xend = 55,
                    y = temp2$data[age == 55 & annee == 2015]$predict + (max(temp2$data[annee == 2015]$predict) - min(temp2$data[annee == 2015]$predict))/2,
                    yend = temp2$data[age == 55 & annee == 2015]$predict,
                    colour = "black", size = 1, arrow = arrow(length = unit(2, "mm"))) +
    annotate(geom = "text", x = 56, y = temp2$data[age == 55 & annee == 2015]$predict + (max(temp2$data[annee == 2015]$predict) - min(temp2$data[annee == 2015]$predict))/1.9,
             label = labelcurve2, hjust = "left", color = "black", size = 5)
  
  
  p <- p + ggplot2::labs(y = ylab, x = "Age") +
    theme_bw() +
    theme(text = element_text(size=20),
          axis.text=element_text(size=22),
          axis.title=element_text(size=28,face="bold")) +
    # ggplot2::scale_linetype_manual(breaks = c(FALSE,TRUE),
    #                                values=c("solid", "dashed"),
    #                                labels = labelsource)  +
    guides(color = "none", linetype = guide_legend(title="")) +
    theme(legend.position="bottom")
  
  return(p)
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



get_quantiles <- function(dt, yvar, pondvar = NULL, p = c(1:9, 9.5, 9.9)/10){
  
  if (is.null(pondvar)){
    x <- dt[,quantile(get(yvar), probs = p)]
  } else{
    x <- dt[,Hmisc::wtd.quantile(get(yvar), probs = p,
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


recompute_uc <- function(table){

table=simul$macro%>%select(annee,Prix,PlafondSS,SMIC,PointFP,SMPT,PIB,RevaloSPC,RevaloRG,RevaloFP,MinVieil1,MinVieil2,
                           Mincont1,Mincont2,SalValid)%>%
  mutate(Mincont2/lag(Mincont2)-1)


##################
#calcul du niveau de vie des retraités
#################
emploi=simul$emp%>%
  select(age,Id, statut)%>%
  left_join(simul$salairenet)

emploi=emploi%>%mutate(salaire=salaires_net)%>%
  select(-salaires_net)



test=select(filter(emploi,statut!=0),Id,age,salaire)%>% # Note : statut != 0 -> l'individu est encore présent
  rename(salaire_Id=salaire)%>%
  left_join((simul$ech)[,c('Id','anaiss')])%>%
  left_join(simul$fam%>%
              group_by(Id)%>%
              mutate(firstf=annee==first(annee),prem_obs=first(annee))%>%
              filter(firstf==T)%>%
              select(Id,prem_obs))%>%  # on ne garde que les les observations à partir de la première observation dans fam
  mutate(annee=age+anaiss)%>%
  filter(annee>=prem_obs)%>%
  select(-prem_obs,-anaiss)%>%
  left_join(simul$fam)%>%
  rename(matri_Id=matri)%>%
  mutate(conjoint=ifelse(matri_Id==3,0,conjoint)) #pour les veufs je met le conjoint à 0

test=test%>%  
  left_join((simul$ech)[,c('Id','anaiss')],by=c("enf1"="Id"))%>% # on verifie ensuite pour chaque enfant s'il est à charge ou non c'est à dire s'il a moins de 21 ans
  mutate(ageenf1=annee-anaiss,enf1=ifelse((annee-anaiss)%in%seq(0,21),enf1,0))%>% # et s'il n'a pas de revnu du travail
  left_join(select(emploi,-statut),by=c("enf1"="Id","ageenf1"="age"))%>%
  mutate(enf1=ifelse(salaire>0,0,enf1),salaireenf1=salaire)%>%
  select(-anaiss,-salaire)%>%
  left_join((simul$fam)[,c('Id','annee','matri')],by=c("enf1"="Id","annee"="annee"))%>%
  mutate(enf1=ifelse(matri==2,0,enf1),matrienf1=matri)%>%
  select(-matri)

test=test%>%
  left_join((simul$ech)[,c('Id','anaiss')],by=c("enf2"="Id"))%>%
  mutate(enf2=ifelse((annee-anaiss)%in%seq(0,21),enf2,0),ageenf2=annee-anaiss)%>%
  left_join(select(emploi,-statut),by=c("enf2"="Id","ageenf2"="age"))%>%
  mutate(enf2=ifelse(salaire>0,0,enf2),salaireenf2=salaire)%>%
  select(-anaiss,-salaire)%>%
  left_join((simul$fam)[,c('Id','annee','matri')],by=c("enf2"="Id","annee"="annee"))%>%
  mutate(enf2=ifelse(matri==2,0,enf2),matrienf2=matri)%>%
  select(-matri)
test=test%>%
  left_join((simul$ech)[,c('Id','anaiss')],by=c("enf3"="Id"))%>%
  mutate(enf3=ifelse((annee-anaiss)%in%seq(0,21),enf3,0),ageenf3=annee-anaiss)%>%
  left_join(select(emploi,-statut),by=c("enf3"="Id","ageenf3"="age"))%>%
  mutate(enf3=ifelse(salaire>0,0,enf3),salaireenf3=salaire)%>%
  select(-anaiss,-salaire)%>%
  left_join((simul$fam)[,c('Id','annee','matri')],by=c("enf3"="Id","annee"="annee"))%>%
  mutate(enf3=ifelse(matri==2,0,enf3),matrienf3=matri)%>%
  select(-matri)
test=test%>%
  left_join((simul$ech)[,c('Id','anaiss')],by=c("enf4"="Id"))%>%
  mutate(enf4=ifelse((annee-anaiss)%in%seq(0,21),enf4,0),ageenf4=annee-anaiss)%>%
  left_join(select(emploi,-statut),by=c("enf4"="Id","ageenf4"="age"))%>%
  mutate(enf4=ifelse(salaire>0,0,enf4),salaireenf4=salaire)%>%
  select(-anaiss,-salaire)%>%
  left_join((simul$fam)[,c('Id','annee','matri')],by=c("enf4"="Id","annee"="annee"))%>%
  mutate(enf4=ifelse(matri==2,0,enf4),matrienf4=matri)%>%
  select(-matri)
test=test%>%
  left_join((simul$ech)[,c('Id','anaiss')],by=c("enf5"="Id"))%>%
  mutate(enf5=ifelse((annee-anaiss)%in%seq(0,21),enf5,0),ageenf5=annee-anaiss)%>%
  left_join(select(emploi,-statut),by=c("enf5"="Id","ageenf5"="age"))%>%
  mutate(enf5=ifelse(salaire>0,0,enf5),salaireenf5=salaire)%>%
  select(-anaiss,-salaire)%>%
  left_join((simul$fam)[,c('Id','annee','matri')],by=c("enf5"="Id","annee"="annee"))%>%
  mutate(enf5=ifelse(matri==2,0,enf5),matrienf5=matri)%>%
  select(-matri)
test=test%>%
  left_join((simul$ech)[,c('Id','anaiss')],by=c("enf6"="Id"))%>%
  mutate(enf6=ifelse((annee-anaiss)%in%seq(0,21),enf6,0),ageenf6=annee-anaiss)%>%
  left_join(select(emploi,-statut),by=c("enf6"="Id","ageenf6"="age"))%>%
  mutate(enf6=ifelse(salaire>0,0,enf6),salaireenf6=ifelse(enf6!=0,salaire,0))%>%
  select(-anaiss,-salaire)%>%
  left_join((simul$fam)[,c('Id','annee','matri')],by=c("enf6"="Id","annee"="annee"))%>%
  mutate(enf6=ifelse(matri==2,0,enf6),matrienf6=matri)%>%
  select(-matri)
####################
# calcul du nombre d'UC pour les adultes et les jeunes indépendants
####################
nbre_uc_adultes=test%>%
  filter(!((age<=21)&(salaire_Id==0)&(matri_Id!=2)))%>%
  mutate(enf1=ifelse(is.na(enf1),0,enf1),enf2=ifelse(is.na(enf2),0,enf2),enf3=ifelse(is.na(enf3),0,enf3),
         enf4=ifelse(is.na(enf4),0,enf4),enf5=ifelse(is.na(enf5),0,enf5),enf6=ifelse(is.na(enf6),0,enf6))%>%
  mutate(nbre_uc=ifelse(conjoint>0,0.75,1)+ifelse(enf1>0&ageenf1%in%15:21,0.25,0)+ifelse(enf2>0&ageenf2%in%15:21,0.25,0)+
           ifelse(enf3>0&ageenf3%in%15:21,0.25,0)+ifelse(enf4>0&ageenf4%in%15:21,0.25,0)+ifelse(enf5>0&ageenf5%in%15:21,0.25,0)+
           +ifelse(enf6>0&ageenf6%in%15:21,0.25,0)+ifelse(enf1>0&ageenf1%in%0:14,0.15,0)+ifelse(enf2>0&ageenf2%in%0:14,0.15,0)+
           ifelse(enf3>0&ageenf3%in%0:14,0.15,0)+ifelse(enf4>0&ageenf4%in%0:14,0.15,0)+ifelse(enf5>0&ageenf5%in%0:14,0.15,0)+
           +ifelse(enf6>0&ageenf6%in%0:14,0.15,0))%>%
  select(Id,annee,age,matri_Id,conjoint,nbre_uc)

summary(nbre_uc_adultes$nbre_uc)

############################
# calcul du nombre d'uc pour les jeunes
# on ajoute les uc entre les parents comme on ajoutera les revenus 
###############################

nbre_uc_jeunes=test%>%
  filter(age<=21&salaire_Id==0&matri_Id!=2)%>%
  select(age,Id,annee,mere,pere,matri_Id)%>%
  left_join(select(nbre_uc_adultes,Id,annee,nbre_uc),by=c("mere"="Id","annee"="annee"))%>%
  rename(nbre_uc_mere=nbre_uc)%>%
  left_join(select(nbre_uc_adultes,Id,annee,nbre_uc),by=c("pere"="Id","annee"="annee"))%>%
  rename(nbre_uc_pere=nbre_uc)%>%
  mutate(nbre_uc=ifelse(!is.na(nbre_uc_pere)&!is.na(nbre_uc_mere),(nbre_uc_pere+nbre_uc_mere),
                        ifelse(!is.na(nbre_uc_pere),nbre_uc_pere,ifelse(!is.na(nbre_uc_mere),nbre_uc_mere,1))))


nbre_uc=bind_rows(nbre_uc_jeunes,nbre_uc_adultes) 

return(nbre_uc)
}
