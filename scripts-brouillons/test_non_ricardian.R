rm(list = ls())

# _____________________________________________
# PART 1: CONSTRUCT LONGITUDINAL PANEL ----
# _____________________________________________


library(tablelight)

source("functions.R")


EP_data <- wealthyR:::prepare_inheritance_sample(
  path_survey =  "../Enquete Patrimoine"
)
EP_data <- EP_data[(MER1E == 3 & PER1E==3) | (!is.na(MTHER))]


inheritance_data <- REtage::prepare_estimation(EP_data)


bounds <- c(3,8,15,30,60,100,150,200,250)*1000
lbounds <- log(bounds)




estim_data <-data.table::copy(inheritance_data)#[get('income')>0]

estim_data[,'MTHER' := as.numeric(as.character(MTHER))]
estim_data[,'age' := get('AGE')]


estim_data <- estim_data[get('age') < 80]
estim_data <- estim_data[get('income')>0]


estim_data[, c('N_heritiers') := .N, by = c("annee","IDENT","IDENTTRANS")]


data.table::fwrite(estim_data, "./modele-heritage/estimsample.csv")

estim_data[is.na(get("MTHER")), c("MTHER") := 0]
estim_data[, inherited := (MTHER != 0) ]
estim_data$inherited <- factor(as.numeric(estim_data$inherited))
estim_data <- estim_data[order(MTHER)]

estim_data <- na.omit(estim_data, cols = c("inherited","tr_age","tr_agfinetu","SEXE", "lw", "MTHER"))



# SELECTION MODEL -----------------


probit <- glm(inherited ~ factor(tr_age) + factor(tr_agfinetu),
              family = binomial(link = "probit"), 
              data = estim_data)
summary(probit)

pred_selection <- predict(probit, type = "link")
pred_selection <- pred_selection + rnorm(length(pred_selection), sd = sd(probit$residuals))
pred_selection <- as.numeric(pred_selection > 0)


confusion_first_step <- data.frame(
  prediction = pred_selection,
  observation = as.numeric(estim_data$inherited)
)
table(confusion_first_step)


# MODEL WITHOUT SELECTION --------


inheritance_model <-  oglm::oglmx(
  data = data.frame(estim_data[MTHER>0]),
  link = "probit",
  formulaMEAN = "MTHER ~ factor(SEXE) + lw",
  constantSD = TRUE,
  threshparam = lbounds
)
class(inheritance_model) <- c("oglm","oglmx")



path_data <- ".."


data <- construct_EP(path_data)


EP_2015 <- data[['EP_2015']]
EP_2018 <- data[['EP_2018']]
EP_lon <- data[['EP_lon']]



data_prediction <- capitulation::prepare_data(
  path_data = "..",
  inheritance_model = inheritance_model,
  selection_model = probit,
  time_0 = "birth",
  # debt_wealthSurvey = "MTDETTES",
  taille_tr_age = 5,
  taille_tr_agfinetu = 2,
  path_data_suffix = "/Destinie2120", 
  extension = ".rda",
  wealthvar_survey = "PATRI_NET"
)

data_prediction[is.na(non_ricardian), c("non_ricardian") := get("H_given")>0]

mean(data_prediction[,.SD[1], by = "Id"]$non_ricardian)

# ++++++++++++++++++++++++++++++++++++++++++
# A/ READ HOUSEHOLD LEVEL DATA ========
# ++++++++++++++++++++++++++++++++++++++++++


population <- data.table::copy(data_prediction)

population

#output <- readRDS("output.rds")

#beta  <- 0.975
#gamma <- 0.72
r <- 0.03
beta <- 0.9855748
gamma <- 2




library(tablelight)

source("functions.R")


# EP_data <- wealthyR:::prepare_inheritance_sample(
#   path_survey =  "../Enquete Patrimoine"
# )
# EP_data <- EP_data[(MER1E == 3 & PER1E==3) | (!is.na(MTHER))]
# 
# 
# inheritance_data <- REtage::prepare_estimation(EP_data)
# 
# 
# bounds <- c(3,8,15,30,60,100,150,200,250)*1000
# lbounds <- log(bounds)
# 
# 
# 
# 
# estim_data <-data.table::copy(inheritance_data)#[get('income')>0]
# 
# estim_data[,'MTHER' := as.numeric(as.character(MTHER))]
# estim_data[,'age' := get('AGE')]
# 
# 
# estim_data <- estim_data[get('age') < 80]
# estim_data <- estim_data[get('income')>0]
# 
# 
# estim_data[, c('N_heritiers') := .N, by = c("annee","IDENT","IDENTTRANS")]
# 
# 
# data.table::fwrite(estim_data, "./modele-heritage/estimsample.csv")
# 
# estim_data[is.na(get("MTHER")), c("MTHER") := 0]
# estim_data[, inherited := (MTHER != 0) ]
# estim_data$inherited <- factor(as.numeric(estim_data$inherited))
# estim_data <- estim_data[order(MTHER)]
# 
# estim_data <- na.omit(estim_data, cols = c("inherited","tr_age","tr_agfinetu","SEXE", "lw", "MTHER"))
# 
# 
# 
# # SELECTION MODEL -----------------
# 
# 
# probit <- glm(inherited ~ factor(tr_age) + factor(tr_agfinetu),
#               family = binomial(link = "probit"), 
#               data = estim_data)
# summary(probit)
# 
# pred_selection <- predict(probit, type = "link")
# pred_selection <- pred_selection + rnorm(length(pred_selection), sd = sd(probit$residuals))
# pred_selection <- as.numeric(pred_selection > 0)
# 
# 
# confusion_first_step <- data.frame(
#   prediction = pred_selection,
#   observation = as.numeric(estim_data$inherited)
# )
# table(confusion_first_step)
# 
# 
# # MODEL WITHOUT SELECTION --------
# 
# 
# inheritance_model <-  oglm::oglmx(
#   data = data.frame(estim_data[MTHER>0]),
#   link = "probit",
#   formulaMEAN = "MTHER ~ factor(SEXE) + lw",
#   constantSD = TRUE,
#   threshparam = lbounds
# )
# class(inheritance_model) <- c("oglm","oglmx")
# 
# # predict -------
# 
# data <- readRDS("./data.rds")
# 
# 
# data_prediction <- capitulation::prepare_data(
#   path_data = "..",
#   inheritance_model = inheritance_model,
#   selection_model = probit,
#   time_0 = "birth",
#   # debt_wealthSurvey = "MTDETTES",
#   taille_tr_age = 5,
#   taille_tr_agfinetu = 2,
#   path_data_suffix = "/Destinie2120", 
#   extension = ".rda",
#   wealthvar_survey = "PATRI_NET"
# )
# 
# 





data = data.table::copy(population)


# _____________________________________________
# PART 2: ESTIMATION ----
# _____________________________________________

population <- data.table::copy(data_prediction)


# SIMULATE MODEL -----------------------------

simulations <- capitulation::life_cycle_model(
  population,
  wealthvar_survey = "K_observed",
  r = r,
  beta = beta,
  gamma = gamma,
  non_ricardian = TRUE,
  observation_year = 2009,
  income_var = "revenu",
  Hgiven_var = "H_given",
  Hreceived_var = "H_received",
  non_ricardian_var = "non_ricardian",
  scale_model = "level",
  return_last = FALSE,
  get_capital_income = TRUE,
  additional_vars = c("tr_age","SEXE","tr_agfinetu","findet", "non_ricardian"))

simulations[,'tr_age_2015' := tr_age]

mean(simulations[, .SD[1], by = "Id"]$non_ricardian)
mean(simulations[, .SD[1], by = "Id"]$wealth == 0)


# ARRANGE DATA ---------------------------------

clean_data <- function(data, sex_var = "SEXE",
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

simulations <- simulations[age > findet]

clean_data(simulations)

EP_2015[,'y' := get('labor_income') + r*get('PATFISOM')]
EP_2015[, 'annee' := 2015]
clean_data(EP_2015, sex_var = "SEXE", labor_income_var = "labor_income", diploma_var = "AGFINETU",
           total_income_var = "y")

EP_2018[, 'annee' := 2018]
clean_data(EP_2018, sex_var = "SEXE", year = 2018, diploma_var = "AGFINETU",
           statut_var = "SITUA")


EP_lon[,'y' := get('labor_income_2015') + r*get('PATFISOM_2015')]
EP_lon <- merge(EP_lon, EP_2015[,.SD,.SDcols = c("IDENTIND14","tr_diplome", "decile_w", "decile_y")],
                by = c("IDENTIND14"))
EP_lon[, c('SEXE') := data.table::fifelse(get('SEXE')==1,
                                          'Male',
                                          'Female')]




plot_K_age2 <- function(simulations, observed_data, has_ricardian = FALSE,
                        weight_observed_data = "POND",
                        wealth_var = "wealth",
                        wealth_var_survey = 'PATRI_NET',
                        year_var = "annee",
                        age_var = "age",
                        age_var_survey = "AGE",
                        graduation_var = "findet",
                        observation_year = 2015,
                        start_year = 2009,
                        final_year = 2040,
                        method = "smooth"){
  
  
  simulations2 <- simulations[get(year_var) %between% c(start_year,final_year)]
  simulations2 <- simulations2[get(age_var)>get(graduation_var)]
  
  if (has_ricardian) simulations2 <- simulations2[!(non_ricardian)]
  
  if (method != "smooth"){
  simulations2 <- simulations2[, .("wealth" = median(get(wealth_var), na.rm = TRUE)),
                               by = c(year_var, age_var)]
  simulations2[,'source' := "simulation"]
  }
  
  if (is.null(weight_observed_data)){
    observed_data2 <- observed_data[,.("wealth" =  median(get(wealth_var_survey), na.rm = TRUE)),
                                    by = age_var_survey]
  } else{
    observed_data2 <- observed_data[,.("wealth" =  Hmisc::wtd.quantile(get(wealth_var_survey), weights = get(weight_observed_data),
                                                                       na.rm = TRUE, probs = .5)),
                                    by = age_var_survey]
  }
  observed_data2[, c(year_var) := observation_year]
  observed_data2[,'source' := "survey"]
  data.table::setnames(observed_data2, old = age_var_survey, new = age_var)
  
  
  dataframes <- data.table::rbindlist(list(simulations2, observed_data2),
                                      use.names = TRUE, fill = TRUE)
  dataframes[,'size' := .5 + 0.5*as.numeric(get("source") == "survey")]
  
  
  ggplot(dataframes[age %between% c(30,75)]) + geom_line(aes(x = age, y = wealth/1000, color = factor(annee), size = size))

  ggplot(dataframes[age %between% c(30,75)]) + geom_smooth(aes(x = age, y = wealth/1000, color = factor(annee), linetype = factor(source == "survey")), se = FALSE)
  
  
}


mean(simulations[, .SD[1], by = "Id"]$non_ricardian)



library(data.table)
library(ggplot2)

capitulation::plot_K_age(simulations[age>findet], xlims = c(30,75),
                         method = "median")
kage1 = plot_K_age2(simulations2[age>findet],EP_2015, method = "median", has_ricardian = TRUE)
kage2 = plot_K_age2(simulations2[age>findet],EP_2015, method = "median", has_ricardian = FALSE)

# NOUVELLE ESTIMATION ------


simulations2 <- capitulation::life_cycle_model(
  population,
  wealthvar_survey = "K_observed",
  r = r,
  beta = 1.040049,
  gamma = gamma,
  non_ricardian = TRUE,
  observation_year = 2009,
  income_var = "revenu",
  Hgiven_var = "H_given",
  Hreceived_var = "H_received",
  non_ricardian_var = "non_ricardian",
  scale_model = "level",
  return_last = FALSE,
  get_capital_income = TRUE,
  additional_vars = c("tr_age","SEXE","tr_agfinetu","findet", "non_ricardian"))

simulations2[,'tr_age_2015' := tr_age]


kage1 = plot_K_age2(simulations2[age>findet],EP_2015, method = "median", has_ricardian = TRUE)
kage2 = plot_K_age2(simulations2[age>findet],EP_2015, method = "median", has_ricardian = FALSE)

ggplot2::ggsave(plot = kage1, filename = "./output_ret_soc_v2/fig01_kage1.pdf",
                width = 13, height = 9)
ggplot2::ggsave(plot = kage2, filename = "./output_ret_soc_v2/fig01_kage2.pdf",
                width = 13, height = 9)

# simulations2 <- capitulation::life_cycle_model(
#   population,
#   wealthvar_survey = "K_observed",
#   r = r,
#   beta = beta,
#   gamma = gamma,
#   non_ricardian = TRUE,
#   observation_year = 2009,
#   income_var = "revenu",
#   Hgiven_var = "H_given",
#   Hreceived_var = "H_received",
#   return_last = FALSE,
#   get_capital_income = TRUE,
#   additional_vars = c("tr_age","SEXE","tr_agfinetu","findet"))
# 
# capitulation::plot_K_age(simulations2[age>findet], xlims = c(30,75),
#                          method = "median")
# 
# simulations3 <- capitulation::life_cycle_model(
#   population,
#   wealthvar_survey = "K_observed",
#   scale_model = "log",
#   r = r,
#   beta = beta,
#   gamma = gamma,
#   non_ricardian = TRUE,
#   observation_year = 2009,
#   income_var = "revenu",
#   Hgiven_var = "hg",
#   Hreceived_var = "hr",
#   return_last = FALSE,
#   get_capital_income = TRUE,
#   additional_vars = c("tr_age","SEXE","tr_agfinetu","findet"))
# 
# simulations3[wealth != 0, 'wealth' := exp(wealth)]
# capitulation::plot_K_age(simulations3[age>findet], xlims = c(30,75),
#                          method = "smooth")
# 




# share non ricardian -----
library(ggplot2)
library(data.table)

xx <- simulations[,mean(wealth == 0), by = c('annee','age')]
ggplot(xx[annee %between% c(2015,2035)]) + geom_smooth(aes(x = age, y = as.numeric(V1), color = factor(annee)), se = FALSE) +
  xlim(35,75)



# Figure 1: K by age -------------------

p_K <- capitulation::plot_K_age(simulations[wealth != 0], method = "median",
                                xlims = c(30,75))

p_K <- p_K +
  #ggplot2::scale_y_continuous(limits = c(-100000, 150000)) +
  #ggplot2::scale_x_continuous(limits = c(25, 90)) +
  ggplot2::scale_color_viridis_d() +
  ggplot2::labs(y = "Richesse simulée (euros)") +
  theme(text = element_text(size=20),
        axis.text=element_text(size=28))

ggplot2::ggsave(plot = p_K, filename = "./output_ret_soc_v2/fig02_calibration.pdf",
                width = 13, height = 9)
ggplot2::ggsave(plot = p_K, filename = "./output_ret_soc_v2/fig02_calibration.png",
                width = 13, height = 9)


dat <- layer_data(p_K, 1)
data.table::setDT(dat)

dat <- dat[,.SD, .SDcols = c("size","colour","x","y","group")]
dat[,'group' := get('group') + 2008]

data.table::fwrite(dat, file = "./output_ret_soc_v2/fig02_data.csv")



population[,'hg' := get('H_given')]
population[,'hr' := get('H_received')]
population[,'tr_age_2015' := floor(get("age")/5)*5]
EP_2015[,'tr_age_2015' := floor(get("AGE")/5)*5]

population[, 'AGE' := get('age')]

mean(population[,.SD[1], by = "Id"]$non_ricardian)


df_moment2 <- wealthyR:::create_moment_data(EP_2015 = EP_2015, EP_2018 = EP_2018,
                                EP_lon = EP_lon, 
                                data_microsimulated = population,
                                observed_moment_data = NULL,
                                r = r,
                                gamma = gamma,
                                beta = 1.040049,
                                r.parameters = NULL,
                                gamma.parameters = NULL,
                                beta.parameters = NULL,
                                r_low = r,
                                r_high = r,
                                non_ricardian = TRUE,
                                non_ricardian_var = "non_ricardian",
                                N_moments = 180,
                                wealth_var = "PATRI_NET",
                                age_var_simulations = "age",
                                normalize = FALSE,
                                scale_model = "level",
                                scale_variable_moment1 = "log",
                                scale_variable_moment2 = "log",
                                scale_moment1 = "level",                               
                                moment1 = "level",
                                stat_moment2 = "difference",
                                ages = c(30,65),
                                exclude_negative = FALSE,
                                Hgiven_var = "hg",
                                Hreceived_var = "hr",
                                additional_vars = c("tr_age","SEXE","tr_agfinetu","findet", "non_ricardian"),
                                by = c("tr_age_2015", "tr_age_2015"))



plot_moment_age_wide <-function(df_moment2, ages = c(30,65)){
  
  tempdat <- data.table::copy(df_moment2)
  tempdat[, 'cumsum' := cumsum(weight)]
  tempdat[, 'moment' := 1 + as.numeric(cumsum>1)]
  tempdat[, c("cumsum") := NULL]
  
  tempdat <- split(tempdat, by = "moment")
  tempdat <- lapply(tempdat, data.table::melt, id.vars = c("Nmoment", "moment"))
  
  
  m1 <- ggplot2::ggplot(tempdat[["1"]][variable != "weight"]) +
    ggplot2::geom_bar(ggplot2::aes(x = Nmoment, y = value,
                                   fill = variable), position = "dodge",
                      stat='identity', width=.5) +
    ggplot2::labs(x = "Moment", y = "Median wealth") +
    ggplot2::scale_fill_manual(values = c('moment_simulations' = 'black',
                                        'moment_data' = 'royalblue'),
                             labels = c("Simulée","Observée")) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse=TRUE)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "top") +
    ggplot2::labs(y = "Part du patrimoine de la\nclasse d'âge dans le total",
                  fill = NULL) +
    ggplot2::theme(text = ggplot2::element_text(size=28),
          axis.text=ggplot2::element_text(size=28))
  
  m1b <- ggplot2::ggplot(tempdat[["1"]][variable == "weight"]) +
    ggplot2::geom_bar(ggplot2::aes(x = Nmoment, y = value), stat = 'identity') +
    ggplot2::geom_point(ggplot2::aes(x = Nmoment, y = value), color = 'red') +
    ggplot2::labs(y = "Density", x = "Moment") +
    ggplot2::scale_y_reverse()
  
  
  m2 <- ggplot2::ggplot(tempdat[["2"]][variable != "weight"], ggplot2::aes(x = Nmoment - 13,
                                                y = value,
                                                color = variable,
                                                shape = variable,
                                                group = variable)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0L) +
    ggplot2::labs(title = "3-year rolling growth rate",
                  x = 'Age in wave 1',
                  y = "Moment") +
    ggplot2::scale_color_manual(values = c('moment_simulations' = 'black',
                                           'moment_data' = 'royalblue'),
                                labels = c("Simulée","Observée")) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(y = "Evolution sur trois ans\ndu patrimoine financier",
                  x = "Age en 2015", title = "") +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::theme(text = ggplot2::element_text(size=28),
          axis.text=ggplot2::element_text(size=28)) +
    ggplot2::theme(legend.title=ggplot2::element_blank())

  m2b <- ggplot2::ggplot(tempdat[["2"]][variable == "weight"]) +
    ggplot2::geom_bar(ggplot2::aes(x = Nmoment - 13, y = value), stat = 'identity') +
    ggplot2::geom_point(ggplot2::aes(x = Nmoment - 13, y = value), color = 'red') +
    ggplot2::labs(y = "Density", x = "Moment") +
    ggplot2::scale_y_reverse()
  
  return(
    list(m1, m1b, m2, m2b)
  )
   
}

plots <- plot_moment_age_wide(df_moment2)
moment1 <- gridExtra::grid.arrange(plots[[1]], plots[[2]])
moment2 <- gridExtra::grid.arrange(plots[[3]], plots[[4]])

ggplot2::ggsave(plot = moment1, "./output_ret_soc_v2/fig03_moment1.pdf",
                width = 18, height = 20)
ggplot2::ggsave(plot = moment2, "./output_ret_soc_v2/fig03_moment2.pdf",
                width = 18, height = 20)

# OLD

# Figure 2: MOMENT 1 =============================

EP_2015[,'tr_age_2015' := floor(get("AGE")/5)*5]
EP_2018[,'tr_age_2015' := floor(get("AGE")/5)*5]

p1 <- wealthyR::plot_moment_age(EP_2015, EP_2018, simulations = simulations,
                                scale_moment_share = "share",
                                by_survey = "tr_age_2015", by_simulation = 'tr_age_2015',
                                scale_variable  = "log")$fit[[1]] +
  ggplot2::scale_fill_manual(values = c('microsimulation' = 'black',
                                        'survey' = 'royalblue'),
                             labels = c("Simulée","Observée")) +
  ggplot2::guides(fill = ggplot2::guide_legend(reverse=TRUE)) +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "top") +
  ggplot2::labs(y = "Part du patrimoine de la\nclasse d'âge dans le total",
                fill = NULL) +
  theme(text = element_text(size=28),
        axis.text=element_text(size=28))


p1b <- wealthyR::plot_moment_age(EP_2015, EP_2018, simulations = simulations2,
                                scale_moment_share = "share",
                                by_survey = "tr_age_2015", by_simulation = 'tr_age_2015',
                                scale_variable  = "log")$fit[[1]] +
  ggplot2::scale_fill_manual(values = c('microsimulation' = 'black',
                                        'survey' = 'royalblue'),
                             labels = c("Simulée","Observée")) +
  ggplot2::guides(fill = ggplot2::guide_legend(reverse=TRUE)) +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "top") +
  ggplot2::labs(y = "Part du patrimoine de la\nclasse d'âge dans le total",
                fill = NULL) +
  theme(text = element_text(size=28),
        axis.text=element_text(size=28))

p2 <- wealthyR::plot_moment_age(EP_2015, EP_2018, simulations = simulations,
                                by_survey = "AGEPR",
                                by_simulation = 'age',
                                scale_variable = "log")$fit[[2]]  +
  ggplot2::scale_y_continuous(labels = scales::percent, trans = "reverse") +
  ggplot2::labs(y = "Part de la population observée")  +
  theme(text = element_text(size=28),
        axis.text=element_text(size=28))

moment1 <- gridExtra::grid.arrange(
  p1,
  p2
)

ggplot2::ggsave(plot = moment1, "./output_ret_soc_v2/fig03_moment1.pdf",
                width = 18, height = 20)
data_1 <- data.table::copy(p1$data)
data_2 <- data.table::copy(p2$data)
data_1[ , source := data.table::fifelse(get("source") == "survey",
                                        "Observée","Simulée")]
data_2[ , source := data.table::fifelse(get("source") == "survey",
                                        "Observée","Simulée")]
data_1[,weight := NULL]
data_2[,weight := NULL]
data.table::fwrite(data_1, file = "./output_ret_soc/fig03a_data.csv")
data.table::fwrite(data_2, file = "./output_ret_soc/fig03b_data.csv")



dat1 = wealthyR::plot_moment_age(EP_2015, EP_2018, simulations = simulations,
                          scale_moment_share = "share",
                          by_survey = "AGE", by_simulation = 'age',
                          scale_variable  = "level", plot = FALSE)
dat2 = wealthyR::plot_moment_age(EP_2015, EP_2018, simulations = simulations2,
                                 scale_moment_share = "log",
                                 by_survey = "AGE", by_simulation = 'age',
                                 scale_moment = "level", scale_variable = "log", plot = FALSE)



# Figure 3: MOMENT 2 ===============================

moment2 <- wealthyR::plot_moment_dK(
  EP_lon = EP_lon, simulations = simulations,
  scale = "log", by = "tr_age_2015",
  label_observed = "Observée", label_simulated = "Simulée"
) +
  ggplot2::scale_color_manual(values = c('Simulée' = 'black',
                                         'Observée' = 'royalblue')) +
  # ggplot2::guides(color = ggplot2::guide_legend(reverse=TRUE)) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::labs(y = "Evolution sur trois ans\ndu patrimoine financier",
                x = "Age en 2015", title = "") +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=28),
        axis.text=element_text(size=28)) +
  theme(legend.title=element_blank())


data_1 <- data.table::copy(moment2$data)
data_1[,"weight" := NULL]

ggplot2::ggsave(plot = moment2, "./output_ret_soc_v2/fig04_moment2.pdf",
                width = 12, height = 8)
ggplot2::ggsave(plot = moment2, "./output_ret_soc_v2/fig04_moment2.png",
                width = 12, height = 8)


EP_2018[, 'net_fin_wealth' := get('PATFISOM') - get('MTDETTES')]
EP_2015[, 'net_fin_wealth' := get('PATFISOM') - get('MTDETTES')]


df <- data.table::data.table(
  "Actifs financiers (enquete patrimoine, individualisée)" =
    get_quantiles(EP_2018, 'PATFISOM', "POND_TRANS"),
  "Actifs - passifs financiers (enquete patrimoine, individualisée)" =
    get_quantiles(EP_2018, 'net_fin_wealth', "POND_TRANS"),
  "Modèle simulé (r = 3%)" = as.numeric(
    simulations[, round(quantile(get('wealth'), probs = c(1:9, 9.5, 9.9)/10))]
  ),
  "q" = 100*c(1:9, 9.5, 9.9)/10
)
df <- data.table::melt(df, id.vars = "q")

dat2 = wealthyR::plot_moment_dK(
  EP_lon = EP_lon, simulations = simulations2,
  scale = "log", by = "tr_age_2015",
  label_observed = "Observée", label_simulated = "Simulée",
  plot = FALSE
)
dat2[order(label, moment)]
# COMPARAISON DES MODELES -------

p1a <- wealthyR::plot_moment_age(EP_2015, EP_2018, simulations = simulations,
                          scale_moment_share = "share",
                          by_survey = "AGE", by_simulation = 'age',
                          scale_variable  = "log")$fit[[1]] +
  ggplot2::scale_fill_manual(values = c('microsimulation' = 'black',
                                        'survey' = 'royalblue'),
                             labels = c("Simulée","Observée")) +
  ggplot2::guides(fill = ggplot2::guide_legend(reverse=TRUE)) +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "top") +
  ggplot2::labs(y = "Part du patrimoine de la\nclasse d'âge dans le total",
                fill = NULL) +
  theme(text = element_text(size=28),
        axis.text=element_text(size=28))
p1b <- wealthyR::plot_moment_age(EP_2015, EP_2018, simulations = simulations2,
                                 scale_moment_share = "share",
                                 by_survey = "AGE", by_simulation = 'age',
                                 scale_variable  = "log")$fit[[1]] +
  ggplot2::scale_fill_manual(values = c('microsimulation' = 'black',
                                        'survey' = 'royalblue'),
                             labels = c("Simulée","Observée")) +
  ggplot2::guides(fill = ggplot2::guide_legend(reverse=TRUE)) +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "top") +
  ggplot2::labs(y = "Part du patrimoine de la\nclasse d'âge dans le total",
                fill = NULL) +
  theme(text = element_text(size=28),
        axis.text=element_text(size=28))


p2a <- wealthyR::plot_moment_dK(
  EP_lon = EP_lon, simulations = simulations,
  scale = "log", by = "tr_age_2015",
  label_observed = "Observée", label_simulated = "Simulée"
) +
  ggplot2::scale_color_manual(values = c('Simulée' = 'black',
                                         'Observée' = 'royalblue')) +
  # ggplot2::guides(color = ggplot2::guide_legend(reverse=TRUE)) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::labs(y = "Evolution sur trois ans\ndu patrimoine financier",
                x = "Age en 2015", title = "") +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=28),
        axis.text=element_text(size=28)) +
  theme(legend.title=element_blank())

p2b <- wealthyR::plot_moment_dK(
  EP_lon = EP_lon, simulations = simulations2,
  scale = "log", by = "tr_age_2015",
  label_observed = "Observée", label_simulated = "Simulée"
) +
  ggplot2::scale_color_manual(values = c('Simulée' = 'black',
                                         'Observée' = 'royalblue')) +
  # ggplot2::guides(color = ggplot2::guide_legend(reverse=TRUE)) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::labs(y = "Evolution sur trois ans\ndu patrimoine financier",
                x = "Age en 2015", title = "") +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=28),
        axis.text=element_text(size=28)) +
  theme(legend.title=element_blank())


gridExtra::grid.arrange(p1a, p1b, nrow = 2)

gridExtra::grid.arrange(p2a, p2b, nrow = 2)
