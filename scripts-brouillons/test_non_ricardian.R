rm(list = ls())

# _____________________________________________
# PART 1: CONSTRUCT LONGITUDINAL PANEL ----
# _____________________________________________

# ++++++++++++++++++++++++++++++++++++++++++
# A/ READ HOUSEHOLD LEVEL DATA ========
# ++++++++++++++++++++++++++++++++++++++++++


population <- readRDS(file = "tempfile.rds")
data <- readRDS("data.rds")
EP_2015 <- data[['EP_2015']]
EP_2018 <- data[['EP_2018']]
EP_lon <- data[['EP_lon']]


output <- readRDS("output.rds")

#beta  <- 0.975
#gamma <- 0.72
r <- 0.03
beta <- 0.9855748
gamma <- 0.7108069





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

# predict -------

data <- readRDS("./data.rds")


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
  non_ricardian = FALSE,
  observation_year = 2009,
  income_var = "revenu",
  Hgiven_var = "H_given",
  Hreceived_var = "H_received",
  return_last = FALSE,
  get_capital_income = TRUE,
  additional_vars = c("tr_age","SEXE","tr_agfinetu","findet"))


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


plot_K_age2 <- function(simulations, observed_data,
                        weight_observed_data = "POND",
                        wealth_var = "wealth",
                        wealth_var_survey = 'PATRI_NET',
                        year_var = "annee",
                        age_var = "age",
                        age_var_survey = "AGE",
                        graduation_var = "findet",
                        observation_year = 2015,
                        start_year = 2009,
                        final_year = 2040){
  
  
  simulations2 <- simulations[get(year_var) %between% c(start_year,final_year)]
  simulations2 <- simulations2[get(age_var)>get(graduation_var)]
  
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
  
  
  ggplot(dataframes[age %between% c(30,75) & source == "survey"]) + geom_line(aes(x = age, y = wealth/1000, color = factor(annee), size = size))

  ggplot(dataframes[age %between% c(30,75)]) + geom_smooth(aes(x = age, y = wealth/1000, color = factor(annee), size = size), se = FALSE)
  
  
}
  


capitulation::plot_K_age(simulations[age>findet], xlims = c(30,75),
                         method = "median")


# AVEC RICARDIENS ------

simulations2 <- capitulation::life_cycle_model(
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
  return_last = FALSE,
  get_capital_income = TRUE,
  additional_vars = c("tr_age","SEXE","tr_agfinetu","findet"))

capitulation::plot_K_age(simulations2[age>findet], xlims = c(30,75),
                         method = "median")

simulations3 <- capitulation::life_cycle_model(
  population,
  wealthvar_survey = "K_observed",
  scale_model = "log",
  r = r,
  beta = beta,
  gamma = gamma,
  non_ricardian = TRUE,
  observation_year = 2009,
  income_var = "revenu",
  Hgiven_var = "hg",
  Hreceived_var = "hr",
  return_last = FALSE,
  get_capital_income = TRUE,
  additional_vars = c("tr_age","SEXE","tr_agfinetu","findet"))

simulations3[wealth != 0, 'wealth' := exp(wealth)]
capitulation::plot_K_age(simulations3[age>findet], xlims = c(30,75),
                         method = "smooth")





# share non ricardian -----

xx <- simulations2[,mean(wealth == 0), by = c('annee','age')]
ggplot(xx[annee %between% c(2015,2035)]) + geom_smooth(aes(x = age, y = as.numeric(V1), color = factor(annee)), se = FALSE)



