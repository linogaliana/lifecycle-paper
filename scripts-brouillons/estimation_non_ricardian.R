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

estim_data <- na.omit(estim_data, cols = c("inherited","tr_age","AGE","tr_agfinetu","SEXE", "lw", "MTHER"))



# SELECTION MODEL -----------------


probit <- glm(inherited ~ AGE + I((AGE^2)/100) + factor(tr_agfinetu),
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
  formulaMEAN = "MTHER ~ factor(SEXE) + lw + AGE + I((AGE^2)/100) + factor(tr_agfinetu)",
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


number_moments <- 2L
scale_wealth <- "log"
select_moments <- NULL
estimation_method <- "two_step"
parameters_estimation <- list("number_moments" = number_moments,
                              "scale_wealth" = scale_wealth,
                              "select_moments" = select_moments,
                              "method" = estimation_method)

#r <- 0.03
#beta <- 0.9855748
#gamma <- 2





library(tablelight)

source("functions.R")


beta <- NULL
r <- 0.03
gamma <- 2

# beta_0 <- runif(1, min = 0.5, max = 1.5)
beta_0 <- 0.7
# gamma_0 <- runif(1, min = 0.2, max = 5)
#gamma_0 <- 1.5

menages_structural2 <- data.table::copy(data_prediction)
menages_structural2[,'hg' := get('H_given')]
menages_structural2[,'hr' := get('H_received')]
menages_structural2[,'tr_age_2015' := floor(get("age")/5)*5]
EP_2015[,'tr_age_2015' := floor(get("AGE")/5)*5]

menages_structural2[, 'AGE' := get('age')]

menages_structural2[,'AGE' := age]

mean(menages_structural2[,.SD[1], by = "Id"]$non_ricardian)


output <- mindist::estimation_theta(
  theta_0 = c("beta" = {if(is.null(beta)) beta_0 else NULL},
              "gamma" = {if(is.null(gamma)) gamma_0 else NULL},
              "r" = {if(is.null(r)) 0.03 else NULL}
  ),
  beta = beta,
  r = r,
  gamma = gamma,
  approach = "two_step",
  prediction_function = wealthyR:::model_capitulation,
  non_ricardian = TRUE,
  non_ricardian_var = "non_ricardian",
  select_moments = select_moments,
  EP_2015 = EP_2015,
  EP_lon = EP_lon,
  EP_2018 = EP_2018,
  data_microsimulated = menages_structural2,
  N_moments = 180,
  wealth_var = "PATRI_NET",
  by = c("tr_age_2015", "tr_age_2015"),
  scale_model = "level",
  scale_variable_moment1 = "log",
  scale_variable_moment2 = "log",
  stat_moment2 = 'difference',
  moment1 = "level",
  moments_weights = "weight",
  verbose = TRUE,
  Hgiven_var = "hg",
  Hreceived_var = "hr",
  method = "Nelder-Mead",
  additional_vars = c("tr_age","SEXE","tr_agfinetu","findet", "non_ricardian")
)



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








# _____________________________________________
# PART 2: ESTIMATION ----
# _____________________________________________