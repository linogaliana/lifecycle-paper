library(tablelight)

N_moments = 6L



unzip("~/Destinie.zip", exdir="~")
unzip("~/Enquete Patrimoine.zip", exdir="~")



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


inheritance_model <- REtage::ordered_model_threshold(
  data = data.frame(estim_data[order(MTHER)]),
  formula = "MTHER ~ lw",
  link = "probit",
  constantSD = TRUE,
  thresholds = lbounds
)

summary(inheritance_model)

saveRDS(
  inheritance_model, file = "modele.rds"
)


# PART 2 ESTIMATION DU MODELE ---------------

path_data <- "~"

data_prediction <- capitulation::prepare_data(
  path_data = "~",
  inheritance_model = inheritance_model
)

aws.s3::s3saveRDS(data_prediction, "data_prediction.rds",
               bucket = "groupe-788")





macro <- capitulation::macro


EP_2015 <- wealthyR::read_EP(macro,
                             path_data = path_data,
                             year = 2015,
                             .colsWealth = c('IDENT','AGEPR','POND',
                                             'PATRI_NET','PATRI_BRUT',
                                             'PATFI','PATFIMTC_DECL',
                                             'PATFISOM','PATIMM',
                                             'PATPROFENT','PATPROFHENT',
                                             'PATRIC_DECL','NBUC','NPERS')
)

EP_2018 <- wealthyR::read_EP(macro,
                             path_data = path_data,
                             year = 2018,
                             .colsWealth = c('IDENT','AGEPR',
                                             #'POND','PATRI_NET',
                                             'PATRI_BRUT',
                                             'PATFI',
                                             #'PATFIMTC_DECL',
                                             'PATFISOM','PATIMM',
                                             'PATPROFENT','PATPROFHENT',
                                             'NBUC','NPERS'#,'PATRIC_DECL')
                             )
)


# +++++++++++++++++++++++++++++++++++++++++++++++
# B/ CREATE LONGITUDINAL INDIVIDUAL DATA ========
# +++++++++++++++++++++++++++++++++++++++++++++++

EP_lon <- wealthyR::longitudinal_survey(macro = macro,
                                        path_data = path_data,
                                        EP_2015 = EP_2015,
                                        EP_2018 = EP_2018)


data <- list(
  'EP_2015' = EP_2015,
  'EP_2018' = EP_2018,
  'EP_lon' = EP_lon
)
saveRDS(data, "data.rds")


menages_structural2 <- data.table::copy(data_prediction)
menages_structural2[,'hg' := get('H_given')]
menages_structural2[,'hr' := get('H_received')]


# ESTIMATION ---------------

number_moments <- 3L
scale_wealth <- "log"
select_moments <- NULL
estimation_method <- "two_step"
parameters_estimation <- list("number_moments" = number_moments,
                              "scale_wealth" = scale_wealth,
                              "select_moments" = select_moments,
                              "method" = estimation_method)
  
output <- wealthyR::estimation_theta(
  theta_0 = c("beta" = 0.9,
              "gamma" = 0.5),
  model_function = wealthyR:::loss_function,
  prediction_function = wealthyR:::model_capitulation,
  method = estimation_method,
  select_moments = select_moments,
  r = 0.03,
  EP_2015 = EP_2015,
  EP_lon = EP_lon,
  EP_2018 = EP_2018,
  data_microsimulated = menages_structural2,
  N_moments = number_moments,
  scale = scale_wealth,
  verbose = TRUE,
  Hgiven_var = "hg",
  Hreceived_var = "hr")

saveRDS(
  menages_structural, file = "tempfile.rds"
)


rmarkdown::render('trois_moments.Rmd',
                  params = list('r' = 0.03,
                                'beta' = output$estimates$theta_hat['beta'],
                                'gamma' = output$estimates$theta_hat['gamma'],
                                "parameters_estimation" = parameters_estimation
                  )
)




# data_prediction_augm2 <- capitulation::life_cycle_model(menages_structural2,
#                                                         wealthvar_survey = "K_observed",
#                                                         r = 0.03,
#                                                         beta = 0.9504511,
#                                                         gamma = 0.6731211,
#                                                         observation_year = 2009,
#                                                         income_var = "revenu",
#                                                         Hgiven_var = "hg",
#                                                         Hreceived_var = "hr",
#                                                         return_last = FALSE,
#                                                         get_capital_income = TRUE)
# 
# 
# rmarkdown::render('automatic_report.Rmd',
#                   params = list('r' = 0.03,
#                                 'beta' = 0.9504511,
#                                 'gamma' = 0.6731211
#                                 )
# )