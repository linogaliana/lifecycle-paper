set.seed(12345)

system("chmod +x microCI/install.sh && microCI/install.sh")

library(tablelight)





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
estim_data[,'age' := get('AGE')]
# estim_data[,'findet' := get('AGFINETU')]

inheritance_model <- REtage::ordered_model_threshold(
  data = data.frame(estim_data[order(MTHER)]),
  formula = "MTHER ~ lw + age + I((age^2)/100) + AGFINETU + I((AGFINETU^2)/100)",
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


#' EP_2015 <- wealthyR::read_EP(macro,
#'                              path_data = path_data,
#'                              year = 2015,
#'                              .colsWealth = c('IDENT','AGEPR','POND',
#'                                              'PATRI_NET','PATRI_BRUT',
#'                                              'PATFI','PATFIMTC_DECL',
#'                                              'PATFISOM','PATIMM',
#'                                              'PATPROFENT','PATPROFHENT',
#'                                              'PATRIC_DECL','NBUC','NPERS')
#' )
#' 
#' EP_2018 <- wealthyR::read_EP(macro,
#'                              path_data = path_data,
#'                              year = 2018,
#'                              .colsWealth = c('IDENT','AGEPR',
#'                                              #'POND','PATRI_NET',
#'                                              'PATRI_BRUT',
#'                                              'PATFI',
#'                                              #'PATFIMTC_DECL',
#'                                              'PATFISOM','PATIMM',
#'                                              'PATPROFENT','PATPROFHENT',
#'                                              'NBUC','NPERS'#,'PATRIC_DECL')
#'                              )
#' )
#' 

EP_2018 <- wealthyR::individualize_EP(path_data = "~", year = 2018)
EP_2015 <- wealthyR::individualize_EP(path_data = "~", year = 2015)


# +++++++++++++++++++++++++++++++++++++++++++++++
# B/ CREATE LONGITUDINAL INDIVIDUAL DATA ========
# +++++++++++++++++++++++++++++++++++++++++++++++

EP_lon <- wealthyR::longitudinal_survey(macro = macro,
                                        path_data = path_data,
                                        EP_2015 = EP_2015,
                                        EP_2018 = EP_2018)
EP_lon[,'tr_age_2015' := floor(get("AGEPR_2015")/5)*5]


data <- list(
  'EP_2015' = EP_2015,
  'EP_2018' = EP_2018,
  'EP_lon' = EP_lon
)
saveRDS(data, "data.rds")


menages_structural2 <- data.table::copy(data_prediction)
menages_structural2[,'hg' := get('H_given')]
menages_structural2[,'hr' := get('H_received')]
menages_structural2[,'tr_age_2015' := floor(get("age")/5)*5]
saveRDS(menages_structural2, file = "tempfile.rds")


# ESTIMATION ---------------

number_moments <- 2L
scale_wealth <- "log"
select_moments <- NULL
estimation_method <- "two_step"
parameters_estimation <- list("number_moments" = number_moments,
                              "scale_wealth" = scale_wealth,
                              "select_moments" = select_moments,
                              "method" = estimation_method)

beta <- NULL
r <- 0.03
gamma <- NULL

# beta_0 <- runif(1, min = 0.5, max = 1.5)
beta_0 <- 0.9
# gamma_0 <- runif(1, min = 0.2, max = 5)
gamma_0 <- 0.5

menages_structural2[,'AGE' := age]

output <- mindist::estimation_theta(
  theta_0 = c("beta" = {if(is.null(beta)) 0.9 else NULL},
              "gamma" = {if(is.null(gamma)) 0.5 else NULL},
              "r" = {if(is.null(r)) 0.03 else NULL}
              ),
  beta = beta,
  r = r,
  gamma = gamma,
  model_function = mindist:::loss_function,
  prediction_function = wealthyR:::model_capitulation,
  approach = estimation_method,
  select_moments = select_moments,
  EP_2015 = EP_2015,
  EP_lon = EP_lon,
  EP_2018 = EP_2018,
  data_microsimulated = menages_structural2,
  N_moments = 180,
  by = c("AGE", "tr_age_2015"),
  # moment1 = "share",
  scale = scale_wealth,
  moments_weights = "weight",
  verbose = TRUE,
  Hgiven_var = "hg",
  Hreceived_var = "hr",
  method = "Nelder-Mead"
)



moments <- wealthyR:::label_moments(
  N_moments = number_moments,
  data = EP_lon,
  scale = scale_wealth,
  select_moments = select_moments,
  by = c("AGEPR", NULL)
)

saveRDS(
  menages_structural2, file = "tempfile.rds"
)


rmarkdown::render(
  'automatic_report.Rmd',
  output_file = "overidentification_moment1_rfixed",
  envir = new.env(),
  params = list('r' = {if(is.null(r)) output$estimates$theta_hat['r'] else r},
                'beta' = {if(is.null(beta)) output$estimates$theta_hat['beta'] else beta},
                'gamma' = {if(is.null(gamma)) output$estimates$theta_hat['gamma'] else gamma},
                "gamma.parameters" = {if(is.null(output$estimates$theta_hat['gamma.parameters'])) output$estimates$theta_hat['gamma.parameters'] else NULL},
                'estimates' = output$estimates,
                "parameters_estimation" = parameters_estimation,
                "moments" = output$moments,
                "label_moments" = moments,
                "output" = output
  )
)


# output <- mindist::estimation_theta(
#   theta_0 = c("beta" = {if(is.null(beta)) 0.9 else NULL},
#               "gamma.parameters" = {if(is.null(gamma)) 0.5 else NULL},
#               "r" = {if(is.null(r)) 0.03 else NULL}
#   ),
#   beta = beta,
#   r = r,
#   gamma = "gamma ~ 0 + tr_age_2015",
#   model_function = mindist:::loss_function,
#   prediction_function = wealthyR:::model_capitulation,
#   additional_vars = "tr_age_2015",
#   approach = estimation_method,
#   select_moments = select_moments,
#   EP_2015 = EP_2015,
#   EP_lon = EP_lon,
#   EP_2018 = EP_2018,
#   data_microsimulated = menages_structural2,
#   N_moments = 180,
#   by = c("AGEPR", NA),
#   scale = scale_wealth,
#   moments_weights = "weight",
#   verbose = TRUE,
#   Hgiven_var = "hg",
#   Hreceived_var = "hr",
#   method = "Nelder-Mead"
# )





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




create_moment_data(
  EP_2015 = EP_2015,
  EP_lon = EP_lon,
  EP_2018 = EP_2018,
  data_microsimulated = menages_structural2,
  N_moments = 4L,
  by = "tr_age_2015"
)
