set.seed(12345)
#system("chmod +x microCI/install.sh && microCI/install.sh")

source("functions.R")

library(tablelight)



unzip("../Destinie.zip", exdir="..")
unzip("../Enquete Patrimoine.zip", exdir="..")


inheritance_model <- create_inheritance_model(
  path_survey =  "../Enquete Patrimoine"
)


summary(inheritance_model)

saveRDS(
  inheritance_model, file = "./modele.rds"
)
inheritance_model <- readRDS("./modele.rds")

# PART 2 PREPARATION DONNEES ---------------

path_data <- ".."


data <- construct_EP(path_data)
EP_2015 <- data[['EP_2015']]
EP_2018 <- data[['EP_2018']]
EP_lon <- data[['EP_lon']]


saveRDS(data, "./data.rds")



data_prediction <- capitulation::prepare_data(
  path_data = "..",
  inheritance_model = inheritance_model,
  time_0 = "birth"
)
# aws.s3::s3saveRDS(data_prediction, "data_prediction.rds",
#                   bucket = "groupe-788")

menages_structural2 <- data.table::copy(data_prediction)
menages_structural2[,'hg' := get('H_given')]
menages_structural2[,'hr' := get('H_received')]
menages_structural2[,'tr_age_2015' := floor(get("age")/5)*5]
menages_structural2[, 'AGE' := get('age')]
saveRDS(menages_structural2, file = "./tempfile.rds")  

menages_structural2 <- readRDS("./tempfile.rds")
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
r <- 0.02
gamma <- NULL

# beta_0 <- runif(1, min = 0.5, max = 1.5)
beta_0 <- 0.9
# gamma_0 <- runif(1, min = 0.2, max = 5)
gamma_0 <- 0.6

menages_structural2[,'AGE' := age]

output <- mindist::estimation_theta(
  theta_0 = c("beta" = {if(is.null(beta)) beta_0 else NULL},
              "gamma" = {if(is.null(gamma)) gamma_0 else NULL},
              "r" = {if(is.null(r)) 0.03 else NULL}
  ),
  beta = beta,
  r = r,
  gamma = gamma,
  # model_function = mindist:::loss_function,
  prediction_function = wealthyR:::model_capitulation,
  approach = estimation_method,
  select_moments = select_moments,
  EP_2015 = EP_2015,
  EP_lon = EP_lon,
  EP_2018 = EP_2018,
  data_microsimulated = menages_structural2,
  N_moments = 180,
  by = c("AGE", "tr_age_2015"),
  moment1 = "share",
  scale = scale_wealth,
  moments_weights = "weight",
  verbose = TRUE,
  Hgiven_var = "hg",
  Hreceived_var = "hr",
  method = "Nelder-Mead"
)


class(output) <- c("mindist", class(output))

tablelight::view_html(tablelight::light_table(output, type = "html"))


moments <- wealthyR:::label_moments(
  N_moments = number_moments,
  data = EP_lon,
  scale = scale_wealth,
  select_moments = select_moments,
  by = c("AGE", NULL)
)

saveRDS(output, "~/output.rds")


tablelight::view_html(tablelight::light_table(output, type = "html", covariate.labels = c("$\\beta$", "$\\gamma$"),
                                              dep.var.labels = "\\textsc{Estimates}", column.labels = NULL))



cat(
  tablelight::light_table(output, type = "latex", covariate.labels = c("$\\beta$", "$\\gamma$"),
                        dep.var.labels = "\\textsc{Estimates}", column.labels = NULL,
                        title = "Estimation results", label = "tab: estimation table"),
  sep = "\n",
  file = "~/5cece6ccccdef65e149a3774/tables/resultsGMM.tex"
)



rmarkdown::render(
  'automatic_report.Rmd',
  output_file = "overidentification_specif_type_share_gamma05",
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
