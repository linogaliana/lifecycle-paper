set.seed(12345)
system("chmod +x microCI/install.sh && microCI/install.sh")

source("functions.R")

library(tablelight)



unzip("~/Destinie.zip", exdir="~")
unzip("~/Enquete Patrimoine.zip", exdir="~")


inheritance_model <- create_inheritance_model()


summary(inheritance_model)

saveRDS(
  inheritance_model, file = "~/estimation/modele.rds"
)


# PART 2 PREPARATION DONNEES ---------------

path_data <- "~"


data <- construct_EP()
EP_2015 <- data[['EP_2015']]
EP_2018 <- data[['EP_2018']]
EP_lon <- data[['EP_lon']]


saveRDS(data, "~/estimation/data.rds")


data_prediction <- capitulation::prepare_data(
  path_data = "~",
  inheritance_model = inheritance_model
)
# aws.s3::s3saveRDS(data_prediction, "data_prediction.rds",
#                   bucket = "groupe-788")

menages_structural2 <- data.table::copy(data_prediction)
menages_structural2[,'hg' := get('H_given')]
menages_structural2[,'hr' := get('H_received')]
menages_structural2[,'tr_age_2015' := floor(get("age")/5)*5]
menages_structural2[, 'AGE' := get('age')]
saveRDS(menages_structural2, file = "~/estimation/tempfile.rds")  



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
gamma_0 <- 0.6

menages_structural2[,'AGE' := age]


menages_structural2[,'SEXE' := as.numeric(as.character(get("SEXE")))]
EP_2015[,'SEXE' := as.numeric(as.character(get("SEXE")))]
EP_2018[,'SEXE' := as.numeric(as.character(get("SEXE")))]
EP_lon[,'SEXE' := as.numeric(as.character(get("SEXE")))]

output <- mindist::estimation_theta(
  theta_0 = c("beta" = {if(is.null(beta)) beta_0 else NULL},
              "gamma.parameters" = gamma_0,
              "r" = {if(is.null(r)) 0.03 else NULL}
  ),
  beta = beta,
  r = r,
  gamma = "gamma ~ 0 + SEXE",
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
  method = "Nelder-Mead",
  additional_vars = "SEXE"
)