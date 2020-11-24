library(data.table)
library(capitulation)
library(ggplot2)

if (!file.exists("~/estimation/tempfile.rds")){
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
  
  
  # PART 2 ESTIMATION DU MODELE ---------------
  
  path_data <- "~"
  
  
  data <- construct_EP()
  
  
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
  saveRDS(menages_structural2, file = "~/estimation/tempfile.rds")  
} else{
  population <- readRDS("~/estimation/tempfile.rds")
  data <- readRDS("~/estimation/data.rds")
  EP_2015 <- data[['EP_2015']]
  EP_2018 <- data[['EP_2018']]
  EP_lon <- data[['EP_lon']]
}

population[,'AGE' := get('age')]

# df_moment <- wealthyR::model_capitulation(
#   EP_2015 = EP_2015,
#   EP_lon = EP_lon,
#   EP_2018 = EP_2018,
#   data_microsimulated = population,
#   theta = estimator_theta,
#   approach = estimation_method,
#   select_moments = select_moments,
#   EP_2015 = EP_2015,
#   EP_lon = EP_lon,
#   EP_2018 = EP_2018,
#   data_microsimulated = population,
#   N_moments = 180,
#   by = c("AGE", "tr_age_2015"),
#   # moment1 = "share",
#   scale = scale_wealth,
#   moments_weights = "weight",
#   verbose = TRUE,
#   Hgiven_var = "hg",
#   Hreceived_var = "hr",
#   method = "Nelder-Mead"
# )
# W_1 <- mindist:::optimal_weight_matrix(df_moment[["epsilon"]])





library(shiny)
runApp("./shiny")
