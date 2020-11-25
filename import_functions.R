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
}

source("functions.R")

population <- readRDS("~/estimation/tempfile.rds")
data <- readRDS("~/estimation/data.rds")
EP_2015 <- data[['EP_2015']]
EP_2018 <- data[['EP_2018']]
EP_lon <- data[['EP_lon']]

population[,'AGE' := get('age')]