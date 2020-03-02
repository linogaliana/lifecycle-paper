

aws.s3::save_object(object = "Destinie.zip", bucket = "groupe-788",
                    file = "~/Destinie.zip")


aws.s3::save_object(object = "Enquete Patrimoine.zip", bucket = "groupe-788",
                    file = "~/Enquete Patrimoine.zip")


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



# PART 2 ESTIMATION DU MODELE ---------------

path_data <- "~"

data_prediction <- capitulation::prepare_data(
  path_data = "~",
  inheritance_model = inheritance_model
)

fst::write_fst(data_prediction, "./data_prediction.fst")


# load(paste0(path_data, "/individual_data2.RData"))

# menages_structural <- data.table::rbindlist(menages_structural)


macro <- capitulation::macro

