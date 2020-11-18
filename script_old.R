library(tablelight)

N_moments = 6L



unzip("~/Destinie.zip", exdir="~")
unzip("~/Enquete Patrimoine.zip", exdir="~")

system("mkdir -p inst/dataINSEE")
system("cp -r ~/Destinie ~/estimation/inst/dataINSEE")
system("mkdir -p './inst/Enquete\ Patrimoine'")
system('cp -r "../Enquete\ Patrimoine" "./inst/dataINSEE/Enquete\ Patrimoine"')

path_data <- "./inst/dataINSEE"

data_prediction <- capitulation::prepare_data()

saveRDS(data_prediction, file = "tempfile.rds")

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



rmarkdown::render('old_report.Rmd',
                  params = list('r' = 0.03,
                                'beta' = .985,
                                'gamma' = .65,
                                "N_moments" = N_moments
                  )
)



data_prediction_augm2 <- capitulation::life_cycle_model(menages_structural2,
                                                        wealthvar_survey = "K_observed",
                                                        r = 0.03,
                                                        beta = 0.985,
                                                        gamma = 0.65,
                                                        observation_year = 2009,
                                                        income_var = "y_indiv",
                                                        return_last = FALSE,
                                                        get_capital_income = TRUE,
                                                        additional_vars = "tr_age_2015")

capitulation::plot_K_age(data_prediction_augm2)
capitulation::plot_rK_age(data_prediction_augm2)
