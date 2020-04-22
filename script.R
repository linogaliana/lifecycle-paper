# install.packages("RcppEigen")
# devtools::install_git("https://git.stable.innovation.insee.eu/microsimulation/retage", upgrade = "never")
# devtools::install_git("https://git.stable.innovation.insee.eu/microsimulation/capitulation", upgrade = "never")
# devtools::install_git("https://git.stable.innovation.insee.eu/microsimulation/wealthyr", upgrade = "never")
# devtools::install_github("linogaliana/tablelight", upgrade = "never")

library(tablelight)

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

aws.s3::s3saveRDS(data_prediction, "data_prediction.rds",
               bucket = "groupe-788")


# load(paste0(path_data, "/individual_data2.RData"))

# menages_structural <- data.table::rbindlist(menages_structural)


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


menages_structural2 <- data.table::copy(data_prediction)
menages_structural2[,'hg' := get('H_given')]
menages_structural2[,'hr' := get('H_received')]

# output <- wealthyR::estimation_theta(
#   theta_0 = c("beta" = 0.9,
#               "gamma" = 0.5),
#   model_function = wealthyR:::loss_function,
#   prediction_function = wealthyR:::model_capitulation,
#   method = "one_step",
#   r = 0.03,
#   EP_2015 = EP_2015,
#   EP_lon = EP_lon,
#   EP_2018 = EP_2018,
#   data_microsimulated = menages_structural2,
#   N_moments = 26L,
#   scale = "log",
#   verbose = TRUE,
#   Hgiven_var = "hg",
#   Hreceived_var = "hr")

data_prediction_augm2 <- capitulation::life_cycle_model(menages_structural2,
                                                        wealthvar_survey = "K_observed",
                                                        r = 0.03,
                                                        beta = 0.9504511,
                                                        gamma = 0.6731211,
                                                        observation_year = 2009,
                                                        income_var = "revenu",
                                                        Hgiven_var = "hg",
                                                        Hreceived_var = "hr",
                                                        return_last = FALSE,
                                                        get_capital_income = TRUE)


capitulation::plot_K_age(data_prediction_augm2)
capitulation::plot_rK_age(data_prediction_augm2)

capitulation::plot_gini(data_prediction_augm2,
                        vars = c("revenu","wealth","Y")
)


LC1 <- ineq::Lc(data_prediction_augm2[annee==2020 & wealth>0]$wealth, plot = FALSE)
LC2 <- ineq::Lc(data_prediction_augm2[annee==2020 & wealth>0]$revenu, plot = FALSE)

plot(LC1)
lines(LC2, col=4)

df <- data.frame(p1 = LC1$p,
                 wealth = LC1$L,
                 uniform = LC1$p, 
                 y = LC2$L)
df <- data.table::data.table(reshape2::melt(df, id.vars = "p1"))

df[, 'lab' := "Pure equality"]
df[variable == "wealth", lab := "Wealth"]
df[variable == "y", lab := "Labor income"]


p <- ggplot2::ggplot(df) +
  ggplot2::geom_line(ggplot2::aes(x = p1, y = value, color = lab))+
  ggplot2::theme(legend.position="bottom") +
  ggplot2::scale_color_viridis_d() +
  ggplot2::labs(x = "", y = "", color = "") +
  ggplot2::theme(text = ggplot2::element_text(size=24),
                 axis.title = ggplot2::element_text(size=20,face="bold"))

p
