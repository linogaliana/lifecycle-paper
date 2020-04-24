# EP_2015
# EP_2018 = NULL
# EP_lon
data_microsimulated = data.table::copy(menages_structural2)
r = 0.03
gamma = 0.5
beta = 0.98
r_low = NULL
r_high = NULL
N_moments = 3
ages = c(35, 50)
stats = c("mean", "sd") 
quantiles = c(0.1, 0.5, 0.9)
age_var_simulations = "age"
normalize = TRUE
#scale = c("level", "log")
scale = "level"
exclude_negative = FALSE
Hgiven_var = "H_given"
Hreceived_var = "H_received" 
N_moments = 26L
survey_years = c(2015, 
                 2018)


# scale <- match.arg(scale)
scale1 <- ifelse(N_moments <= 2, yes = "log", no = scale)


simulations <- capitulation::life_cycle_model(data = data_microsimulated, 
                                              r = r, gamma = gamma, beta = beta, r_low = r_low, r_high = r_high, 
                                              income_var = "y_indiv", weight_var = NULL, wealthvar_survey = "K_observed", 
                                              Hgiven_var = Hgiven_var, Hreceived_var = Hreceived_var)


# MOMENT 1/ PIC ----------------------

age_var = "AGEPR"
wealth_var = "PATFISOM"
age_var = "AGEPR"
deltaK_var = "dW"
# age_2015_var = "AGEPR_2015" 


moment1 <- moment_pic(EP_2015, wealth_var = wealth_var, 
                      age_var = "AGEPR",
                      moment_var = "moment",
                      normalize = normalize, 
                      scale = scale1,
                      survey_year = survey_years[1],
                      exclude_negative = exclude_negative)

moment2 <- moment_pic(EP_2018, wealth_var = wealth_var, 
                      age_var = "AGEPR",
                      moment_var = "moment",
                      normalize = normalize, 
                      scale = scale1,
                      survey_year = survey_years[2],
                      exclude_negative = exclude_negative)

moments_observed <- rbind(
  moment1[, year := 2015], moment2[,year := 2018]
)
moments_observed[,'source' := "survey"]                            


moment_simulation <- lapply(2009:2025, function(yy){
  moment_pic(simulations, wealth_var = "wealth", 
             moment_var = "moment",
             age_var = age_var_simulations,
             normalize = normalize, 
             scale = scale1,
             survey_year = yy,
             exclude_negative = exclude_negative)
})
moment_simulation <- data.table::rbindlist(moment_simulation)
moment_simulation[, 'year' := 2009:2025]
moment_simulation[,'source' := "simulation"]                            

moments_age <- data.table::rbindlist(list(
  moments_observed,
  moment_simulation), use.names = TRUE, fill = TRUE
)
moments_age[, 'Nmoment' := NULL]


ggplot2::ggplot() +
  ggplot2::geom_point(data = moments_age,
                      ggplot2::aes(x = year,
                                   y = moment,
                                   shape = source,
                                   color = source)) +
  ggplot2::geom_line(data = moments_age[source=='simulation'],
                      ggplot2::aes(x = year,
                                   y = moment,
                                   color = source)) +
  ggplot2::scale_color_viridis_d() +
  ggplot2::labs(x = "Year", y = "Moment value")
