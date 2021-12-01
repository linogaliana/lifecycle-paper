source("functions.R")


path_data <- ".."

menages_structural2 <- readRDS('start_estimation.rds')



data <- construct_EP(path_data)


EP_2015 <- data[['EP_2015']]
EP_2018 <- data[['EP_2018']]
EP_lon <- data[['EP_lon']]
EP_2015[,'tr_age_2015' := floor(get("AGE")/5)*5]


plot_K_age2 <- function(simulations, observed_data, has_ricardian = FALSE,
                        weight_observed_data = "POND",
                        wealth_var = "wealth",
                        trans = NULL,
                        trans_survey = NULL,
                        wealth_var_survey = 'PATRI_NET',
                        year_var = "annee",
                        age_var = "age",
                        age_var_survey = "AGE",
                        graduation_var = "findet",
                        observation_year = 2015,
                        start_year = 2009,
                        final_year = 2040,
                        method = "smooth"){
  
  
  simulations2 <- simulations[get(year_var) %between% c(start_year,final_year)]
  simulations2 <- simulations2[get(age_var)>get(graduation_var)]
  observed_data2 <- data.table::copy(observed_data)
  
  if (has_ricardian) simulations2 <- simulations2[!(non_ricardian)]
  
  if (!is.null(trans)){
    
    if (trans == "log"){
      simulations2[,'wealth' := log(wealth)]
    }
    if (trans == "exp"){
      simulations2[,'wealth' := exp(wealth)]
    }
  }  
  
  if (!is.null(trans_survey)){
    
    observed_data2 <- observed_data2[get(wealth_var_survey)>0]
    
    if (trans_survey == "log"){
      observed_data2[,c(wealth_var_survey) := log(get(wealth_var_survey))]
    }
    if (trans_survey == "exp"){
      observed_data2[,c(wealth_var_survey) := exp(get(wealth_var_survey))]
    }
  }  
  
  if (method != "smooth"){
    simulations2 <- simulations2[, .("wealth" = median(get(wealth_var), na.rm = TRUE)),
                                 by = c(year_var, age_var)]
    simulations2[,'source' := "simulation"]
  }
  
  if (is.null(weight_observed_data)){
    observed_data2 <- observed_data2[,.("wealth" =  median(get(wealth_var_survey), na.rm = TRUE)),
                                     by = age_var_survey]
  } else{
    observed_data2 <- observed_data2[,.("wealth" =  Hmisc::wtd.quantile(get(wealth_var_survey), weights = get(weight_observed_data),
                                                                        na.rm = TRUE, probs = .5)),
                                     by = age_var_survey]
  }
  observed_data2[, c(year_var) := observation_year]
  observed_data2[,'source' := "survey"]
  data.table::setnames(observed_data2, old = age_var_survey, new = age_var)
  
  
  dataframes <- data.table::rbindlist(list(simulations2, observed_data2),
                                      use.names = TRUE, fill = TRUE)
  dataframes[,'size' := .5 + 0.5*as.numeric(get("source") == "survey")]
  
  
  ggplot(dataframes[age %between% c(30,75)]) + geom_line(aes(x = age, y = wealth/1000, color = factor(annee), size = size))
  
  ggplot(dataframes[age %between% c(30,75)]) + geom_smooth(aes(x = age, y = wealth/1000, color = factor(annee), linetype = factor(source == "survey")), se = FALSE)
  
  
}


loop_gamma <- function(g){
  
  df = data.table::copy(menages_structural2)
  
  message("---------------------------------------")
  message("---------------------------------------")
  message(sprintf("gamma: %s", g))
  
  
  output <- mindist::estimation_theta(
    theta_0 = c("beta" = 0.9,
                # "gamma" = {if(is.null(gamma)) gamma_0 else NULL},
                "r" = {if(is.null(r)) 0.03 else NULL}
    ),
    beta = beta,
    r = r,
    gamma = g,
    approach = "two_step",
    prediction_function = wealthyR:::model_capitulation,
    # non_ricardian = TRUE,
    # non_ricardian_var = "non_ricardian",
    EP_2015 = EP_2015,
    EP_lon = EP_lon,
    EP_2018 = EP_2018,
    data_microsimulated = menages_structural2,
    N_moments = 180,
    wealth_var = "PATRI_NET",
    by = c("tr_age_2015", "tr_age_2015"),
    scale_model = "level",
    scale_variable_moment1 = "asinh",
    scale_variable_moment2 = "log",
    stat_moment2 = 'difference',
    moment1 = "level",
    moments_weights = "weight",
    verbose = TRUE,
    Hgiven_var = "hg",
    Hreceived_var = "hr",
    method = "Nelder-Mead",
    additional_vars = c("tr_age","SEXE","tr_agfinetu","findet", "non_ricardian")
  )
  
  flname <- sprintf("./gamma-loop/gamma_%s.rds", gsub("\\.","", as.character(g)))
  
  message("---------------------------------------")
  message(sprintf("Writing output at %s location ", flname))
  
  saveRDS(output, flname)
  
  message("GMM output saved !")
  
  rm(output)
  gc()
}


gamma <- c(seq(.1,0.9, by = 0.1), seq(1,3, by = 0.2))
lapply(gamma, loop_gamma)

library(data.table)
library(ggplot2)

loop_output_plot <- function(g, beta = NULL, save_plot = TRUE){
  
  flname <- sprintf("./gamma-loop/gamma_%s.rds", gsub("\\.","", as.character(g)))
  out <- readRDS(flname)
  
  if (is.null(beta)) beta <- as.numeric(out$estimates$theta_hat)
  
  simulations <- capitulation::life_cycle_model(
    menages_structural2,
    wealthvar_survey = "K_observed",
    r = r,
    beta = beta,
    gamma = g,
    observation_year = 2009,
    income_var = "revenu",
    Hgiven_var = "hg",
    Hreceived_var = "hr",
    # return_last = TRUE,
    get_capital_income = TRUE,
    scale_model = "level",
    additional_vars = c("tr_age_2015","SEXE","findet"))
  p <- plot_K_age2(simulations[age>findet],EP_2015, method = "median")
  p <- p + ggtitle(sprintf("gamma: %s ; beta: %s", g, as.numeric(out$estimates$theta_hat)))
  plname <- sprintf("./gamma-loop/ggplot/gamma_%s.pdf", gsub("\\.","", as.character(g)))
  if (save_plot){
    ggsave(plot = p, filename = plname)
  } else{
    return(list(p, simulations))
  }
  return(simulations)
}

gamma <- c(seq(.1,0.9, by = 0.1), seq(1,3, by = 0.2))
simuls = lapply(gamma, loop_output_plot)

loop_output_plot(0.7, save_plot = FALSE)
simuls = lapply(gamma, loop_output_plot, save_plot = FALSE)

gridExtra::grid.arrange()

library(gridExtra)
n <- 5
nCol <- 4
pdf("./gamma-loop/ggplot/all.pdf", width = 100, height = 100) # Open a new pdf file
do.call("grid.arrange", c(lapply(simuls, function(p) p[[1]]), ncol=nCol))
dev.off()

lapply(simuls, function(d){
  p <- plot_K_age2(d[age>findet],EP_2015, method = "median")
  g <- d$gamma[1]
  b  <- d$beta[1]
  p <- p + ggtitle(sprintf("gamma: %s ; beta: %s", g, b))
  plname <- sprintf("./gamma-loop/ggplot/gamma_%s.pdf", gsub("\\.","", as.character(g)))
  ggsave(plot = p, filename = plname)
})


lapply(gamma, function(g) {
  flname <- sprintf("./gamma-loop/gamma_%s.rds", gsub("\\.","", as.character(g)))
  out <- readRDS(flname)
  return(out$estimates$theta_hat/g)
})

head(essai[[1]], 2)
head(essai[[3]], 2)
