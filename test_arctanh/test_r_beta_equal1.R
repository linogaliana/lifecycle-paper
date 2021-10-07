library(ggplot2)


inheritance_model <- readRDS("./modele.rds")


population <- readRDS(file = "./tempfile.rds")


path_data <- "./.."
source("./functions.R")

data <- construct_EP(path_data)

EP_2015 <- data[['EP_2015']]
EP_2018 <- data[['EP_2018']]
EP_lon <- data[['EP_lon']]


output_piste1 <- readRDS("./test_arctanh/output_piste1.rds")
output_piste2 <- readRDS("./test_arctanh/output_piste2.rds")
output_log <- readRDS("./test_arctanh/output_model_scale_log.rds")
output_rbeta <- readRDS("./output_beta_1plusR_equal1.rds")

EP_2015[,'y' := get('w') + r*get('PATFISOM')]
EP_2015[, 'annee' := 2015]
clean_data(EP_2015, sex_var = "SEXE", labor_income_var = "w", diploma_var = "AGFINETU",
           total_income_var = "y")

EP_2018[, 'annee' := 2018]
clean_data(EP_2018, sex_var = "SEXE", year = 2018, diploma_var = "AGFINETU")


EP_lon[,'y' := get('w_2015') + r*get('PATFISOM_2015')]
EP_lon <- merge(EP_lon, EP_2015[,.SD,.SDcols = c("IDENTIND14","tr_diplome", "decile_w", "decile_y")],
                by = c("IDENTIND14"))
EP_lon[, c('SEXE') := data.table::fifelse(get('SEXE')==1,
                                          'Homme',
                                          'Femme')]



plot_everything <- function(dir, scale_model, 
                            scale_moment, output, EP_2015, population,
                            scale = "arsinh",
                            r = 0.03){
  
  dir.create(sprintf("./test_arctanh/%s", dir))
  
  beta <- output$estimates$theta_hat['beta']
  gamma <- output$estimates$theta_hat['gamma']
  
  simulations <- capitulation::life_cycle_model(
    population,
    wealthvar_survey = "K_observed",
    r = r,
    beta = beta,
    gamma = gamma,
    observation_year = 2009,
    income_var = "revenu",
    Hgiven_var = "hg",
    Hreceived_var = "hr",
    # return_last = TRUE,
    get_capital_income = TRUE,
    scale_model = scale_model,
    additional_vars = c("tr_age_2015","sexe","findet"))
  simulations <- simulations[age > findet]
  
  if (scale_model == "log") simulations[,'wealth' := exp(get('wealth'))]
  
  clean_data(simulations)
  
  
  p1 <- capitulation::plot_K_age(simulations[age>findet], xlims = c(30,75),
                                 method = "median") +
    ylab("Simulated net wealth\n(in euros)")
  ggplot2::ggsave(plot = p1, sprintf("./test_arctanh/%s/k_age.pdf", dir), width = 12, height = 8)
  
  p2 <- capitulation::plot_rK_age(simulations[age>findet], xlims = c(30,75)) +
    ylab("Capital income share\n(% total income)")
  
  ggplot2::ggsave(plot = p2, sprintf("./test_arctanh/%s/rk_age.pdf", dir), width = 12, height = 8)
  
  
  tempdf = output$moments$moment_optimum[get("Nmoment") <62]
  moments = data.table::melt(tempdf[, .SD, .SDcols = c("Nmoment", "moment_simulations", "moment_data")],
                             id.vars = c("Nmoment"))
  moments[,'age' := get("Nmoment")+19]
  moments[,"variable" := data.table::fifelse(get("variable") == "moment_simulations",
                                             "Microsimulation",
                                             "Wealth survey")]
  p_moment1 <- ggplot2::ggplot(moments) +
    ggplot2::geom_bar(ggplot2::aes(x = age, y = value,
                                   fill = variable), position = "dodge",
                      stat='identity', width=.5) +
    ggplot2::scale_color_viridis_d() +
    ggplot2::scale_fill_viridis_d() +
    ggplot2::labs(x = "Age", y = sprintf("Median net wealth\n(%s scale)", scale),
                  color = NULL, fill = NULL) +
    ggplot2::theme(legend.position = "top") +
    scale_fill_manual(values = c('Microsimulation' = 'black', 
                                 'Wealth survey' = 'royalblue')) + theme_bw() +
    theme(legend.position = "top") +
    theme(axis.text=element_text(size=24),
          axis.title=element_text(size=24,face="bold"),
          legend.text=element_text(size=24))
  ggsave(plot = p_moment1, sprintf("./test_arctanh/%s/moment1.pdf", dir), width = 18, height = 10)
  
  tempdf = output$moments$moment_optimum[get("Nmoment") >= 62]
  moments = data.table::melt(tempdf[, .SD, .SDcols = c("Nmoment", "moment_simulations", "moment_data")],
                             id.vars = c("Nmoment"))
  moments[,'age' := seq(30, length.out = .N, by = 5), by = variable]
  moments[,"variable" := data.table::fifelse(get("variable") == "moment_simulations",
                                             "Microsimulation",
                                             "Wealth survey")]
  
  p_moment2 <- ggplot2::ggplot(moments) + geom_line(aes(x = age, y = value, color = variable)) +
    geom_point(aes(x = age, y = value, color = variable, shape = variable)) +
    geom_hline(yintercept = 0, color = "red") +
    scale_color_manual(values = c('Microsimulation' = 'black', 
                                  'Wealth survey' = 'royalblue')) + theme_bw() +
    theme(legend.position = "bottom") +
    labs(y = "Net wealth growth between\n 2015 and 2018") +
    scale_y_continuous(labels = scales::percent) +
    theme(axis.text=element_text(size=24),
          axis.title=element_text(size=24,face="bold"),
          legend.text=element_text(size=24))
  
  ggsave(plot = p_moment2, sprintf("./test_arctanh/%s/moment2.pdf", dir), width = 18, height = 10)
  
  
  
  simulations[,'endet' := get("wealth") < 0]
  df_endet <- simulations[annee == 2015, .('taux_endet' = mean(endet)), by = "age"]
  df_endet[,'source' := 'microsimulation']
  df_endet2 <- data.table::copy(EP_2015)
  data.table::setnames(df_endet2, old = "AGE", new = "age")
  df_endet2[,'endet' := get("PATRI_NET") < 0]
  df_endet2 <- df_endet2[, .('taux_endet' = mean(endet)), by = "age"]
  df_endet2[,'source' := 'survey']
  df_endet <- data.table::rbindlist(list(df_endet, df_endet2))
  
  end = ggplot(df_endet) +
    geom_line(aes(x = age, y = taux_endet, color = source)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(y = "Part individus endettés (en 2015)")
  ggsave(plot = end, sprintf("./test_arctanh/%s/taux_endettement.pdf", dir), width = 18, height = 10)
  
  
  
  simul_copy <- data.table::copy(simulations)
  p3 <- capitulation::plot_top_share(simul_copy, negative_values = "truncate") +
    theme_bw() + 
    theme(legend.position = "bottom") +
    labs(x = "Year", y = "Fraction detained by top 10%",
         color = NULL) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme(axis.text=element_text(size=24),
          axis.title=element_text(size=24,face="bold"),
          legend.text=element_text(size=24)) +
    scale_color_manual(labels = c("Labor income","Total income",'Wealth'),
                       values = c("#F8766D", "#00BA38", "#619CFF"))
  ggsave(plot = p3, sprintf("./test_arctanh/%s/top10.pdf", dir), width = 12, height = 8)
  
  simul_copy <- data.table::copy(simulations)
  p3 <- capitulation::plot_top_share(simul_copy, negative_values = "truncate", threshold = 0.99) +
    theme_bw() + 
    theme(legend.position = "bottom") +
    labs(x = "Year", y = "Fraction detained by top 1%",
         color = NULL) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme(axis.text=element_text(size=24),
          axis.title=element_text(size=24,face="bold"),
          legend.text=element_text(size=24)) +
    scale_color_manual(labels = c("Labor income","Total income",'Wealth'),
                       values = c("#F8766D", "#00BA38", "#619CFF"))    
  
  ggsave(plot = p3, sprintf("./test_arctanh/%s/top1.pdf", dir), width = 12, height = 8)
  
  simul_copy <- data.table::copy(simulations)
  p2 <- capitulation::plot_gini(simul_copy,
                                langage = "English",
                                labels = c("Labor income", "Net wealth", "Total income"),
                                vars = c("revenu", "wealth", "Y"),
                                negative_values = "truncate") +
    theme(axis.text=element_text(size=24),
          axis.title=element_text(size=24,face="bold"),
          legend.text=element_text(size=24))
  ggsave(plot = p2, sprintf("./test_arctanh/%s/gini_evolution_noneg.pdf", dir), width = 12, height = 8)
  
  return(NULL)
}


plot_everything(dir = "beta_1plusR", scale_model = "level", scale_moment = "asinh", output = output_rbeta,
                EP_2015 = EP_2015, population = population, r = NULL)
