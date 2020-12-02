
# PARAMETERS GLOBAUX -------------------------

population <- readRDS(file = "~/estimation/tempfile.rds")
data <- readRDS("~/estimation/data.rds")
EP_2015 <- data[['EP_2015']]
EP_2018 <- data[['EP_2018']]
EP_lon <- data[['EP_lon']]


beta  <- 0.9724306
gamma <- 0.8435442


# SIMULATE MODEL -----------------------------


simulations <- capitulation::life_cycle_model(
  population,
  wealthvar_survey = "K_observed",
  r = 0.03,
  beta = output$estimates$theta_hat["beta"],
  gamma = output$estimates$theta_hat["gamma"],
  observation_year = 2009,
  income_var = "revenu",
  Hgiven_var = "hg",
  Hreceived_var = "hr",
  return_last = FALSE,
  get_capital_income = TRUE, additional_vars = c("tr_age_2015","sexe","findet"))


# UN PEU DE DATA CLEANING --------------------

simulations[, 'SEXE' := data.table::fifelse(get('sexe')==1,
                                            'Homme',
                                            'Femme')]
simulations[, 'tr_diplome' := cut(findet, breaks = c(min(findet), 16,18,21,25, max(findet)), include.lowest = TRUE)]
simulations[, 'decile_w' := cut(revenu,quantile(revenu,probs= 0:10/10), labels = 1:10, include.lowest = TRUE)]
simulations[, 'decile_y' := cut(Y,quantile(Y,probs= 0:10/10), labels = 1:10, include.lowest = TRUE)]


write_stats <- function(data, wealth_var = "wealth"){
  
  
  data.table::fwrite(
    simulations[annee == 2015, as.list(round(summary(get(wealth_var)))), by="SEXE"],
    file = "./stats/stats_sexe.csv"
  )
  
  
  
  data.table::fwrite(
    simulations[annee == 2015, as.list(round(summary(get(wealth_var)))), by="tr_diplome"][order(tr_diplome)],
    file = "./stats/stats_diplome.csv"
  )
  
  
  data.table::fwrite(
    simulations[annee == 2015, as.list(round(summary(get(wealth_var)))), keyby=decile_w][order(decile_w)],
    file = "./stats/stats_income_labor.csv"
  )
  
  data.table::fwrite(
    simulations[annee == 2015, as.list(round(summary(get(wealth_var)))), keyby=decile_y][order(decile_y)],
    file = "./stats/stats_income_total.csv"
  )
  
}


EP_2015


# GRAPHIQUE PAR ANNEE --------------------------------

library(ggplot2)
library(data.table)

tempdf <- simulations[,.('median wealth' = median(wealth),
                         'mean wealth' = mean(wealth)),by = annee]
tempdf <- data.table::melt(tempdf, id.vars = "annee")


p1 <- ggplot(tempdf[annee>=2009 & annee<=2040]) + geom_line(aes(x = annee, y = value/1000, color = variable)) +
  labs(x = "Year", y = "Wealth (thousand euros)", color = NULL) +
  theme(legend.position = "bottom")
ggsave(plot = p1, "./stats/plot_wealth_evolution.pdf")





# CHUTES --------------------------------


tempdf <- simulations[,.('med' = median(wealth),
                         'mean' = mean(wealth)),by = c("annee", "age")]
tempdf <- data.table::melt(tempdf, id.vars = c("annee","age"))


ggplot(tempdf[annee>=2009 & annee<=2040 & age %between% c(25,80) & variable == "med"]) +
  geom_smooth(aes(x = age, y = value, color = factor(annee)), se = FALSE)
