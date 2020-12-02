
# PARAMETERS GLOBAUX -------------------------

population <- readRDS(file = "~/estimation/tempfile.rds")
data <- readRDS("~/estimation/data.rds")
EP_2015 <- data[['EP_2015']]
EP_2018 <- data[['EP_2018']]
EP_lon <- data[['EP_lon']]


beta  <- 0.9724306
gamma <- 0.8435442
r <- 0.03

# SIMULATE MODEL -----------------------------


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
  return_last = FALSE,
  get_capital_income = TRUE,
  additional_vars = c("tr_age_2015","sexe","findet"))


# UN PEU DE DATA CLEANING --------------------

clean_data <- function(data, sex_var = "sexe",
                       diploma_var = "findet",
                       labor_income_var = "revenu",
                       total_income_var = "Y"){
  
  
  data[, c('SEXE') := data.table::fifelse(get(sex_var)==1,
                                       'Homme',
                                       'Femme')]
  data[, c('tr_diplome') := cut(get(diploma_var), breaks = c(min(get(diploma_var)), 16,18,21,25, max(get(diploma_var))), include.lowest = TRUE)]
  data[, c('decile_w') := cut(get(labor_income_var),
                           quantile(get(labor_income_var),
                                    probs= 0:10/10),
                           labels = 1:10, include.lowest = TRUE
  )]
  data[, c('decile_y') := cut(get(total_income_var),
                           quantile(get(total_income_var),
                                    probs= 0:10/10),
                           labels = 1:10, include.lowest = TRUE
  )]
  
  return(data)
  
}

clean_data(simulations)

EP_2015[,'y' := get('w') + r*get('PATFISOM')]
EP_2015[, 'annee' := 2015]
clean_data(EP_2015, sex_var = "SEXE", labor_income_var = "w", diploma_var = "AGFINETU",
           total_income_var = "y")



# COMPUTE SUMMARY STATS ---------

write_stats <- function(data, wealth_var = "wealth",
                        suffix = ""){
  
  
  data.table::fwrite(
    data[annee == 2015, as.list(round(summary(get(wealth_var)))), by="SEXE"],
    file = sprintf("./stats/stats_sexe%s.csv", suffix)
  )
  
  
  
  data.table::fwrite(
    data[annee == 2015, as.list(round(summary(get(wealth_var)))), by="tr_diplome"][order(tr_diplome)],
    file = sprintf("./stats/stats_diplome%s.csv", suffix)
  )
  
  
  data.table::fwrite(
    data[annee == 2015, as.list(round(summary(get(wealth_var)))), keyby=decile_w][order(decile_w)],
    file = sprintf("./stats/stats_income_labor%s.csv", suffix)
  )
  
  data.table::fwrite(
    data[annee == 2015, as.list(round(summary(get(wealth_var)))), keyby=decile_y][order(decile_y)],
    file = sprintf("./stats/stats_income_total%s.csv", suffix)
  )
  
}


write_stats(simulations)
write_stats(EP_2015, wealth_var = "PATFISOM", suffix = "_EP15")


# GRAPHIQUES PAR ANNEE --------------------------------

library(ggplot2)
library(data.table)

tempdf <- simulations[,.('median wealth' = median(wealth),
                         'mean wealth' = mean(wealth)),by = annee]
tempdf <- data.table::melt(tempdf, id.vars = "annee")


p1 <- ggplot(tempdf[annee>=2009 & annee<=2040]) + geom_line(aes(x = annee, y = value/1000, color = variable)) +
  labs(x = "Year", y = "Wealth (thousand euros)", color = NULL) +
  theme(legend.position = "bottom")
ggsave(plot = p1, "./stats/plot_wealth_evolution.pdf")


# capitulation::plot_K_age(simulations)
# capitulation::plot_K_age(simulations)


## INEGALITES =========================

simul_copy <- data.table::copy(simulations)

p2 <- capitulation::plot_gini(simul_copy,
                        vars = c("revenu", "wealth", "Y"))
ggsave(plot = p2, "./stats/gini_evolution.pdf", width = 12, height = 8)

simul_copy <- data.table::copy(simulations)

p3 <- capitulation::plot_top_share(simul_copy) +
  theme_bw() + 
  theme(legend.position = "bottom") +
  labs(x = "Year", y = "Percentage total held by top10",
       color = NULL)
ggsave(plot = p3, "./stats/top10_evolution.pdf", width = 12, height = 8)

simul_copy <- data.table::copy(simulations)

p3b <- capitulation::plot_top_share(simul_copy, threshold = 0.99) +
  theme_bw() + 
  theme(legend.position = "bottom") +
  labs(x = "Year", y = "Percentage total held by top1",
       color = NULL)
ggsave(plot = p3b, "./stats/top1_evolution.pdf", width = 12, height = 8)



# ENDETTEMENT ======================

simulations[,'endet' := get("wealth") < 0]


ggplot(simulations[, .('taux_endet' = mean(endet)), by = "annee"]) +
  geom_line(aes(x = annee, y = taux_endet)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  xlim(2009,2040) + labs(y = "Part individus endettés")


simulations[annee == 2015, .('taux_endet' = mean(endet)) , by = "SEXE"]
simulations[annee == 2015, .('taux_endet' = mean(endet)) , by = "tr_diplome"][order(tr_diplome)]

ggplot2::ggplot(
  simulations[annee == 2015, .('taux_endet' = mean(endet)) , by = "decile_w"][order(decile_w)]
) + geom_point(aes(x = decile_w, y = taux_endet))

ggplot2::ggplot(
  simulations[annee == 2015, .('taux_endet' = mean(endet)) , by = "decile_y"][order(decile_y)]
) + geom_point(aes(x = decile_y, y = taux_endet))


# MOMENTS --------------------------










# CHUTES --------------------------------


tempdf <- simulations[,.('med' = median(wealth),
                         'mean' = mean(wealth)),by = c("annee", "age")]
tempdf <- data.table::melt(tempdf, id.vars = c("annee","age"))


ggplot(tempdf[annee>=2009 & annee<=2040 & age %between% c(25,80) & variable == "med"]) +
  geom_smooth(aes(x = age, y = value, color = factor(annee)), se = FALSE)



