population <- readRDS(file = "~/estimation/tempfile.rds")

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


simulations[annee == 2015, as.list(summary(wealth)), by="sexe"]

simulations[, 'tr_diplome' := cut(findet, breaks = c(min(findet), 16,18,21,25, max(findet)), include.lowest = TRUE)]
simulations[, 'decile_y' := cut(Y,quantile(Y,probs= 0:10/10), labels = 1:10, include.lowest = TRUE)]


simulations[annee == 2015, as.list(summary(wealth)), by="sexe"]
simulations[annee == 2015, as.list(summary(wealth)), by="tr_diplome"][order(tr_diplome)]

simulations[annee == 2015, as.list(round(summary(wealth))), keyby=decile_w][order(decile_w)]
simulations[annee == 2015, as.list(round(summary(wealth)), keyby=decile_y][order(decile_y)]


tempdf <- simulations[,.('med' = median(wealth),
                         'mean' = mean(wealth)),by = annee]
tempdf <- data.table::melt(tempdf, id.vars = "annee")

# PAR ANNEE ------


library(ggplot2)
library(data.table)


ggplot(tempdf[annee>=2009 & annee<=2040]) + geom_line(aes(x = annee, y = value, color = variable))



# PAR AGE ------

tempdf <- simulations[,.('med' = median(wealth),
                         'mean' = mean(wealth)),by = c("annee", "age")]
tempdf <- data.table::melt(tempdf, id.vars = c("annee","age"))


ggplot(tempdf[annee>=2009 & annee<=2040 & age %between% c(25,80) & variable == "med"]) +
  geom_smooth(aes(x = age, y = value, color = factor(annee), linetype = variable), se = FALSE)
