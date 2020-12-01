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
simulations[, 'decile_w' := cut(revenu,quantile(revenu,probs=seq(.1,.9,.1)))]
simulations[, 'decile_y' := cut(Y,quantile(revenu,probs=seq(.1,.9,.1)))]


simulations[annee == 2015, as.list(summary(wealth)), by="sexe"]
simulations[annee == 2015, as.list(summary(wealth)), by="tr_diplome"][order(tr_diplome)]

simulations[annee == 2015, as.list(summary(wealth)), keyby=decile_w][order(decile_w)]
simulations[annee == 2015, as.list(summary(wealth)), keyby=decile_y][order(decile_y)]

