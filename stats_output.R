
# PARAMETERS GLOBAUX -------------------------

population <- readRDS(file = "tempfile.rds")
data <- readRDS("data.rds")
EP_2015 <- data[['EP_2015']]
EP_2018 <- data[['EP_2018']]
EP_lon <- data[['EP_lon']]


beta  <- 0.975
gamma <- 0.8428378
r <- 0.03



# SIMULATE MODEL -----------------------------

population2 <- data.table::copy(population)
population2 <- population2[age>findet]

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

simulations2 <- capitulation::life_cycle_model(
  population2,
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


capitulation::plot_K_age(simulations)

# UN PEU DE DATA CLEANING --------------------

clean_data <- function(data, sex_var = "sexe",
                       diploma_var = "findet",
                       labor_income_var = "revenu",
                       total_income_var = "Y",
                       year = 2015){
  
  
  data[, c('SEXE') := data.table::fifelse(get(sex_var)==1,
                                          'Male',
                                          'Female')]
  data[, c('tr_diplome') := cut(get(diploma_var), breaks = c(min(get(diploma_var)), 16,18,21,25, max(get(diploma_var))), include.lowest = TRUE)]
  
  if (year != 2015) return(data)  
  
  data[, c('decile_w') := cut(get(labor_income_var),
                              quantile(get(labor_income_var),
                                       probs= 0:10/10) + seq_along(0:10)*.Machine$double.eps,
                              labels = 1:10, include.lowest = TRUE
  )]
  data[, c('decile_y') := cut(get(total_income_var),
                              quantile(get(total_income_var),
                                       probs= 0:10/10)  + seq_along(0:10)*.Machine$double.eps,
                              labels = 1:10, include.lowest = TRUE
  )]
  
  return(data)
  
}

simulations <- simulations[get("age") > get("findet")]


clean_data(simulations)

EP_2015[,'y' := get('w') + r*get('PATFISOM')]
EP_2015[, 'annee' := 2015]
clean_data(EP_2015, sex_var = "SEXE", labor_income_var = "w", diploma_var = "AGFINETU",
           total_income_var = "y")

EP_2018[, 'annee' := 2018]
clean_data(EP_2018, sex_var = "SEXE", year = 2018, diploma_var = "AGFINETU")


EP_lon[,'y' := get('labor_income') + r*get('PATFISOM_2015')]
EP_lon <- merge(EP_lon, EP_2015[,.SD,.SDcols = c("IDENTIND14","tr_diplome", "decile_w", "decile_y")],
                by = c("IDENTIND14"))
EP_lon[, c('SEXE') := data.table::fifelse(get('SEXE')==1,
                                          'Homme',
                                          'Femme')]

# COMPUTE SUMMARY STATS ---------

write_stats <- function(data, wealth_var = "wealth",
                        pond = NULL,
                        suffix = ""){
  
  mysummary <- function(x, weights = NULL, ...){
    return(
      list('Min.' = round(min(x, ...)),
           '1st Qu.' = round(Hmisc::wtd.quantile(x, weights = weights, probs = 0.25, ...)),
           'Median' = round(Hmisc::wtd.quantile(x, weights = weights, probs = 0.5, ...)),
           'Mean' = round(Hmisc::wtd.mean(x, weights = weights, ...)),
           '3rd Qu.' = round(Hmisc::wtd.quantile(x, weights = weights, probs = 0.75, ...)),
           'Max' = round(max(x, ...))
      )
    )
  }
  
  if (is.null(pond)){
    pond <- "tempvar"
    data[, c(pond) := 1L]
  }
  
  data.table::fwrite(
    data[annee == 2015, mysummary(get(wealth_var), weights = get(pond)), by="SEXE"],
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
  
  if (pond == "tempvar") data[, c(pond) := NULL]
  
}


write_stats(simulations)
write_stats(EP_2015, wealth_var = "PATFISOM", suffix = "_EP15", pond = "POND")


cat(
  tablelight:::stack_summary(object = list(EP_2015, EP_2015, EP_2015), x_vars = "PATFISOM", weight_vars = "POND",
                             multirow_labels = c("Whole population","By sex:","By labor income decile:"), 
                             by = c(NA, "SEXE", "decile_w"),
                             add.lines = "Summary statistics are computed using survey weights",
                             caption = "Summary statistics on wealth survey (\\textit{Enquête Patrimoine})",
                             label = "tab: summary stat EP 2015",
                             add_rules = TRUE,
                             stats = c("mean","1Q",'median','3Q')
  ),
  sep = "\n"
)

cat(
  tablelight:::stack_summary(object = list(simulations[annee==2015],
                                           simulations[annee==2015],
                                           simulations[annee==2015]), x_vars = "wealth",
                             multirow_labels = c("Whole population","By sex:","By labor income decile:"), 
                             by = c(NA, "SEXE", "decile_w"),
                             caption = "Summary statistics predicted by our model (year : 2015)",
                             label = "tab: summary stat microsimulated wealth",
                             add_rules = TRUE,
                             stats = c("mean","1Q",'median','3Q')
  ),
  sep = "\n"
)






# MODELE HERITAGE ------

model <- readRDS("modele.rds")
cat(
  tablelight::light_table(
    model, type = "latex", title = "Heritage: estimating equation",
    label = "tab:heritage",
    dep.var.labels = "Amount inherited",
    column.labels = "\\textit{Model in log}",
    covariate.labels = c("Log income","Age",
                         "Age (squared, divided by 100)", "Graduation age",
                         "Graduation age (squared, divided by 100)")
  ), sep = "\n"
)



## Faire un truc propre pour update papier


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
                              vars = c("revenu", "wealth", "Y"),
                              negative_values = "truncate")
ggsave(plot = p2, "./stats/gini_evolution_noneg.pdf", width = 12, height = 8)

simul_copy <- data.table::copy(simulations)
p2b <- capitulation::plot_gini(simul_copy,
                               vars = c("revenu", "wealth", "Y"),
                               negative_values = "keep")
ggsave(plot = p2b, "./stats/gini_evolution.pdf", width = 12, height = 8)


simul_copy <- data.table::copy(simulations)
p3 <- capitulation::plot_top_share(simul_copy, negative_values = "truncate") +
  theme_bw() + 
  theme(legend.position = "bottom") +
  labs(x = "Year", y = "Percentage total held by top10",
       color = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))
ggsave(plot = p3, "./stats/top10_evolution_noneg.pdf", width = 12, height = 8)

simul_copy <- data.table::copy(simulations)
p3 <- capitulation::plot_top_share(simul_copy, negative_values = "keep") +
  theme_bw() + 
  theme(legend.position = "bottom") +
  labs(x = "Year", y = "Percentage total held by top10",
       color = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))
ggsave(plot = p3, "./stats/top10_evolution.pdf", width = 12, height = 8)


simul_copy <- data.table::copy(simulations)
p3 <- capitulation::plot_top_share(simul_copy, negative_values = "truncate") +
  theme_bw() + 
  theme(legend.position = "bottom") +
  labs(x = "Year", y = "Percentage total held by top10",
       color = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

ggsave(plot = p3, "./stats/top10_evolution_noneg.pdf", width = 12, height = 8)

simul_copy <- data.table::copy(simulations)
p3 <- capitulation::plot_top_share(simul_copy, negative_values = "keep") +
  theme_bw() + 
  theme(legend.position = "bottom") +
  labs(x = "Year", y = "Percentage total held by top10",
       color = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

ggsave(plot = p3, "./stats/top10_evolution.pdf", width = 12, height = 8)



simul_copy <- data.table::copy(simulations)
p3 <- capitulation::plot_top_share(simul_copy, threshold = 0.99, negative_values = "truncate") +
  theme_bw() + 
  theme(legend.position = "bottom") +
  labs(x = "Year", y = "Percentage total held by top1",
       color = NULL)
ggsave(plot = p3, "./stats/top1_evolution_noneg.pdf", width = 12, height = 8)

simul_copy <- data.table::copy(simulations)
p3 <- capitulation::plot_top_share(simul_copy, threshold = 0.99, negative_values = "keep") +
  theme_bw() + 
  theme(legend.position = "bottom") +
  labs(x = "Year", y = "Percentage total held by top1",
       color = NULL)
ggsave(plot = p3, "./stats/top1_evolution.pdf", width = 12, height = 8)



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

## MOMENT 1 =================

moment1 <- gridExtra::grid.arrange(
  wealthyR::plot_moment_age(EP_2015, EP_2018, simulations = simulations,
                            by_survey = "AGE", by_simulation = 'age', scale = "log")$fit[[1]] +
    scale_fill_manual(values = c('microsimulation' = 'black', 
                                 'survey' = 'royalblue')) + theme_bw() +
    theme(legend.position = "top"),
  wealthyR::plot_moment_age(EP_2015, EP_2018, simulations = simulations,
                            by_survey = "AGEPR", by_simulation = 'age', scale = "log")$fit[[2]]
)

ggsave(plot = moment1, "./stats/moment1.pdf", width = 18, height = 10)


## MOMENT 1 (BY=.) =================


# moment1_sex <- wealthyR:::plot_moment_age_facet(EP_2015, EP_2018, simulations = simulations,
#                                  by_survey = "AGE", by_simulation = "age",
#                                  scale = "log",
#                                  facets_vars = "SEXE")   +
#   scale_fill_manual(values = c('microsimulation' = 'black', 
#                                'survey' = 'royalblue')) + theme_bw() +
#   theme(legend.position = "top")
# ggsave(plot = moment1_sex, "./stats/moment1_by_sex.pdf", width = 12, height = 8)








## MOMENT 2 ================

moment2 <- wealthyR::plot_moment_dK(
  EP_lon = EP_lon, simulations = simulations,
  scale = "log", by = "tr_age_2015"
) + scale_color_manual(values = c('simulation' = 'black', 
                                  'survey' = 'royalblue')) + theme_bw() +
  theme(legend.position = "bottom")


ggsave(plot = moment2, "./stats/moment2.pdf", width = 12, height = 8)


## MOMENT 2 BY=. ================

## SEXE +++++++++

moment2_sex <- wealthyR:::plot_moment_dK_facet(
  EP_lon = EP_lon, simulations = simulations,
  scale = "log", xaxis = "tr_age_2015", facets_vars = "SEXE")

moment2_sex <- moment2_sex +
  scale_color_manual(values = c('simulation' = 'black', 
                                'survey' = 'royalblue')) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(plot = moment2_sex, "./stats/moment2_by_sex.pdf", width = 12, height = 8)


## diplome ++++

moment2_diplome <- wealthyR:::plot_moment_dK_facet(
  EP_lon = EP_lon[!is.na(tr_diplome)], simulations = simulations,
  scale = "log", xaxis = "tr_age_2015", facets_vars = "tr_diplome")

moment2_diplome <- moment2_diplome +
  scale_color_manual(values = c('simulation' = 'black', 
                                'survey' = 'royalblue')) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(plot = moment2_diplome, "./stats/moment2_by_diplome.pdf", width = 12, height = 8)


## income decile ++++


moment2_decile_labor <- wealthyR:::plot_moment_dK_facet(
  EP_lon = EP_lon, simulations = simulations,
  scale = "log", xaxis = "tr_age_2015", facets_vars = "decile_w")

moment2_decile_labor <- moment2_decile_labor + scale_color_manual(values = c('simulation' = 'black', 
                                                                             'survey' = 'royalblue')) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(plot = moment2_decile_labor, "./stats/moment2_by_decile_labor.pdf", width = 20, height = 20)



moment2_decile_total <- wealthyR:::plot_moment_dK_facet(
  EP_lon = EP_lon, simulations = simulations,
  scale = "log", xaxis = "tr_age_2015", facets_vars = "decile_y")

moment2_decile_total <- moment2_decile_total + scale_color_manual(values = c('simulation' = 'black', 
                                                                             'survey' = 'royalblue')) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(plot = moment2_decile_total, "./stats/moment2_by_decile_total.pdf", width = 20, height = 20)





# CHUTES --------------------------------


tempdf <- simulations[,.('med' = median(wealth),
                         'mean' = mean(wealth)),by = c("annee", "age")]
tempdf <- data.table::melt(tempdf, id.vars = c("annee","age"))


ggplot(tempdf[annee>=2009 & annee<=2040 & age %between% c(25,80) & variable == "med"]) +
  geom_smooth(aes(x = age, y = value, color = factor(annee)), se = FALSE)



