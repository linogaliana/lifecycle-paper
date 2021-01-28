model <- readRDS("./modele.rds")

population <- readRDS("./tempfile.rds")

data <- readRDS("data.rds")
EP_2015 <- data[['EP_2015']]
EP_2018 <- data[['EP_2018']]
EP_lon <- data[['EP_lon']]


output <- readRDS("output.rds")

beta  <- output$estimates$theta_hat['beta']
gamma <- output$estimates$theta_hat['gamma']
r <- 0.03


source("functions.R")

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
  additional_vars = c("tr_age_2015","sexe","findet","ageliq"))

simulations <- simulations[age > findet]

clean_data(simulations)
simulations[, c('retired') := data.table::fifelse(get('age') <= get("ageliq"),
                                                  'active',
                                                  'retired')]

EP_2015[,'y' := get('labor_income') + r*get('PATFISOM')]
EP_2015[, 'annee' := 2015]
clean_data(EP_2015, sex_var = "SEXE",
           labor_income_var = "labor_income",
           diploma_var = "AGFINETU",
           total_income_var = "y",
           statut_var = "SITUA")

EP_2018[,'y' := get('labor_income') + r*get('PATFISOM')]
EP_2018[, 'annee' := 2018]
clean_data(EP_2018, sex_var = "SEXE", year = 2018,
           labor_income_var = "labor_income",
           diploma_var = "AGFINETU",
           total_income_var = "y",
           statut_var = "SITUA")


EP_lon[,'y' := get('labor_income_2015') + r*get('PATFISOM_2015')]
EP_lon <- merge(EP_lon, EP_2015[,.SD,.SDcols = c("IDENTIND14","tr_diplome", "decile_w", "decile_y")],
                by = c("IDENTIND14"))
EP_lon[, c('SEXE') := data.table::fifelse(get('SEXE')==1,
                                          'Male',
                                          'Female')]
EP_lon[, c('retired') := data.table::fifelse(get('SITUA_2015')=="5",
                                             'retired',
                                             'active')]


EP_data <- wealthyR:::prepare_inheritance_sample(
  path_survey =   "../Enquete Patrimoine"
)


inheritance_data <- REtage::prepare_estimation(EP_data,
                                               taille_tr_agfinetu = 1)


bounds <- c(3,8,15,30,60,100,150,200,250)*1000
lbounds <- log(bounds)

estim_data <- inheritance_data[get('income')>0]

estim_data[,'MTHER' := as.numeric(as.character(MTHER))]
estim_data <- estim_data[order(MTHER)]
estim_data[,'age' := get('AGE')]


EP_lon[,'evol' := 100*(log(w_real_2018) - log(w_real_2015))]
EP_lon[, c("inc_2015") := get("labor_income_2015") + exp(rnorm(nrow(EP_lon)))]
EP_lon2 <- EP_lon[is.finite(evol) & is.finite(POND)]

EP_lon2[, c('decile_w') := cut(get("inc_2015"),
                               quantile(get("inc_2015"),
                                        probs= 0:10/10),
                               labels = 1:10, include.lowest = TRUE
)]



# Figure 1: estimated vs predicted inheritance ------


inheritance_perf = estim_model("MTHER ~ lw + tr_age + SEXE + tr_agfinetu",
                               formulaSD = NULL)
pH <- inheritance_perf[[3]] + ggplot2::labs(y = "Total",
                                            x = "Valeur héritée, en tranches") +
  ggplot2::scale_fill_manual(breaks = c("observed","predicted"),
                             values = c("red","royalblue"),
                             labels = c("Observée dans l'Enquête Patrimoine",
                                        "Simulée à partir du modèle d'héritage")) +
  labs(fill = NULL)

ggplot2::ggsave(plot = pH, filename = "./output_ret_soc/fig01_inheritance_predicted.pdf",
                width = 13, height = 9)



# Figure 1: K by age -------------------

p_K <- capitulation::plot_K_age(simulations, method = "smooth",
                                xlims = c(30,75))

p_K <- p_K +
  #ggplot2::scale_y_continuous(limits = c(-100000, 150000)) +
  #ggplot2::scale_x_continuous(limits = c(25, 90)) +
  ggplot2::scale_color_viridis_d() +
  ggplot2::labs(y = "Richesse simulée (euros)")

ggplot2::ggsave(plot = p_K, filename = "./output_ret_soc/fig02_calibration.pdf",
                width = 13, height = 9)



# Figure 2: MOMENT 1 =============================

moment1 <- gridExtra::grid.arrange(
  wealthyR::plot_moment_age(EP_2015, EP_2018, simulations = simulations,
                            scale_moment_share = "share",
                            by_survey = "AGE", by_simulation = 'age', scale_variable  = "log")$fit[[1]] +
    ggplot2::scale_fill_manual(values = c('microsimulation' = 'black',
                                          'survey' = 'royalblue')) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "top") +
    ggplot2::labs(y = "Patrimoine médian"),
  wealthyR::plot_moment_age(EP_2015, EP_2018, simulations = simulations,
                            by_survey = "AGEPR",
                            by_simulation = 'age',
                            scale_variable = "log")$fit[[2]]  +
    ggplot2::scale_y_continuous(labels = scales::percent, trans = "reverse") +
    ggplot2::labs(y = "Part de la population observée")
)

ggplot2::ggsave(plot = moment1, "./output_ret_soc/fig03_moment1.pdf", width = 18, height = 20)


# Figure 3: MOMENT 2 ===============================

moment2 <- wealthyR::plot_moment_dK(
  EP_lon = EP_lon, simulations = simulations,
  scale = "log", by = "tr_age_2015"
) +
  ggplot2::scale_color_manual(values = c('simulation' = 'black',
                                         'survey' = 'royalblue')) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::labs(y = "Evolution sur trois ans du patrimoine financier",
                x = "Age en 2015", title = "") +
  ggplot2::scale_y_continuous(labels = scales::percent)


ggplot2::ggsave(plot = moment2, "./output_ret_soc/fig04_moment2.pdf",
                width = 12, height = 8)



# Figure 5: fit  ===============================


EP_2018[, 'net_fin_wealth' := get('PATFISOM') - get('MTDETTES')]
EP_2015[, 'net_fin_wealth' := get('PATFISOM') - get('MTDETTES')]


df <- data.table::data.table(
  "Actifs financiers (enquete patrimoine, individualisée)" =
    get_quantiles(EP_2018, 'PATFISOM', "POND_TRANS"),
  "Actifs - passifs financiers (enquete patrimoine, individualisée)" =
    get_quantiles(EP_2018, 'net_fin_wealth', "POND_TRANS"),
  "Modèle simulé (r = 3%)" = as.numeric(
    simulations[, round(quantile(get('wealth'), probs = c(1:9, 9.5, 9.9)/10))]
  ),
  "q" = 100*c(1:9, 9.5, 9.9)/10
)
df <- data.table::melt(df, id.vars = "q")



library(scales)
tn <- trans_new("abslog",
                function(x) sign(x)*log(abs(x)),
                function(y) exp(abs(y))*sign(y),
                domain=c(-Inf, Inf))

p_fit <- ggplot(df) +
  # geom_line(aes(x = q, y = value, color = variable,
  #               size = grepl("simulé", variable))) +
  geom_line(aes(x = q, y = sign(value)*log(abs(value)), color = variable,
                size = grepl("simulé", variable))) +
  geom_point(aes(x = q, y = sign(value)*log(abs(value)), color = variable)) +
  # geom_point(aes(x = q, y = value, color = variable)) +
  scale_size_manual(values = c(0.1, 2), 
                    labels = c("Données observées dans Enquête Patrimoine 2018",
                               "Données simulées")) +
  geom_hline(yintercept = 0) +
  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin()) +
  guides(col = guide_legend(nrow = 3)) +
  labs(x = "Quantile distribution du patrimoine financier",
       y = "Patrimoine",
       size = NULL,
       color = NULL)
# scale_y_continuous(trans = tn)


ggplot2::ggsave(plot = p_fit, "./output_ret_soc/fig05_fit.pdf",
                width = 12, height = 8)


# Table 1: data used for external validation ---------
# Pas besoin de R


# Table 2: table evolution richesse entre 2015 et 2018

note_eplon <- c("\\textit{Enquête Patrimoine 2014-2015 and 2017-2018}, statistics based on household head or spouse information. Variables are transformed into 2009 constant euros. Summary statistics are computed using 2015 survey weights.",
                "This table shows the distribution of individual wealth (household wealth divided by the number of spouses) growth between 2015 and 2018 on the whole sample of panelized individual.",
                "Labor income represents the sum of labor earnings, retirement pensions and unemployment benefits.")


tablelight:::stack_summary(object = list(EP_lon2, EP_lon2, EP_lon2,
                                         EP_lon2, EP_lon2[!is.na(tr_diplome)]), 
                           x_vars = "evol", weight_vars = "POND",
                           multirow_labels = c("Whole population","By sex:","By status:",
                                               "By labor income decile:",
                                               "By diploma level"),
                           # type = "dataframe",
                           by = c(NA, "SEXE", "retired", "decile_w", "tr_diplome"),
                           add.lines = note_eplon,
                           caption = "Summary statistics on the evolution of wealth between 2015 and 2018 (growth rates in \\%)",
                           label = "tab: summary stat EP 2015",
                           add_rules = TRUE,
                           stats = c("1Q", "median", 'mean','3Q','P90', "N")
)


# Table 3: inheritance ------------------


cat(
  tablelight::light_table(
    model, type = "latex", title = "Heritage: estimating equation",
    label = "tab:heritage",
    dep.var.labels = "Amount inherited",
    column.labels = "\\textit{Model in log}",
    covariate.labels = c("Log income",
                         c(sprintf("Age between %s and %s",
                                   seq(20, 90, by = 5),
                                   seq(25, 95, by = 5)
                         ),
                         "Older than 95"
                         ),
                         "Sexe",
                         paste0("Graduation age: ", c("14 or lower", 15: 28, "30 or higher"))
    ),
    add.lines = "Model estimated by interval regression (ordered probit regression with known thresholds) using declared received bequests in \\textit{Enquête Patrimoine 2009}"
  ),
  sep = "\n"
)


# Table 4: simulated and observed distribution inheritance --------

inheritance_distrib <- population[, .SD[1], by = "Id"]

tablelight:::summary_(
  inheritance_distrib[inheritance_distrib$H_given != 0], xvar = "hg",
  stats = c("1Q", 'median',"mean",'3Q','P90')
)


# Table 7: summary statistics on 2015 wealth survey ----------------

note <- c("\\textit{Enquête Patrimoine 2014-2015}, statistics based on household head or spouse information. Variables are transformed into 2009 constant euros. Summary statistics are computed using survey weights.",
          "This table shows the distribution of individual wealth (household wealth divided by the number of spouses) in 2015 on the whole sample (panelized and non-panelized individuals in wealth survey)",
          "Labor income represents the sum of labor earnings, retirement pensions and unemployment benefits.",
          "Wealth is household wealth divided by the number of spouses.")
tablelight:::stack_summary(object = list(EP_2015, EP_2015, EP_2015, EP_2015, EP_2015[!is.na(tr_diplome)]), x_vars = "PATFISOM", weight_vars = "POND",
                           multirow_labels = c("Whole population","By sex:","By labor income decile:"),
                           by = c(NA, "SEXE", "retired", "decile_w","tr_diplome"),
                           add.lines = note,
                           caption = "Summary statistics on 2015 wealth survey (\\textit{Enquête Patrimoine})",
                           label = "tab: summary stat EP 2015",
                           add_rules = TRUE,
                           stats = c("1Q", "mean", 'median','3Q','P90','N'),
                           type = "dataframe"
)


# Table 8: summary statistics on microsimulated data ----------------


note2 <- c("Results are based on Destinie microsimulation model for 2015. Variables are transformed into 2009 constant euros.",
           "Labor income represents the sum of labor earnings, retirement pensions and unemployment benefits.",
           "Wealth is individual simulated wealth based on Section \\ref{sec:model} model and structural parameters derived from estimation")


tablelight:::stack_summary(object = list(simulations[annee==2015 & age>findet],
                                         simulations[annee==2015 & age>findet],
                                         simulations[annee==2015 & age>findet],
                                         simulations[annee==2015 & age>findet],
                                         simulations[annee==2015 & age>findet]), x_vars = "wealth",
                           multirow_labels = c("Whole population","By sex:","By status:",
                                               "By labor income decile:",
                                               "By diploma level"),
                           by = c(NA, "SEXE", "retired", "decile_w", "tr_diplome"),
                           add.lines = note2,
                           caption = "Summary statistics predicted by our model (year : 2015)",
                           label = "tab: summary stat microsimulated wealth",
                           add_rules = TRUE,
                           stats = c("1Q", "mean", 'median','3Q','P90','N'),
                           type = "dataframe"
)


# Table 9: top income and wealth simulated shares --------


simulations[,'wealth_trunc' := pmax(0, get('wealth'))]
simulations[,'wealth_trunc' := get('wealth_trunc') + exp(rnorm(nrow(simulations)))]

sh_wealth <- share_total(simulations = simulations[age>findet & annee == 2015],
                         yvar = "wealth_trunc")
sh_labor <- share_total(simulations = simulations[age>findet & annee == 2015],
                        yvar = "revenu")
sh_income <- share_total(simulations = simulations[age>findet & annee == 2015],
                         yvar = "Y")

table_share <- merge(
  merge(sh_labor, sh_income),
  sh_wealth
)[order(-Group)]


EP_2015[,'wealth_trunc' := pmax(0, get('w_real'))]
EP_2015[,'wealth_trunc' := get('w_real') + exp(rnorm(nrow(EP_2015)))]

EP_2015[,'Y' := get("labor_income") + r*get("w_real")]

sh_wealth2 <- share_total(simulations = EP_2015[AGE > AGFINETU], yvar = "wealth_trunc")
sh_labor2 <- share_total(simulations = EP_2015[AGE > AGFINETU], yvar = "labor_income", jitterize = TRUE)
sh_income2 <- share_total(simulations = EP_2015[AGE > AGFINETU], yvar = "Y")

table_share2 <- merge(
  merge(sh_labor2, sh_income2),
  sh_wealth2
)[order(-Group)]



data.table::setnames(table_share, old = c("revenu", "Y", "wealth_trunc"),
                     new = c("Labor income","Total income",
                             "Financial wealth"))
data.table::setnames(table_share2, old = c("labor_income", "Y", "wealth_trunc"),
                     new = c("Labor income","Total income",
                             "Financial wealth"))

df <- do.call(c,
              lapply(1:nrow(table_share), function(i) paste(format(table_share[i], digits=3L), collapse = " & "))
)
df2 <- do.call(c,
               lapply(1:nrow(table_share2), function(i) paste(format(table_share2[i], digits=3L), collapse = " & "))
)

latex_table <- c(
  "\\begin{table}[ht]",
  "\\centering",
  "\\caption{Top income and wealth simulated shares (in \\%)}",
  "\\label{tab: concentration}",
  "\\begin{tabular}{lrrr}",
  "\\hline",
  "\\textsc{Group} & \\textsc{Labor income} & \\textsc{Total income} & \\textsc{Financial wealth} \\\\",
  "\\hline",
  paste0(c("\\multicolumn{4}{c}{\\textsc{Wealth survey}}",
           paste(df2, collapse = " \\\\ "),
           "\\midrule",
           "\\multicolumn{4}{c}{\\textsc{Simulated data}}",
           paste(df, collapse = " \\\\ ")
  ), " \\\\ "),
  "\\bottomrule",
  "\\multicolumn{4}{p{0.9\\linewidth}}{In this table, total income represents labor income (labor earnings, unemployment benefits and retirement pensions) plus financial income assuming a 3\\% return on observed or simulated wealth. Negative wealth are bottom-coded to zero in this Table} \\\\",
  "\\multicolumn{4}{p{0.9\\linewidth}}{Wealth survey data comes from \\textit{Enquête Patrimoine 2014-2015}. Household financial wealth is individualized using number of spouses in the household.} \\\\",
  "\\multicolumn{4}{p{0.9\\linewidth}}{Simulated data come from \\textit{Destinie} microsimulated data. Household labor income is individualized between spouses. Wealth is individual simulated wealth based on Section \\ref{sec:model} model and structural parameters derived from estimation}",
  "\\end{tabular}",
  "\\end{table}"
)
latex_table[latex_table == "\\midrule \\\\ "] <- "\\midrule"

cat(latex_table, sep = "\n")



# new table: rK/Y ----------------

EP_2015[,'rKY_100' := pmax(0, 100*r*get("w_real")/(get("labor_income") + r*get("w_real")))]
simulations[,"rKY_100" := pmax(0, get('rKY')*100)]


# PATRIMOINE ===============

note <- c("\\textit{Enquête Patrimoine 2014-2015}, statistics based on household head or spouse information. Variables are transformed into 2009 constant euros. Summary statistics are computed using survey weights.",
          "This table shows the distribution of individual wealth (household wealth divided by the number of spouses) in 2015 on the whole sample (panelized and non-panelized individuals in wealth survey)",
          "Labor income represents the sum of labor earnings, retirement pensions and unemployment benefits.",
          "Wealth is household wealth divided by the number of spouses.")


tablelight:::stack_summary(object = list(EP_2015, EP_2015, EP_2015, EP_2015, EP_2015[!is.na(tr_diplome)]),
                           x_vars = "rKY_100", weight_vars = "POND",
                           multirow_labels = c("Whole population","By sex:","By status:",
                                               "By labor income decile:",
                                               "By diploma level"),
                           by = c(NA, "SEXE", "retired", "decile_w","tr_diplome"),
                           add.lines = note,
                           caption = "Summary statistics on 2015 wealth survey (\\textit{Enquête Patrimoine})",
                           label = "tab: summary stat EP 2015",
                           add_rules = TRUE,
                           stats = c("1Q", "mean", 'median','3Q','P90','N'),
                           type = "dataframe"
)


# SIMULATIONS ======



note2 <- c("Results are based on Destinie microsimulation model for 2015. Variables are transformed into 2009 constant euros.",
           "Labor income represents the sum of labor earnings, retirement pensions and unemployment benefits.",
           "Wealth is individual simulated wealth based on Section \\ref{sec:model} model and structural parameters derived from estimation")


tablelight:::stack_summary(object = list(simulations[annee==2015 & age>findet],
                                         simulations[annee==2015 & age>findet],
                                         simulations[annee==2015 & age>findet],
                                         simulations[annee==2015 & age>findet],
                                         simulations[annee==2015 & age>findet]),
                           x_vars = "rKY_100",
                           multirow_labels = c("Whole population","By sex:","By status:",
                                               "By labor income decile:",
                                               "By diploma level"),
                           by = c(NA, "SEXE", "retired", "decile_w", "tr_diplome"),
                           add.lines = note2,
                           caption = "Summary statistics predicted by our model (year : 2015)",
                           label = "tab: summary stat microsimulated wealth",
                           add_rules = TRUE,
                           stats = c("1Q", "mean", 'median','3Q','P90','N'),
                           #                           type = "dataframe"
)
