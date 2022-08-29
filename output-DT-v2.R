library(tablelight)
library(data.table)
library(ggplot2) 

source("functions.R", encoding = "UTF-8")

library(targets)


bounds <- c(3,8,15,30,60,100,150,200,250)*1000
lbounds <- log(bounds)

tar_load(probit_model)
tar_load(inheritance_model)
tar_load(estim_data)
tar_load(output_uncertainty)
tar_load(output_uncertainty_selection)
tar_load(output_uncertainty_selection_no_trans)
tar_load(EP_lon)
tar_load(EP_2015)
tar_load(EP_2018)
tar_load(simulations2)
tar_load(simulations)
tar_load(test_moments)
tar_load(test_moments_uncertainty)
tar_load(data_prediction_selection)

# erreur dans les uc qu'on corrige
tar_load(uc)

# beta  <- mindist:::sigmoide(
#   as.numeric(
#     output_uncertainty_selection$estimates$theta_hat
#   )
# )
beta <- output_uncertainty_selection_no_trans$estimates$theta_hat
gamma <- 1
r <- 0.03


# FIGURE 1: PREDICTED INHERITANCE DISTRIBUTION ---------------

prediction_2step <- REtage:::predict_two_steps(probit = probit_model,
                                               intreg = inheritance_model,
                                               estim_data, scale = "class", lbounds = lbounds)

prediction <- prediction_2step


confusion <- data.frame('Observed' = estim_data$MTHER, 'Predicted' = prediction_2step$prediction, "rn" = seq_len(nrow(estim_data)))
confusion <- data.table::melt(data.table::setDT(confusion), id.vars = "rn")

p_french <- plot_confusion('fr')
p_english <- plot_confusion('en')


ggplot2::ggsave(plot = p_french, filename = "./output_ret_soc_v3/fig01_inheritance_predicted.pdf",
                width = 13, height = 9)
ggplot2::ggsave(plot = p_french, filename = "./output_ret_soc_v3/fig01_inheritance_predicted.png",
                width = 13, height = 9)
ggplot2::ggsave(plot = p_english, filename = "./output_ret_soc_v3/fig01_inheritance_predicted_DT.pdf",
                width = 13, height = 9)
data.table::fwrite(p_french$data[, .N, by = c('variable','value')], file = "./output_ret_soc_v3/fig01_data.csv")


# FIGURE & 6: PATRIMOINE ET rK PAR AGE ----------------


kage_fr <- plot_K_age2(simulations2, EP_2015, method = "median", has_ricardian = FALSE,
                       trans = "div_1000", trans_survey = "div_1000",
                       lang = "fr")

kage_eng <- plot_K_age2(simulations2, EP_2015, method = "median", has_ricardian = FALSE,
                        trans = "div_1000", trans_survey = "div_1000")

kage_fr_no_risk <- plot_K_age2(simulations, EP_2015, method = "median", has_ricardian = FALSE,
                               trans = "div_1000", trans_survey = "div_1000",
                               lang = "fr")

kage_eng_no_risk <- plot_K_age2(simulations, EP_2015, method = "median", has_ricardian = FALSE,
                                trans = "div_1000", trans_survey = "div_1000")


ggplot2::ggsave(plot = kage_fr, filename = "./output_ret_soc_v3/fig01_kage2.pdf",
                width = 13, height = 9)
ggplot2::ggsave(plot = kage_fr, filename = "./output_ret_soc_v3/fig01_kage2.png",
                width = 13, height = 9)

data.table::fwrite(kage_fr$data[,.SD,.SDcols = c('annee','age','predict','source')],
                   "./output_ret_soc_v3/fig01_data.csv")

ggplot2::ggsave(plot = kage_eng, filename = "./output_ret_soc_v3/fig01_kage2_DT.pdf",
                width = 13, height = 9)
ggplot2::ggsave(plot = kage_eng_no_risk, filename = "./output_ret_soc_v3/fig01_kage2_no_risk_DT.pdf",
                width = 13, height = 9)

EP_2015[,'rKY' := r*get("PATRI_NET")/(get("labor_income") + r*get("PATRI_NET"))]

rkage <- plot_K_age2(simulations2, EP_2015, method = "median", has_ricardian = FALSE,
                     wealth_var = "rKY", wealth_var_survey = "rKY",
                     lang = "eng") +
  ggplot2::scale_y_continuous(labels = scales::percent)
rkage_fr <- plot_K_age2(simulations2, EP_2015, method = "median", has_ricardian = FALSE,
                        wealth_var = "rKY", wealth_var_survey = "rKY",
                        lang = "fr") +
  ggplot2::scale_y_continuous(labels = scales::percent)
rkage_no_risk <- plot_K_age2(simulations, EP_2015, method = "median", has_ricardian = FALSE,
                             wealth_var = "rKY", wealth_var_survey = "rKY",
                             lang = "eng") +
  ggplot2::scale_y_continuous(labels = scales::percent)


ggplot2::ggsave(plot = rkage, filename = "./output_ret_soc_v3/fig01b_rkage_DT.pdf",
                width = 13, height = 9)
ggplot2::ggsave(plot = rkage_no_risk, filename = "./output_ret_soc_v3/fig01b_rkage_no_risk_DT.pdf",
                width = 13, height = 9)


ggplot2::ggsave(plot = rkage_fr, filename = "./output_ret_soc_v3/fig01b_rkage.pdf",
                width = 13, height = 9)
ggplot2::ggsave(plot = rkage_fr, filename = "./output_ret_soc_v3/fig01b_rkage.png",
                width = 13, height = 9)
data.table::fwrite(rkage_fr$data[,.SD,.SDcols = c('annee','age','predict','source')],
                   "./output_ret_soc_v3/fig01b_data.csv")



# FIGURE 2 & 3: MOMENTS ---------------------------

plots <- plot_moment_age_wide(test_moments_uncertainty)
moment1 <- plots[[1]]
moment2 <- plots[[2]]
plots_eng <- plot_moment_age_wide(test_moments_uncertainty, lang = "eng")
moment1_eng <- plots_eng[[1]]
moment2_eng <- plots_eng[[2]]
plots_eng_no_risk <- plot_moment_age_wide(test_moments, lang = "eng")
moment1_eng_no_risk <- plots_eng_no_risk[[1]]
moment2_eng_no_risk <- plots_eng_no_risk[[2]]

ggplot2::ggsave(plot = moment1, "./output_ret_soc_v3/fig03_moment1.pdf",
                width = 18, height = 20)
ggplot2::ggsave(plot = moment2, "./output_ret_soc_v3/fig03_moment2.pdf",
                width = 18, height = 20)
ggplot2::ggsave(plot = moment1, "./output_ret_soc_v3/fig03_moment1.png",
                width = 18, height = 20)
ggplot2::ggsave(plot = moment2, "./output_ret_soc_v3/fig03_moment2.png",
                width = 18, height = 20)
data.table::fwrite(plots[[1]]$data, "./output_ret_soc_v3/fig03_moment1.csv")
data.table::fwrite(plots[[2]]$data, "./output_ret_soc_v3/fig03_moment2.csv")

ggplot2::ggsave(plot = moment1_eng, "./output_ret_soc_v3/fig03_moment1_DT.pdf",
                width = 18, height = 20)
ggplot2::ggsave(plot = moment2_eng, "./output_ret_soc_v3/fig03_moment2_DT.pdf",
                width = 18, height = 20)
ggplot2::ggsave(plot = moment1_eng_no_risk, "./output_ret_soc_v3/fig03_moment1_no_risk_DT.pdf",
                width = 18, height = 20)
ggplot2::ggsave(plot = moment2_eng_no_risk, "./output_ret_soc_v3/fig03_moment2_no_risk_DT.pdf",
                width = 18, height = 20)


# Figure 4: FIT  ===============================


EP_2018[, 'net_fin_wealth' := get('PATRI_NET')]
EP_2015[, 'net_fin_wealth' := get('PATRI_NET')]

p_fit1 <- plot_fit_distribution(EP_2018, simulations2)
p_fit2 <- plot_fit_distribution(EP_2018, simulations2, "eng")


ggplot2::ggsave(plot = p_fit1[[1]], "./output_ret_soc_v3/fig05_fit.png",
                width = 12, height = 8)
ggplot2::ggsave(plot = p_fit1[[1]], "./output_ret_soc_v3/fig05_fit.pdf",
                width = 12, height = 8)
ggplot2::ggsave(plot = p_fit1[[2]], "./output_ret_soc_v3/fig05_fit_level.png",
                width = 12, height = 8)
data.table::fwrite(p_fit1[[1]]$data, file = "./output_ret_soc_v3/fig05_data.csv")
ggplot2::ggsave(plot = p_fit2[[1]], "./output_ret_soc_v3/fig05_fit_DT.pdf",
                width = 16, height = 8)
ggplot2::ggsave(plot = p_fit2[[2]], "./output_ret_soc_v3/fig05_fit_level_DT.pdf",
                width = 16, height = 8)




# FIGURE 7 & 8: top shares --------------------------

path_data = ".."
library(dplyr)

data.table::setnames(uc, "nbre_uc", "UC")

tar_load(simulations2_contrefactuel_beta)
tar_load(simulations_contrefatuel_beta)

simul_copy <- restructure_simulations(simulations2, uc)
simul_copy_no_risk <- restructure_simulations(simulations, uc)
simul_copy_contrefactuel_beta <- restructure_simulations(simulations2_contrefactuel_beta, uc)
simul_copy_contrefactuel_beta_no_risk <- restructure_simulations(simulations_contrefatuel_beta, uc)



simul_household <- simul_copy[, lapply(.SD, sum), by = c("id_household","annee"),
                              .SDcols = c("revenu","wealth","Y", "UC")]
simul_household[, `:=` (revenu = revenu/UC, wealth = wealth/UC, Y=Y/UC)]

simul_household_no_risk <- simul_copy_no_risk[, lapply(.SD, sum), by = c("id_household","annee"),
                                              .SDcols = c("revenu","wealth","Y", "UC")]
simul_household_no_risk[, `:=` (revenu = revenu/UC, wealth = wealth/UC, Y=Y/UC)]

simul_household_contrefactuel_beta <- simul_copy_contrefactuel_beta[, lapply(.SD, sum), by = c("id_household","annee"),
                                              .SDcols = c("revenu","wealth","Y", "UC")]
simul_household_contrefactuel_beta[, `:=` (revenu = revenu/UC, wealth = wealth/UC, Y=Y/UC)]

simul_household_contrefactuel_beta_no_risk <- simul_copy_contrefactuel_beta_no_risk[, lapply(.SD, sum), by = c("id_household","annee"),
                                                                    .SDcols = c("revenu","wealth","Y", "UC")]
simul_household_contrefactuel_beta_no_risk[, `:=` (revenu = revenu/UC, wealth = wealth/UC, Y=Y/UC)]


# top 10% =========

p3 <- plot_top_shares2(simul_copy, threshold = 0.9, lang = "eng")

p3_household_top10 <- plot_top_shares2(simul_household, threshold = 0.9, lang = "eng")
p3_household_top10_no_risk <- plot_top_shares2(simul_household_no_risk, threshold = 0.9, lang = "eng")

p3_fr <- plot_top_shares2(simul_copy, threshold = 0.9, lang = "fr")
p3_household_fr <- plot_top_shares2(simul_household, threshold = 0.9, lang = "fr")

ggsave(plot = p3_household_fr, sprintf("./output_ret_soc_v3/top10.png", dir), width = 12, height = 8)
ggsave(plot = p3_household_fr, sprintf("./output_ret_soc_v3/top10.pdf", dir), width = 12, height = 8)
data.table::fwrite(p3$data, "./output_ret_soc_v3/top10.csv")

#ggsave(plot = p3_household_top10, sprintf("./output_ret_soc_v3/top10_DT.pdf", dir), width = 12, height = 8)
ggsave(plot = p3_household_top10_no_risk, sprintf("./output_ret_soc_v3/top10_no_risk_DT.pdf", dir), width = 12, height = 8)


p3_new_top10 = plot_top_share_together(
  p3_household_top10, 
  p3_household_top10_no_risk,
  0.9
)
ggsave(plot = p3_new_top10, "./output_ret_soc_v3/top10_DT.pdf", width = 12, height = 8)




# top 1% ==========

p3 <- plot_top_shares2(simul_copy, threshold = 0.99, lang = "eng")

p3_household_top1 <- plot_top_shares2(simul_household, threshold = 0.99, lang = "eng")
p3_household_top1_no_risk <- plot_top_shares2(simul_household_no_risk, threshold = 0.99, lang = "eng")

p3_fr <- plot_top_shares2(simul_copy, threshold = 0.99, lang = "fr")
p3_household_fr <- plot_top_shares2(simul_household, threshold = 0.99, lang = "fr")

ggsave(plot = p3_household_top1, sprintf("./output_ret_soc_v3/top1_DT.pdf", dir), width = 12, height = 8)
ggsave(plot = p3_household_fr, sprintf("./output_ret_soc_v3/top1.png", dir), width = 12, height = 8)
ggsave(plot = p3_household_fr, sprintf("./output_ret_soc_v3/top1.pdf", dir), width = 12, height = 8)
data.table::fwrite(p3$data, "./output_ret_soc_v3/top1.csv")

p2 <- plot_gini2(simul_copy)
p2_household <- plot_gini2(simul_household)
p2_fr <- plot_gini2(simul_copy, "French")
p2_household_fr <- plot_gini2(simul_household, "French")
ggsave(plot = p2_household, sprintf("./output_ret_soc_v3/gini_evolution_noneg_DT.pdf", dir), width = 12, height = 8)
ggsave(plot = p2_household_fr, sprintf("./output_ret_soc_v3/gini_evolution_noneg.png", dir), width = 12, height = 8)
ggsave(plot = p2_household_fr, sprintf("./output_ret_soc_v3/gini_evolution_noneg.pdf", dir), width = 12, height = 8)
data.table::fwrite(p2$data, "./output_ret_soc_v3/gini.csv")


p3_new_top1 = plot_top_share_together(
  p3_household_top1, 
  p3_household_top1_no_risk,
  0.99
)
ggsave(plot = p3_new_top1, "./output_ret_soc_v3/top1_DT.pdf", width = 12, height = 8)



# nviem_retraites/nviem_actifs -----------------

tar_load(simul)

retraites <- simul$retraites[, c("Id", "annee", "retraite_nette")]
retraites <- retraites[retraite_nette > 0]
retraites[, c("statut_ret") := "retraite"]


agg_nv <- create_table_nv(simul_copy, simul_household, median)
agg_nv_no_risk <- create_table_nv(simul_copy_no_risk, simul_household_no_risk, median)
agg_nv_contrefactuel_beta <- create_table_nv(simul_copy_contrefactuel_beta, simul_household_contrefactuel_beta, median)
agg_nv_contrefactuel_beta_no_risk <- create_table_nv(simul_copy_contrefactuel_beta_no_risk, simul_household_contrefactuel_beta_no_risk, median)

p_nv <- plot_nv_evol(agg_nv[annee<2050])
p_nv_no_risk <- plot_nv_evol(agg_nv_no_risk[annee<2050])
p_nv_contrefactuel_beta <- plot_nv_evol(agg_nv_contrefactuel_beta[annee<2050])
p_nv_contrefactuel_beta_no_risk <- plot_nv_evol(agg_nv_contrefactuel_beta_no_risk[annee<2050])

p <- plot_nv_together(p_nv, p_nv_no_risk)

ggplot2::ggsave(plot = p, filename = "./output_ret_soc_v3/standard_livings_DT.pdf",
                width = 13, height = 9)


p_contrefactuel_vs_baseline <- plot_nv_together(p_nv, p_nv_contrefactuel_beta)
p_contrefactuel_vs_no_risk <- plot_nv_together(p_nv_no_risk, p_nv_contrefactuel_beta)

p_contrefactuel_no_risk_vs_no_risk <- plot_nv_together(p_nv, p_nv_contrefactuel_beta_no_risk, labels = c("Baseline", "Same beta, no risk")) #effet incertitude
p_contrefactuel_no_risk_vs_no_risk2 <- plot_nv_together(p_nv, p_nv_contrefactuel_beta, labels = c("Baseline", "beta=0.992, uncertainty" )) #effet beta (avec incertitude)
p_contrefactuel_no_risk_vs_no_risk3 <- plot_nv_together(p_nv_no_risk, p_nv_contrefactuel_beta_no_risk, labels = c("Baseline, no risk", 'beta=1.005, no risk')) #effet beta (no incertitude)


# new figure: consumption path w/o uncertainty -------------

tar_load(data_consumption_no_risk)
tar_load(data_consumption)


p_no_risk <- plot_data_consumption(
  data_consumption_no_risk
)
p_uncertainty <- plot_data_consumption(
  data_consumption
)


p <- plot_consumption_together(p_uncertainty, p_no_risk)

ggplot2::ggsave(plot = p1,
                filename = "./output_ret_soc_v3/consumption_no_risk.pdf",
                width = 13, height = 9)
ggplot2::ggsave(plot = p2,
                filename = "./output_ret_soc_v3/consumption_risk.pdf",
                width = 13, height = 9)
ggplot2::ggsave(plot = p,
                filename = "./output_ret_soc_v3/consumption_DT.pdf",
                width = 13, height = 9)


# Table 1: data used for external validation ---------
# Pas besoin de R


# Table 2: table evolution richesse entre 2015 et 2018 --------

# simulations2[, c('retired') := data.table::fifelse(get('age') <= get("ageliq"),
#                                                   'active',
#                                                   'retired')]
# 
# EP_2015[, c('retired') := data.table::fifelse(get('SITUA')=="5",
#                                               'retired',
#                                               'active')]
# EP_lon[, c('retired') := data.table::fifelse(get('SITUA_2015')=="5",
#                                              'retired',
#                                              'active')]
# EP_lon[,'evol' := 100*(log(w_real_2018) - log(w_real_2015))]
# EP_lon[, c("inc_2015") := get("labor_income_2015") + exp(rnorm(nrow(EP_lon)))] #jitterize data
# EP_lon2 <- EP_lon[is.finite(evol) & is.finite(POND)]
# 
# EP_lon2[, c('decile_w') := cut(get("inc_2015"),
#                                quantile(get("inc_2015"),
#                                         probs= 0:10/10),
#                                labels = 1:10, include.lowest = TRUE
# )]
# 
# 
# note_eplon <- c("\\textit{Enquête Patrimoine 2014-2015 and 2017-2018}, statistics based on household head or spouse information. Variables are transformed into 2009 constant euros. Summary statistics are computed using 2015 survey weights.",
#                 "This table shows the distribution of individual wealth (household wealth divided by the number of spouses) growth between 2015 and 2018 on the whole sample of panelized individual.",
#                 "Labor income represents the sum of labor earnings, retirement pensions and unemployment benefits.")
# 
# 
# tablelight:::stack_summary(object = list(EP_lon2, EP_lon2, EP_lon2,
#                                          EP_lon2, EP_lon2[!is.na(tr_diplome)]), 
#                            x_vars = "evol", weight_vars = "POND",
#                            multirow_labels = c("Whole population","By sex:","By status:",
#                                                "By labor income decile:",
#                                                "By diploma level"),
#                            type = "dataframe",
#                            by = c(NA, "SEXE", "retired", "decile_w", "tr_diplome"),
#                            add.lines = note_eplon,
#                            caption = "Summary statistics on the evolution of wealth between 2015 and 2018 (growth rates in \\%)",
#                            label = "tab: summary stat EP 2015",
#                            add_rules = TRUE,
#                            stats = c("1Q", "median", 'mean','3Q','P90', "N")
# )


# Table 3: inheritance ------------------

cat(
  tablelight::light_table(
    list(probit_model, inheritance_model), type = "latex",
    title = "Heritage: models for outcome and selection",
    label = "tab:heritage",
    dep.var.labels = c("Inheritance probability", "Amount inherited (log)"),
    dep.var.separate = 1,
    column.labels = c("(\\textsc{Selection})", "(\\textsc{Outcome})"),
    # omit = names(probit$coefficients)[startsWith("factor(tr_ag", x = names(probit$coefficients))],
    covariate.labels = c(
      c(sprintf("Age between %s and %s",
                seq(25, 75, by = 5),
                seq(30, 80, by = 5)
      )),
      c(sprintf("Graduation age (%s or %s)",
                seq(14, 28, by = 2),
                seq(15, 29, by = 2)
      )
      ),
      "Graduation age (higher than 30)",
      'Sexe (reference : Male)',
      "Log income",
      "$\\log(\\sigma)$"
    ),
    stats.add = c("Controls for age & Yes & Yes",
                  "Controls for graduation age & Yes & Yes",
                  "Model & Probit & Interval regression"),
    add.lines = c(
      "Model estimated by interval regression (ordered probit regression with known thresholds) using declared received bequests in \\textit{Enquête Patrimoine 2015}",
      "Only the subset of individuals whom both parents are deceased is included in the sample"
    )
  ),
  sep = "\n"
)


# Table 4: simulated and observed distribution inheritance --------

inheritance_distrib <- data_prediction_selection[, .SD[1], by = "Id"]

tablelight:::summary_(
  inheritance_distrib[inheritance_distrib$H_given != 0], xvar = "hg",
  stats = c("1Q", 'median',"mean",'3Q','P90', 'N')
)


# Table 6: top income and wealth simulated shares --------

# niveau indiv ========

# simulations2[,'wealth_trunc' := pmax(0, get('wealth'))]
# simulations2[,'wealth_trunc' := get('wealth_trunc') + exp(rnorm(nrow(simulations)))]
# 
# sh_wealth <- share_total(simulations = simulations2[age>findet & annee == 2015],
#                          jitterize = TRUE,
#                          yvar = "wealth_trunc")
# sh_labor <- share_total(simulations = simulations2[age>findet & annee == 2015],
#                         jitterize = TRUE,
#                         yvar = "revenu")
# sh_income <- share_total(simulations = simulations2[age>findet & annee == 2015],
#                          jitterize = TRUE,
#                          yvar = "Y")
# 
# table_share <- merge(
#   merge(sh_labor, sh_income),
#   sh_wealth
# )[order(-Group)]
# 
# 
# 
# EP_2015[,'wealth_trunc' := pmax(0, get('PATRI_NET'))]
# EP_2015[,'wealth_trunc' := get('PATRI_NET') + exp(rnorm(nrow(EP_2015)))]
# 
# EP_2015[,'Y' := get("labor_income") + r*get("w_real")]
# 
# sh_wealth2 <- share_total(simulations = EP_2015[AGE > AGFINETU], yvar = "wealth_trunc")
# sh_labor2 <- share_total(simulations = EP_2015[AGE > AGFINETU], yvar = "labor_income", jitterize = TRUE)
# sh_income2 <- share_total(simulations = EP_2015[AGE > AGFINETU], yvar = "Y")
# 
# table_share2 <- merge(
#   merge(sh_labor2, sh_income2),
#   sh_wealth2
# )[order(-Group)]
# 
# 
# 
# data.table::setnames(table_share, old = c("revenu", "Y", "wealth_trunc"),
#                      new = c("Labor income","Total income",
#                              "Financial wealth"))
# data.table::setnames(table_share2, old = c("labor_income", "Y", "wealth_trunc"),
#                      new = c("Labor income","Total income",
#                              "Financial wealth"))
# 
# df <- do.call(c,
#               lapply(1:nrow(table_share), function(i) paste(format(table_share[i], digits=3L), collapse = " & "))
# )
# df2 <- do.call(c,
#                lapply(1:nrow(table_share2), function(i) paste(format(table_share2[i], digits=3L), collapse = " & "))
# )
# 
# latex_table <- c(
#   "\\begin{table}[ht]",
#   "\\centering",
#   "\\caption{Top income and wealth simulated shares (in \\%)}",
#   "\\label{tab: concentration}",
#   "\\begin{tabular}{lrrr}",
#   "\\hline",
#   "\\textsc{Group} & \\textsc{Labor income} & \\textsc{Total income} & \\textsc{Net wealth} \\\\",
#   "\\hline",
#   paste0(c("\\multicolumn{4}{c}{\\textsc{Wealth survey}}",
#            paste(df2, collapse = " \\\\ "),
#            "\\midrule",
#            "\\multicolumn{4}{c}{\\textsc{Simulated data}}",
#            paste(df, collapse = " \\\\ ")
#   ), " \\\\ "),
#   "\\bottomrule",
#   "\\multicolumn{4}{p{0.9\\linewidth}}{In this table, total income represents labor income (labor earnings, unemployment benefits and retirement pensions) plus financial income assuming a 3\\% return on observed or simulated wealth. Negative wealth are bottom-coded to zero in this Table} \\\\",
#   "\\multicolumn{4}{p{0.9\\linewidth}}{Wealth survey data comes from \\textit{Enquête Patrimoine 2014-2015}. Household financial wealth is individualized using number of spouses in the household.} \\\\",
#   "\\multicolumn{4}{p{0.9\\linewidth}}{Simulated data come from \\textit{Destinie} microsimulated data. Household labor income is individualized between spouses. Wealth is individual simulated wealth based on Section \\ref{sec:model} model and structural parameters derived from estimation}",
#   "\\end{tabular}",
#   "\\end{table}"
# )
# latex_table[latex_table == "\\midrule \\\\ "] <- "\\midrule"
# 
# cat(latex_table, sep  = "\n")


# niveau menage =================

simul_household[,'wealth_trunc' := pmax(0, get('wealth'))]
simul_household[,'wealth_trunc' := get('wealth_trunc') + exp(rnorm(nrow(simul_household)))]

simul_household_no_risk[,'wealth_trunc' := pmax(0, get('wealth'))]
simul_household_no_risk[,'wealth_trunc' := get('wealth_trunc') + exp(rnorm(nrow(simul_household)))]

table_share <- data.table::rbindlist(
  list(get_share(p3_household_top10$data,"Top 10 \\%")[],
       get_share(p3_household_top1$data)[]
  ))
table_share_no_risk <- data.table::rbindlist(
  list(get_share(p3_household_top10_no_risk$data,"Top 10 \\%")[],
       get_share(p3_household_top1_no_risk$data)[]
  ))

# sh_wealth <- share_total(simulations = simul_household,
#                          yvar = "wealth_trunc")
# sh_labor <- share_total(simulations = simulations,
#                         yvar = "revenu")
# sh_income <- share_total(simulations = simulations,
#                          yvar = "Y")
# 
# table_share <- merge(
#   merge(sh_labor, sh_income),
#   sh_wealth
# )[order(-Group)]


cols_wealth <- c('PATRI_NET','PATRI_BRUT',
                 'PATFI',
                 'PATFISOM','PATIMM',
                 'PATPROFENT','PATPROFHENT',
                 "MTDETTES")
cols_wealth <- c(cols_wealth,
                 "ZSALAIRES",  "ZRETRAITES", "ZCHOMAGE")

EP_2015_menages <- wealthyR:::read_EP(capitulation::macro,
                                      path_data = path_data,
                                      year = 2015,
                                      level = "household",
                                      .colsWealth = c('IDENT','AGEPR',
                                                      cols_wealth,
                                                      'NBUC','NPERS'))
EP_2015_menages[, 'labor_income' := ZSALAIRES + ZRETRAITES + ZCHOMAGE]
EP_2015_menages[, `:=` (labor_income = labor_income/NBUC, PATRI_NET = PATRI_NET/NBUC)]


EP_2015_menages[,'wealth_trunc' := pmax(0, get('PATRI_NET'))]
EP_2015_menages[,'wealth_trunc' := get('PATRI_NET') + exp(rnorm(nrow(EP_2015_menages)))]
EP_2015_menages[,'Y' := get("labor_income") + r*get("w_real")]

sh_wealth2 <- share_total(simulations = EP_2015_menages, yvar = "wealth_trunc")
sh_labor2 <- share_total(simulations = EP_2015_menages, yvar = "labor_income", jitterize = TRUE)
sh_income2 <- share_total(simulations = EP_2015_menages, yvar = "Y")

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
df_no_risk <- do.call(c,
              lapply(1:nrow(table_share_no_risk), function(i) paste(format(table_share_no_risk[i], digits=3L), collapse = " & "))
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
  "\\textsc{Group} & \\textsc{Labor income} & \\textsc{Total income} & \\textsc{Net wealth} \\\\",
  "\\hline",
  paste0(c("\\multicolumn{4}{c}{\\textsc{Wealth survey}}",
           paste(df2, collapse = " \\\\ "),
           "\\midrule",
           "\\multicolumn{4}{c}{\\textsc{Simulated data (mortality risk)}}",
           paste(df, collapse = " \\\\ "),
           "\\multicolumn{4}{c}{\\textsc{Simulated data (no mortality risk)}}",
           paste(df_no_risk, collapse = " \\\\ ")
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



# Table 5: SENSITIVITY ----

tar_load(output_uncertainty_selection)
tar_load(output_uncertainty_selection_gamma12)
tar_load(output_uncertainty_selection_gamma08)

tar_load(
  output_uncertainty_selection_gamma12_no_risk
)
tar_load(
  output_uncertainty_selection_gamma08_no_risk
)
tar_load(output_selection)


out <- output_uncertainty_selection_no_trans
#out$estimates$theta_hat <- mindist:::sigmoide(out$estimates$theta_hat)
#out$estimates$se_theta_hat <- out$estimates$se_theta_hat * mindist:::sigmoide_derivative(out$estimates$theta_hat)^2

out2 <- output_uncertainty_selection_gamma08
#out2$estimates$theta_hat <- mindist:::sigmoide(out2$estimates$theta_hat)
#out2$estimates$se_theta_hat <- out2$estimates$se_theta_hat * mindist:::sigmoide_derivative(out2$estimates$theta_hat)^2

out3 <- output_uncertainty_selection_gamma12
#out3$estimates$theta_hat <- mindist:::sigmoide(out3$estimates$theta_hat)
#out3$estimates$se_theta_hat <- out3$estimates$se_theta_hat * mindist:::sigmoide_derivative(out3$estimates$theta_hat)^2

out_no_risk <- output_selection
#out$estimates$theta_hat <- mindist:::sigmoide(out$estimates$theta_hat)
#out$estimates$se_theta_hat <- out$estimates$se_theta_hat * mindist:::sigmoide_derivative(out$estimates$theta_hat)^2

out2_no_risk <- output_uncertainty_selection_gamma08_no_risk
#out2$estimates$theta_hat <- mindist:::sigmoide(out2$estimates$theta_hat)
#out2$estimates$se_theta_hat <- out2$estimates$se_theta_hat * mindist:::sigmoide_derivative(out2$estimates$theta_hat)^2

out3_no_risk <- output_uncertainty_selection_gamma12_no_risk
#out3$estimates$theta_hat <- mindist:::sigmoide(out3$estimates$theta_hat)
#out3$estimates$se_theta_hat <- out3$estimates$se_theta_hat * mindist:::sigmoide_derivative(out3$estimates$theta_hat)^2

cat(
  tablelight::light_table(
    list(out2, out, out3),
    add.lines = paste0(
      "Model estimated by minimum distance using aggregate moments ",
      "from microsimulated data and wealth surveys. ",
      "Moments in wealth survey are defined in Table \\ref{tab:data}.",
      " The same moments are computed in microsimulated data."
    ),
    dep.var.labels = "Exogeneous risk aversion coefficient $\\gamma$",
    column.labels = c("$\\gamma$=0.8","$\\gamma$ = 1","$\\gamma=1.2$"),
    stats.var.separate = 3,
    title = "Estimated parameters for different values of $\\gamma$",
    label = "tab: robustness r GMM",
    covariate.labels = "$\\beta$"
  ),
  sep = "\n"
)
