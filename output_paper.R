rm(list = ls())

# _____________________________________________
# PART 1: CONSTRUCT LONGITUDINAL PANEL ----
# _____________________________________________

# ++++++++++++++++++++++++++++++++++++++++++
# A/ READ HOUSEHOLD LEVEL DATA ========
# ++++++++++++++++++++++++++++++++++++++++++


population <- readRDS(file = "tempfile.rds")
data <- readRDS("data.rds")
EP_2015 <- data[['EP_2015']]
EP_2018 <- data[['EP_2018']]
EP_lon <- data[['EP_lon']]


output <- readRDS("output.rds")

beta  <- output$estimates$theta_hat['beta']
gamma <- output$estimates$theta_hat['gamma']
r <- 0.03

# _____________________________________________
# PART 2: ESTIMATION ----
# _____________________________________________


# SIMULATE MODEL -----------------------------


simulations <- capitulation::life_cycle_model(
  population,
  wealthvar_survey = "K_observed",
  r = r,
  beta = beta,
  gamma = gamma,
  non_ricardian = TRUE,
  observation_year = 2009,
  income_var = "revenu",
  Hgiven_var = "hg",
  Hreceived_var = "hr",
  return_last = FALSE,
  get_capital_income = TRUE,
  additional_vars = c("tr_age_2015","sexe","findet"))


# ARRANGE DATA ---------------------------------

clean_data <- function(data, sex_var = "sexe",
                       diploma_var = "findet",
                       labor_income_var = "revenu",
                       total_income_var = "Y",
                       statut_var = NULL,
                       year = 2015){
  
  
  data[, c('SEXE') := data.table::fifelse(get(sex_var)==1,
                                          'Male',
                                          'Female')]
  data[, c('tr_diplome') := cut(get(diploma_var), breaks = c(min(get(diploma_var)), 16,18,21,25, max(get(diploma_var))), include.lowest = TRUE)]
  
  if (year != 2015) return(data)
  
  data[, c(labor_income_var) := get(labor_income_var) + exp(rnorm(nrow(data)))]
  data[, c(total_income_var) := get(total_income_var) + exp(rnorm(nrow(data)))]
  
  data[, c('decile_w') := cut(get(labor_income_var),
                              quantile(get(labor_income_var),
                                       probs= 0:10/10, na.rm = TRUE),
                              labels = 1:10, include.lowest = TRUE
  )]
  data[, c('decile_y') := cut(get(total_income_var),
                              quantile(get(total_income_var),
                                       probs= 0:10/10, na.rm = TRUE),
                              labels = 1:10, include.lowest = TRUE
  )]
  
  if (is.null(statut_var)) return(data)
  
  data[,'retired' := data.table::fifelse(get(statut_var) == "5",
                                         "retired","active")]
  
  return(data)  
}

simulations <- simulations[age > findet]

clean_data(simulations)

EP_2015[,'y' := get('labor_income') + r*get('PATFISOM')]
EP_2015[, 'annee' := 2015]
clean_data(EP_2015, sex_var = "SEXE", labor_income_var = "labor_income", diploma_var = "AGFINETU",
           total_income_var = "y")

EP_2018[, 'annee' := 2018]
clean_data(EP_2018, sex_var = "SEXE", year = 2018, diploma_var = "AGFINETU",
           statut_var = "SITUA")


EP_lon[,'y' := get('labor_income') + r*get('PATFISOM_2015')]
EP_lon <- merge(EP_lon, EP_2015[,.SD,.SDcols = c("IDENTIND14","tr_diplome", "decile_w", "decile_y")],
                by = c("IDENTIND14"))
EP_lon[, c('SEXE') := data.table::fifelse(get('SEXE')==1,
                                          'Male',
                                          'Female')]


# TABLES SUMMARY STATS ----------------------

# Enquete Patrimoine ========================

note <- c("\\textit{Enquête Patrimoine 2014-2015}, statistics based on household head or spouse information. Variables are transformed into 2009 constant euros. Summary statistics are computed using survey weights.",
          "This table shows the distribution of individual wealth (household wealth divided by the number of spouses) in 2015 on the whole sample (panelized and non-panelized individuals in wealth survey)",
          "Labor income represents the sum of labor earnings, retirement pensions and unemployment benefits.",
          "Wealth is household wealth divided by the number of spouses.")

cat(
  tablelight:::stack_summary(object = list(EP_2015, EP_2015, EP_2015), x_vars = "PATFISOM", weight_vars = "POND",
                             multirow_labels = c("Whole population","By sex:","By labor income decile:"),
                             by = c(NA, "SEXE", "decile_w"),
                             add.lines = note,
                             caption = "Summary statistics on 2015 wealth survey (\\textit{Enquête Patrimoine})",
                             label = "tab: summary stat EP 2015",
                             add_rules = TRUE,
                             stats = c("1Q", "mean", 'median','3Q','P90','N')
  ),
  sep = "\n",
  file = "./tables/summary_wealth_EP15.tex"
)

# Enquete Patrimoine ========================

# note <- c("\\textit{Enquête Patrimoine 2014-2015}, statistics based on household head or spouse information. Variables are transformed into 2009 constant euros. Summary statistics are computed using survey weights.",
#           "Labor income represents the sum of labor earnings, retirement pensions and unemployment benefits.",
#           "Wealth is household wealth divided by the number of spouses.")

EP_lon[,'evol' := 100*(log(w_real_2018) - log(w_real_2015))]
EP_lon[, c("inc_2015") := get("labor_income") + exp(rnorm(nrow(EP_lon)))]
EP_lon2 <- EP_lon[is.finite(evol) & is.finite(POND)]

EP_lon2[, c('decile_w') := cut(get("inc_2015"),
                               quantile(get("inc_2015"),
                                        probs= 0:10/10),
                               labels = 1:10, include.lowest = TRUE
)]

note_eplon <- c("\\textit{Enquête Patrimoine 2014-2015 and 2017-2018}, statistics based on household head or spouse information. Variables are transformed into 2009 constant euros. Summary statistics are computed using 2015 survey weights.",
                "This table shows the distribution of individual wealth (household wealth divided by the number of spouses) growth between 2015 and 2018 on the whole sample of panelized individual.",
                "Labor income represents the sum of labor earnings, retirement pensions and unemployment benefits.")


cat(
  tablelight:::stack_summary(object = list(EP_lon2, EP_lon2, EP_lon2), x_vars = "evol", weight_vars = "POND",
                             multirow_labels = c("Whole population","By sex:","By labor income decile:"),
                             by = c(NA, "SEXE", "decile_w"),
                             add.lines = note_eplon,
                             caption = "Summary statistics on the evolution of wealth between 2015 and 2018 (growth rates in \\%)",
                             label = "tab: summary stat EP 2015",
                             add_rules = TRUE,
                             stats = c("1Q",
                                       "median", 'mean','3Q','P90', "N"
                             )
  ),
  sep = "\n",
  file = "./tables/summary_wealth_EPlon.tex"
)


# Microsimulation ===========================

note2 <- c("Results are based on Destinie microsimulation model for 2015. Variables are transformed into 2009 constant euros.",
           "Labor income represents the sum of labor earnings, retirement pensions and unemployment benefits.",
           "Wealth is individual simulated wealth based on Section \\ref{sec:model} model and structural parameters derived from estimation")

cat(
  tablelight:::stack_summary(object = list(simulations[annee==2015 & age>findet],
                                           simulations[annee==2015 & age>findet],
                                           simulations[annee==2015 & age>findet]), x_vars = "wealth",
                             multirow_labels = c("Whole population","By sex:","By labor income decile:"),
                             by = c(NA, "SEXE", "decile_w"),
                             add.lines = note2,
                             caption = "Summary statistics predicted by our model (year : 2015)",
                             label = "tab: summary stat microsimulated wealth",
                             add_rules = TRUE,
                             stats = c("1Q", "mean", 'median','3Q','P90')
  ),
  sep = "\n",
  file = "./tables/summary_wealth_microsimulation.tex"
)


# INEQUALITIES ----------------------------

yvar <- "wealth"

share_total <- function(simulations, yvar = "wealth", jitterize = FALSE){
  
  year_2015 <- data.table::copy(simulations)
  
  if (jitterize){
    year_2015[, c(yvar) := get(yvar) + exp(rnorm(nrow(year_2015)))]
  }
  
  year_2015[, c('decile_wealth') := cut(get(yvar),
                                        quantile(get(yvar),
                                                 probs= 0:10/10),
                                        labels = 1:10, include.lowest = TRUE
  )]
  year_2015[, c('percentile_wealth') := cut(get(yvar),
                                            quantile(get(yvar),
                                                     probs= 0:100/100),
                                            labels = 1:100, include.lowest = TRUE
  )]
  year_2015 <- year_2015[,.(sh_wealth = sum(get(yvar))),
                         by = "percentile_wealth"]
  
  year_2015[,c('top_10','top_1') := list(as.numeric(as.character(get('percentile_wealth')))>90,
                                         as.numeric(as.character(get('percentile_wealth'))) == 100)]
  
  top10 <- year_2015[,.(sh_wealth = sum(sh_wealth)), by = top_10]
  top1 <- year_2015[,.(sh_wealth = sum(sh_wealth)), by = top_1]
  
  top10[,sh_wealth := sh_wealth/sum(sh_wealth)]
  top1[,sh_wealth := sh_wealth/sum(sh_wealth)]
  
  top10 <- 100*as.numeric(top10[get("top_10")][['sh_wealth']])
  top1 <- 100*as.numeric(top1[get("top_1")][['sh_wealth']])
  
  df <- data.table::data.table("Group" = c("Top 10 \\%", "Top 1 \\%"),
                               "yvar" = c(top10, top1))
  
  data.table::setnames(df, old = "yvar", new = yvar)
  
  return(
    df
  )
}

simulations[,'wealth_trunc' := pmax(0, get('wealth'))]
simulations[,'wealth_trunc' := get('wealth_trunc') + exp(rnorm(nrow(simulations)))]

sh_wealth <- share_total(simulations = simulations[age>findet & annee == 2015], yvar = "wealth_trunc")
sh_labor <- share_total(simulations = simulations[age>findet & annee == 2015], yvar = "revenu")
sh_income <- share_total(simulations = simulations[age>findet & annee == 2015], yvar = "Y")

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

cat(latex_table, sep = "\n",
    file = "./tables/topshares.tex")


share_total(pre_indiv[annee == 2015], yvar = "salaire", jitterize = TRUE)
share_total(population[annee == 2015], yvar = "y_indiv", jitterize = TRUE)
share_total(indiv[annee == 2015 & age > findet], yvar = "salaire", jitterize = TRUE)
share_total(simulations[annee == 2015], yvar = "revenu", jitterize = TRUE)


EP_2015_bis <- individualize_EP(path_data = "~", individualize_income = TRUE)
EP_2015_bis[,'labor_income' := ZSALAIRES + ZRETRAITES + ZCHOMAGE]
EP_2015_bis[,'wealth_trunc' := pmax(0, get('w_real'))]
EP_2015_bis[,'wealth_trunc' := get('w_real') + exp(rnorm(nrow(EP_2015_bis)))]
EP_2015_bis[,'Y' := get("labor_income") + r*get("w_real")]
sh_wealth3 <- share_total(simulations = EP_2015_bis[AGE > AGFINETU], yvar = "wealth_trunc")
sh_labor3 <- share_total(simulations = EP_2015_bis[AGE > AGFINETU], yvar = "labor_income", jitterize = TRUE)
sh_income3 <- share_total(simulations = EP_2015_bis[AGE > AGFINETU], yvar = "Y")

table_share3 <- merge(
  merge(sh_labor3, sh_income3),
  sh_wealth3
)[order(-Group)]

df3 <- do.call(c,
               lapply(1:nrow(table_share3), function(i) paste(format(table_share3[i], digits=3L), collapse = " & "))
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
           paste(df3, collapse = " \\\\ "),
           "\\midrule",
           "\\multicolumn{4}{c}{\\textsc{Simulated data}}",
           paste(df, collapse = " \\\\ ")
  ), " \\\\ "),
  "\\bottomrule",
  "\\multicolumn{4}{p{0.9\\linewidth}}{In this table, total income represents labor income (labor earnings, unemployment benefits and retirement pensions) plus financial income assuming a 3\\% return on observed or simulated wealth. Negative wealth are bottom-coded to zero in this Table} \\\\",
  "\\multicolumn{4}{p{0.9\\linewidth}}{Wealth survey data comes from \\textit{Enquête Patrimoine 2014-2015}. Household financial wealth and labor income are individualized using number of spouses in the household.} \\\\",
  "\\multicolumn{4}{p{0.9\\linewidth}}{Simulated data come from \\textit{Destinie} microsimulated data. Household labor income is individualized between spouses. Wealth is individual simulated wealth based on Section \\ref{sec:model} model and structural parameters derived from estimation}",
  "\\end{tabular}",
  "\\end{table}"
)
latex_table[latex_table == "\\midrule \\\\ "] <- "\\midrule"

cat(latex_table, sep = "\n",
    file = "./tables/topshares.tex")


# MODELE HERITAGE -------------------------

# REGRESSION TABLE ========================

model <- readRDS("./modele.rds")
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
  sep = "\n",
  file = "./tables/heritage.tex"
)

# INHERITANCE PREDICTED =================

inheritance_distrib <- population[, .SD[1], by = "Id"]


print(xtable::xtable(tablelight:::summary_(
  inheritance_distrib[inheritance_distrib$H_given != 0], xvar = "hg",
  stats = c("1Q", 'median',"mean",'3Q','P90')),
  caption = "Heritage: simulated distribution",
  label = "tab:heritage_simu"),
  type = "latex",
  include.rownames=FALSE,
  caption.placement = "top",
  file = "./tables/simulated_heritage.tex")


# Figure des histogrammes: inheritance predicted vs observed
# cf. script exploreep/inheritance.R


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

estim_model <- function(form = "MTHER ~ lw + age + I((age^2)/100) + AGFINETU + I((AGFINETU^2)/100)",
                        formulaSD = NULL,
                        optmeth = "NR",
                        thresholds = lbounds,
                        search_iter = 10){
  
  estim_data <- estim_data[order(MTHER)] 
  
  # inheritance_model <- REtage::ordered_model_threshold(
  #   data = data.frame(estim_data),
  #   formula = form,
  #   formulaSD = formulaSD,
  #   link = "probit",
  #   constantSD = TRUE,
  #   thresholds = lbounds,
  #   optmeth = optmeth
  # )
  
  inheritance_model <- oglm::oglmx(
    formulaMEAN = form,
    formulaSD = formulaSD,
    data = estim_data,
    threshparam = thresholds,
    start_method = "search",
    search_iter = search_iter
  )
  
  
  estim_data[, pred := predict(inheritance_model, estim_data,
                               type ="latent")$y_latent_pred]
  estim_data[, pred_cut := cut(pred, c(-Inf, lbounds, Inf),
                               labels = order(unique(MTHER)))]
  
  library(ggplot2)
  
  confusion_matrix <- as.data.frame(table(estim_data$pred_cut, estim_data$MTHER))
  
  p1 <- ggplot(data = confusion_matrix,
               mapping = aes(x = Var1,
                             y = Var2)) +
    geom_tile(aes(fill = Freq)) +
    geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
    scale_fill_viridis_c(trans = "log", labels = scales::number_format(accuracy = 1),
                         option = "plasma") +
    scale_y_discrete(limits = rev(levels(confusion_matrix$Var2))) +
    labs(x = "Predicted", y = "Actual")
  
  
  confusion_matrix2 <- as.data.frame(table(estim_data$pred_cut, estim_data$MTHER))
  data.table::setDT(confusion_matrix2)
  confusion_matrix2[,'Freq2' := as.numeric(Freq)/sum(as.numeric(Freq)), by = Var2]
  
  p2 <- ggplot(data = confusion_matrix2,
               mapping = aes(x = Var1,
                             y = Var2)) +
    geom_tile(aes(fill = Freq2)) +
    geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
    scale_fill_viridis_c(trans = "log", labels = scales::percent_format(accuracy = 1),
                         option = "plasma") +
    scale_y_discrete(limits = rev(levels(confusion_matrix$Var2))) +
    labs(x = "Predicted", y = "Actual")
  
  
  tempdata <- estim_data[,lapply(.SD, function(x) as.numeric(as.character(x))),.SDcols = c("MTHER", "pred_cut")]
  tempdata <- data.table::melt(tempdata)
  p3 <- ggplot(tempdata) +
    geom_histogram(aes(x = value, fill = variable),
                   position = position_dodge(preserve = "single")) +
    scale_fill_viridis_d()
  
  accur <- sum(confusion_matrix2[Var1 == Var2]$Freq)/sum(confusion_matrix2$Freq)
  # print(sprintf("Accuracy: %1.2f%%", 100*accur))
  
  return(list(p1,p2,p3,'model' = inheritance_model, "accuracy" = accur))
}

mod5 = estim_model("MTHER ~ lw + tr_age + SEXE + tr_agfinetu",
                   formulaSD = NULL)

ggplot2::ggsave(plot = mod5[[3]], filename = "./pics/inheritance_predicted.pdf",
                width = 13, height = 9)




# COMPARE FIT ====================================

# EP_data <- wealthyR:::prepare_inheritance_sample(
#   path_survey =  "~/Enquete Patrimoine"
# )
#
# inheritance_data <- REtage::prepare_estimation(EP_data)
#
#
# bounds <- c(3,8,15,30,60,100,150,200,250)*1000
# lbounds <- log(bounds)
#
# estim_data <- inheritance_data[get('revenu')>0]
#
# estim_data[,'MTHER' := as.numeric(as.character(MTHER))]
# estim_data <- estim_data[order(MTHER)]
# estim_data[,'age' := get('AGE')]
#
# inheritance_model <- REtage::ordered_model_threshold(
#   data = data.frame(estim_data[order(MTHER)]),
#   formula = "MTHER ~ lw + age + I((age^2)/100) + AGFINETU + I((AGFINETU^2)/100)",
#   link = "probit",
#   constantSD = TRUE,
#   thresholds = lbounds
# )
#
# predicted_dist <- predict(inheritance_model, newdata = estim_data)
#
#
# ggplot2::ggplot(data.frame(x = as.numeric(as.character(predicted_dist)))) +
#   ggplot2::geom_histogram(ggplot2::aes(x = x, y = ..density..))



# GRAPHIQUES -----------------------------


p_K <- capitulation::plot_K_age(simulations, method = "smooth",
                                xlims = c(30,75))

p_K <- p_K +
  #ggplot2::scale_y_continuous(limits = c(-100000, 150000)) +
  #ggplot2::scale_x_continuous(limits = c(25, 90)) +
  ggplot2::scale_color_viridis_d()

ggplot2::ggsave(plot = p_K, filename = "./pics/02_calibration.pdf",
                width = 13, height = 9)


p <- capitulation::plot_rK_age(simulations, scale_viridis = TRUE,
                               langage = "English",
                               xlims = c(30,75))

ggplot2::ggsave(plot = p, filename = "./pics/02_calibration_rK.pdf",
                width = 13, height = 9)


# MOMENT 1 =============================

moment1 <- gridExtra::grid.arrange(
  wealthyR::plot_moment_age(EP_2015, EP_2018, simulations = simulations,
                            scale_moment = "share",
                            by_survey = "AGE", by_simulation = 'age', scale_variable  = "log")$fit[[1]] +
    ggplot2::scale_fill_manual(values = c('microsimulation' = 'black',
                                          'survey' = 'royalblue')) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "top"),
  wealthyR::plot_moment_age(EP_2015, EP_2018, simulations = simulations,
                            by_survey = "AGEPR", by_simulation = 'age', scale_variable = "log")$fit[[2]]
)

ggplot2::ggsave(plot = moment1, "./pics/moment1.pdf", width = 18, height = 20)


# MOMENT 2 =============================

moment2 <- wealthyR::plot_moment_dK(
  EP_lon = EP_lon, simulations = simulations,
  scale = "log", by = "tr_age_2015"
) +
  ggplot2::scale_color_manual(values = c('simulation' = 'black',
                                         'survey' = 'royalblue')) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom")


ggplot2::ggsave(plot = moment2, "./pics/moment2.pdf", width = 12, height = 8)

# LORENZ =====

p <- capitulation:::plot_lorenz(simulations)

p <- ggplot2::ggplot(p$data[variable != "total"]) + ggplot2::geom_line(ggplot2::aes(x = p1,
                                                                                    y = value, color = lab)) + ggplot2::theme(legend.position = "bottom") +
  ggplot2::labs(x = "", y = "", color = "") + ggplot2::theme(text = ggplot2::element_text(size = 24),
                                                             axis.title = ggplot2::element_text(size = 20, face = "bold"))

ggplot2::ggsave(plot = p, "./pics/lorenz.pdf", width = 12, height = 12)


# FIT QUALITY ----------


