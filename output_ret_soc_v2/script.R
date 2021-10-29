library(tablelight)
library(data.table)
library(ggplot2) 

source("functions.R")


# MODELE HERITAGE ------------------------------------

EP_data <- wealthyR:::prepare_inheritance_sample(
  path_survey =  "../Enquete Patrimoine"
)
EP_data <- EP_data[(MER1E == 3 & PER1E==3) | (!is.na(MTHER))]


inheritance_data <- REtage::prepare_estimation(EP_data, taille_tr_age = 5,
                                               taille_tr_agfinetu = 2)


bounds <- c(3,8,15,30,60,100,150,200,250)*1000
lbounds <- log(bounds)


estim_data <-data.table::copy(inheritance_data)#[get('income')>0]

estim_data[,'MTHER' := as.numeric(as.character(MTHER))]
estim_data[,'age' := get('AGE')]


estim_data <- estim_data[get('age') < 80]
estim_data <- estim_data[get('income')>0]
estim_data <- estim_data[get('age') > 20]

estim_data[, c('N_heritiers') := .N, by = c("annee","IDENT","IDENTTRANS")]
estim_data[is.na(get("MTHER")), c("MTHER") := 0]
estim_data[, inherited := (MTHER != 0) ]
estim_data$inherited <- factor(as.numeric(estim_data$inherited))
estim_data <- estim_data[order(MTHER)]

estim_data <- na.omit(estim_data, cols = c("inherited","tr_age","tr_agfinetu","SEXE", "lw", "MTHER"))


## SELECTION MODEL ========================


probit <- glm(inherited ~ factor(tr_age) + factor(tr_agfinetu),
              family = binomial(link = "probit"), 
              data = estim_data)
summary(probit)

pred_selection <- predict(probit, type = "link")
pred_selection <- pred_selection + rnorm(length(pred_selection), sd = sd(probit$residuals))
pred_selection <- as.numeric(pred_selection > 0)


confusion_first_step <- data.frame(
  prediction = pred_selection,
  observation = as.numeric(estim_data$inherited)
)
table(confusion_first_step)


## MODEL OUTCOME ==========================


inheritance_model <-  oglm::oglmx(
  data = data.frame(estim_data[MTHER>0]),
  link = "probit",
  formulaMEAN = "MTHER ~ factor(SEXE) + lw + factor(tr_age) + factor(tr_agfinetu)",
  constantSD = TRUE,
  threshparam = lbounds,
  start_method = "search"
)
class(inheritance_model) <- c("oglm","oglmx")
summary(inheritance_model)

inheritance_model <-  oglm::oglmx(
  data = data.frame(estim_data[MTHER>0]),
  link = "probit",
  formulaMEAN = "MTHER ~ factor(SEXE) + lw + factor(tr_age) + factor(tr_agfinetu)",
  constantSD = TRUE,
  threshparam = lbounds,
  start_method = "search"
)
class(inheritance_model) <- c("oglm","oglmx")
summary(inheritance_model)


# PREDICT =========================

prediction_2step <- REtage:::predict_two_steps(probit = probit,
                                               intreg = inheritance_model, estim_data, scale = "class", lbounds = lbounds)

prediction <- prediction_2step


confusion <- data.frame('Observed' = estim_data$MTHER, 'Predicted' = prediction_2step$prediction, "rn" = seq_len(nrow(estim_data)))
confusion <- data.table::melt(data.table::setDT(confusion), id.vars = "rn")



p <- ggplot(confusion) +
  geom_histogram(aes(x = factor(value),fill = variable), stat = "count", alpha=0.6, position = 'dodge') +
  scale_x_discrete(labels= c("No inheritance", REtage:::get_labs(lbounds))) +
  theme(legend.title=element_blank(), legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust=1)) +
  labs(x = NULL, y = 'Number of individuals')
p <- p +
  ggplot2::labs(y = "Nombre d'individus",
                x = "Valeur héritée, en tranches") +
  ggplot2::scale_fill_manual(breaks = c("Observed","Predicted"),
                             values=c("#69b3a2", "#404080"),
                             labels = c("Observée",
                                        "Simulée")) +
  labs(fill = NULL) +
  theme(text = element_text(size=20),
        axis.text=element_text(size=28))

ggplot2::ggsave(plot = p, filename = "./output_ret_soc_v2/fig01_inheritance_predicted.pdf",
                width = 13, height = 9)
ggplot2::ggsave(plot = p, filename = "./output_ret_soc_v2/fig01_inheritance_predicted.png",
                width = 13, height = 9)



# MICROSIMULATION --------------------------------


path_data <- ".."

data <- construct_EP(path_data)
EP_2015 <- data[['EP_2015']]
EP_2018 <- data[['EP_2018']]
EP_lon <- data[['EP_lon']]


# data_prediction <- capitulation::prepare_data(
#   path_data = "..",
#   inheritance_model = inheritance_model,
#   selection_model = probit,
#   time_0 = "birth",
#   # debt_wealthSurvey = "MTDETTES",
#   taille_tr_age = 5,
#   taille_tr_agfinetu = 2,
#   path_data_suffix = "/Destinie2120",
#   extension = ".rda",
#   wealthvar_survey = "PATRI_NET"
# )
# 
# data_prediction[is.na(non_ricardian), c("non_ricardian") := get("H_given")>0]
# mean(data_prediction[,.SD[1], by = "Id"]$non_ricardian)
# menages_structural2 <- data.table::copy(data_prediction)
# menages_structural2[,'hg' := get('H_given')]
# menages_structural2[,'hr' := get('H_received')]
# menages_structural2[,'tr_age_2015' := floor(get("age")/5)*5]
# saveRDS(menages_structural2, 'start_estimation.rds')


menages_structural2 <- readRDS('start_estimation.rds')



EP_2015[,'tr_age_2015' := floor(get("AGE")/5)*5]



beta  <- 0.992
gamma <- 1
r <- 0.03



# SIMULATE MODEL -----------------------------


simulations <- capitulation::life_cycle_model(
  menages_structural2,
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
  additional_vars = c("tr_age_2015","tr_age","SEXE","findet","ageliq"))

simulations <- simulations[age > findet]
simulations[,'sexe' := "SEXE"]

clean_data2(simulations)

EP_2015[,'y' := get('labor_income') + r*get('PATFISOM')]
EP_2015[, 'annee' := 2015]
clean_data2(EP_2015, sex_var = "SEXE", labor_income_var = "labor_income", diploma_var = "AGFINETU",
            total_income_var = "y")

EP_2018[, 'annee' := 2018]
clean_data2(EP_2018, sex_var = "SEXE", year = 2018, diploma_var = "AGFINETU",
            statut_var = "SITUA")


EP_lon[,'y' := get('labor_income_2015') + r*get('PATFISOM_2015')]
EP_lon <- merge(EP_lon, EP_2015[,.SD,.SDcols = c("IDENTIND14","tr_diplome", "decile_w", "decile_y")],
                by = c("IDENTIND14"))
EP_lon[, c('SEXE') := data.table::fifelse(get('SEXE')==1,
                                          'Male',
                                          'Female')]




kage <- plot_K_age2(simulations[age>findet],EP_2015, method = "median", has_ricardian = FALSE)
kage <- kage + 
  ggplot2::scale_color_viridis_d() +
  ggplot2::labs(y = "Richesse simulée (en milliers d'euros)", x = "Age") +
  theme(text = element_text(size=20),
        axis.text=element_text(size=28)) +
  ggplot2::scale_linetype_manual(breaks = c(FALSE,TRUE),
                                 values=c("solid", "dashed"),
                                 labels = c("Simulée",
                                            "Observée (patrimoine 2015)"))  +
  guides(color = "none", linetype = guide_legend(title="Source")) +
  theme(legend.position="top")

ggplot2::ggsave(plot = kage, filename = "./output_ret_soc_v2/fig01_kage2.pdf",
                width = 13, height = 9)




df_moment2 <- wealthyR:::create_moment_data(EP_2015 = EP_2015, EP_2018 = EP_2018,
                                            EP_lon = EP_lon, 
                                            data_microsimulated = menages_structural2,
                                            observed_moment_data = NULL,
                                            r = r,
                                            gamma = gamma,
                                            beta = beta,
                                            r.parameters = NULL,
                                            gamma.parameters = NULL,
                                            beta.parameters = NULL,
                                            r_low = r,
                                            r_high = r,
                                            # non_ricardian = TRUE,
                                            # non_ricardian_var = "non_ricardian",
                                            N_moments = 180,
                                            wealth_var = "PATRI_NET",
                                            age_var_simulations = "age",
                                            normalize = FALSE,
                                            scale_model = "log",
                                            scale_variable_moment1 = "asinh",
                                            scale_variable_moment2 = "log",
                                            scale_moment1 = "level",                               
                                            moment1 = "level",
                                            stat_moment2 = "difference",
                                            ages = c(30,65),
                                            exclude_negative = FALSE,
                                            Hgiven_var = "hg",
                                            Hreceived_var = "hr",
                                            additional_vars = c("tr_age","SEXE","tr_agfinetu","findet", "non_ricardian"),
                                            by = c("tr_age_2015", "tr_age_2015"))


plot_moment_age_wide <-function(df_moment2, ages = c(30,65)){
  
  tempdat <- data.table::copy(df_moment2)
  tempdat[, 'cumsum' := cumsum(weight)]
  tempdat[, 'moment' := 1 + as.numeric(cumsum>1)]
  tempdat[, c("cumsum") := NULL]
  
  tempdat <- split(tempdat, by = "moment")
  tempdat <- lapply(tempdat, data.table::melt, id.vars = c("Nmoment", "moment"))
  
  
  m1 <- ggplot2::ggplot(tempdat[["1"]][variable != "weight"]) +
    ggplot2::geom_bar(ggplot2::aes(x = Nmoment, y = value,
                                   fill = variable), position = "dodge",
                      stat='identity', width=.5) +
    ggplot2::labs(x = "Moment", y = "Median wealth") +
    ggplot2::scale_fill_manual(values = c('moment_simulations' = 'black',
                                          'moment_data' = 'royalblue'),
                               labels = c("Simulée","Observée")) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse=TRUE)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "top") +
    ggplot2::labs(y = "Part du patrimoine de la\nclasse d'âge dans le total",
                  fill = NULL) +
    ggplot2::theme(text = ggplot2::element_text(size=28),
                   axis.text=ggplot2::element_text(size=28))
  
  m1b <- ggplot2::ggplot(tempdat[["1"]][variable == "weight"]) +
    ggplot2::geom_bar(ggplot2::aes(x = Nmoment, y = value), stat = 'identity') +
    ggplot2::geom_point(ggplot2::aes(x = Nmoment, y = value), color = 'red') +
    ggplot2::labs(y = "Density", x = "Moment") +
    ggplot2::scale_y_reverse()
  
  
  m2 <- ggplot2::ggplot(tempdat[["2"]][variable != "weight"], ggplot2::aes(x = Nmoment - 13,
                                                                           y = value,
                                                                           color = variable,
                                                                           shape = variable,
                                                                           group = variable)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0L) +
    ggplot2::labs(title = "3-year rolling growth rate",
                  x = 'Age in wave 1',
                  y = "Moment") +
    ggplot2::scale_color_manual(values = c('moment_simulations' = 'black',
                                           'moment_data' = 'royalblue'),
                                labels = c("Simulée","Observée")) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(y = "Evolution sur trois ans\ndu patrimoine financier",
                  x = "Age en 2015", title = "") +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::theme(text = ggplot2::element_text(size=28),
                   axis.text=ggplot2::element_text(size=28)) +
    ggplot2::theme(legend.title=ggplot2::element_blank())
  
  m2b <- ggplot2::ggplot(tempdat[["2"]][variable == "weight"]) +
    ggplot2::geom_bar(ggplot2::aes(x = Nmoment - 13, y = value), stat = 'identity') +
    ggplot2::geom_point(ggplot2::aes(x = Nmoment - 13, y = value), color = 'red') +
    ggplot2::labs(y = "Density", x = "Moment") +
    ggplot2::scale_y_reverse()
  
  return(
    list(m1, m1b, m2, m2b)
  )
  
}

plots <- plot_moment_age_wide(df_moment2)
moment1 <- gridExtra::grid.arrange(plots[[1]], plots[[2]])
moment2 <- gridExtra::grid.arrange(plots[[3]], plots[[4]])

ggplot2::ggsave(plot = moment1, "./output_ret_soc_v2/fig03_moment1.pdf",
                width = 18, height = 20)
ggplot2::ggsave(plot = plots[[1]], "./output_ret_soc_v2/fig03_moment1_part1.pdf",
                width = 18, height = 20)
ggplot2::ggsave(plot = plots[[2]], "./output_ret_soc_v2/fig03_moment1_part2.pdf",
                width = 18, height = 20)
ggplot2::ggsave(plot = moment2, "./output_ret_soc_v2/fig03_moment2.pdf",
                width = 18, height = 20)
ggplot2::ggsave(plot = plots[[3]], "./output_ret_soc_v2/fig03_moment2_part1.pdf",
                width = 18, height = 20)
ggplot2::ggsave(plot = plots[[4]], "./output_ret_soc_v2/fig03_moment2_part2.pdf",
                width = 18, height = 20)



# Figure 5: fit  ===============================


EP_2018[, 'net_fin_wealth' := get('PATRI_NET')]
EP_2015[, 'net_fin_wealth' := get('PATRI_NET')]


df <- data.table::data.table(
  "Actifs financiers (enquete patrimoine, individualisée)" =
    get_quantiles(EP_2018, 'PATRI_BRUT', "POND_TRANS"),
  "Actifs - passifs financiers (enquete patrimoine, individualisée)" =
    get_quantiles(EP_2018, 'net_fin_wealth', "POND_TRANS"),
  "Modèle simulé (r = 3%)" = as.numeric(
    simulations[, round(quantile(get('wealth'), probs = c(1:9, 9.5, 9.9)/10))]
  ),
  "q" = 100*c(1:9, 9.5, 9.9)/10
)
df <- data.table::melt(df, id.vars = "q")



# library(scales)
# tn <- trans_new("abslog",
#                 function(x) sign(x)*log(abs(x)),
#                 function(y) exp(abs(y))*sign(y),
#                 domain=c(-Inf, Inf))

p_fit <- ggplot(df) +
  # geom_line(aes(x = q, y = value, color = variable,
  #               size = grepl("simulé", variable))) +
  geom_line(aes(x = q, y = asinh(value), color = variable,
                size = grepl("simulé", variable))) +
  geom_point(aes(x = q, y = asinh(value), color = variable)) +
  # geom_point(aes(x = q, y = value, color = variable)) +
  scale_size_manual(values = c(0.1, 2), 
                    labels = c("Observé (Patrimoine 2018)",
                               "Simulé")) +
  geom_hline(yintercept = 0) +
  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin()) +
  guides(col = guide_legend(nrow = 3)) +
  labs(x = "Quantile de la distribution du patrimoine financier",
       y = "Patrimoine",
       size = NULL,
       color = NULL) +
  theme(text = element_text(size=20),
        axis.text=element_text(size=28))


p_fit_level <- ggplot(df) +
  # geom_line(aes(x = q, y = value, color = variable,
  #               size = grepl("simulé", variable))) +
  geom_line(aes(x = q, y = value, color = variable,
                size = grepl("simulé", variable))) +
  geom_point(aes(x = q, y = value, color = variable)) +
  # geom_point(aes(x = q, y = value, color = variable)) +
  scale_size_manual(values = c(0.1, 2), 
                    labels = c("Observé (Patrimoine 2018)",
                               "Simulé")) +
  geom_hline(yintercept = 0) +
  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin()) +
  guides(col = guide_legend(nrow = 3)) +
  labs(x = "Quantile de la distribution du patrimoine financier",
       y = "Patrimoine",
       size = NULL,
       color = NULL) +
  theme(text = element_text(size=20),
        axis.text=element_text(size=28))
# scale_y_continuous(trans = tn)


ggplot2::ggsave(plot = p_fit, "./output_ret_soc/fig05_fit.png",
                width = 12, height = 8)
ggplot2::ggsave(plot = p_fit_level, "./output_ret_soc/fig05_fit_level.png",
                width = 12, height = 8)
data.table::fwrite(p_fit$data, file = "./output_ret_soc/fig05_data.csv")



# Table 1: data used for external validation ---------
# Pas besoin de R


# Table 2: table evolution richesse entre 2015 et 2018 --------

simulations[, c('retired') := data.table::fifelse(get('age') <= get("ageliq"),
                                                  'active',
                                                  'retired')]

EP_2015[, c('retired') := data.table::fifelse(get('SITUA')=="5",
                                             'retired',
                                             'active')]
EP_lon[, c('retired') := data.table::fifelse(get('SITUA_2015')=="5",
                                             'retired',
                                             'active')]
EP_lon[,'evol' := 100*(log(w_real_2018) - log(w_real_2015))]
EP_lon[, c("inc_2015") := get("labor_income_2015") + exp(rnorm(nrow(EP_lon)))] #jitterize data
EP_lon2 <- EP_lon[is.finite(evol) & is.finite(POND)]

EP_lon2[, c('decile_w') := cut(get("inc_2015"),
                               quantile(get("inc_2015"),
                                        probs= 0:10/10),
                               labels = 1:10, include.lowest = TRUE
)]


note_eplon <- c("\\textit{Enquête Patrimoine 2014-2015 and 2017-2018}, statistics based on household head or spouse information. Variables are transformed into 2009 constant euros. Summary statistics are computed using 2015 survey weights.",
                "This table shows the distribution of individual wealth (household wealth divided by the number of spouses) growth between 2015 and 2018 on the whole sample of panelized individual.",
                "Labor income represents the sum of labor earnings, retirement pensions and unemployment benefits.")


tablelight:::stack_summary(object = list(EP_lon2, EP_lon2, EP_lon2,
                                         EP_lon2, EP_lon2[!is.na(tr_diplome)]), 
                           x_vars = "evol", weight_vars = "POND",
                           multirow_labels = c("Whole population","By sex:","By status:",
                                               "By labor income decile:",
                                               "By diploma level"),
                           type = "dataframe",
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
    inheritance_model, type = "latex", title = "Heritage: outcome model",
    label = "tab:heritage",
    dep.var.labels = "Amount inherited",
    column.labels = "\\textit{Model in log}",
    covariate.labels = c('Sexe', "Log income",
                         c(sprintf("Age between %s and %s",
                                   seq(25, 75, by = 5),
                                   seq(30, 80, by = 5)
                         )),
                         c(sprintf("Graduation age (%s or %s)",
                                   seq(14, 30, by = 2),
                                   seq(15, 31, by = 2)
                         )                         
                         )
    ),
    add.lines = "Model estimated by interval regression (ordered probit regression with known thresholds) using declared received bequests in \\textit{Enquête Patrimoine 2009}"
  ),
  sep = "\n"
)


# Table 4: simulated and observed distribution inheritance --------

inheritance_distrib <- menages_structural2[, .SD[1], by = "Id"]

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


simulations_2015 <- simulations[annee==2015 & age>findet]

simulations_2015[, c('decile_w') := cut(get("revenu"),
                                        quantile(get("revenu"),
                                                 probs= 0:10/10, na.rm = TRUE),
                                        labels = 1:10, include.lowest = TRUE
)]

tablelight:::stack_summary(object = list(simulations_2015,
                                         simulations_2015,
                                         simulations_2015,
                                         simulations_2015,
                                         simulations_2015), x_vars = "wealth",
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




# PATRIMOINE ===============

EP_2015[,'rKY_100' := pmax(0, 100*r*get("w_real")/(get("labor_income") + r*get("w_real")))]
simulations[,"rKY_100" := pmax(0, get('rKY')*100)]
simulations_2015 <- simulations[annee==2015 & age>findet]

simulations_2015[, c('decile_w') := cut(get("revenu"),
                                        quantile(get("revenu"),
                                                 probs= 0:10/10, na.rm = TRUE),
                                        labels = 1:10, include.lowest = TRUE
)]

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



tablelight:::stack_summary(object = list(simulations_2015,
                                         simulations_2015,
                                         simulations_2015,
                                         simulations_2015,
                                         simulations_2015),
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

