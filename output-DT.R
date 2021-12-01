library(tablelight)
library(data.table)
library(ggplot2) 

source("functions.R", encoding = "UTF-8")


# MODELE HERITAGE ------------------------------------

EP_data <- wealthyR:::prepare_inheritance_sample(
  path_survey =  "../Enquete Patrimoine"
)
EP_data <- EP_data[(MER1E == 3 & PER1E==3) | (!is.na(MTHER))]


inheritance_data <- REtage::prepare_estimation(EP_data, taille_tr_age = 5,
                                               taille_tr_agfinetu = 2)


bounds <- c(3,8,15,30,60,100,150,200,250)*1000
lbounds <- log(bounds)


estim_data <- data.table::copy(inheritance_data)#[get('income')>0]

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



plot_confusion <- function(langage = c('fr','eng')){
  
  langage <- match.arg(langage)
  labs_x <- c(ifelse(langage == "eng", "No inheritance","Pas d'héritage"),
              REtage:::get_labs(lbounds)
  )
  if (langage == "fr"){
    labs_x <- gsub("Less than", "Moins de",labs_x)
    labs_x <- gsub("More than", "Plus de",labs_x)
  }
  ylab <- ifelse(langage == "eng", 'Number of individuals', "Nombre d'individus")
  xlab <- ifelse(langage == "eng", "Inherited amount in euros",
                 "Valeur héritée (en euros)")
  labels_legend <- if(langage == "eng") c("Observed","Predicted") else c("Observée", "Simulée")
  
  p <- ggplot(confusion) +
    geom_histogram(aes(x = factor(value),fill = variable),
                   stat = "count", alpha=0.6, position = 'dodge') +
    scale_x_discrete(labels= labs_x) +
    theme(legend.title=element_blank(), legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust=1)) +
    labs(x = NULL, y = ylab)
  
  p <- p +
    ggplot2::labs(y = ylab,
                  x = xlab) +
    ggplot2::scale_fill_manual(breaks = c("Observed","Predicted"),
                               values=c("#69b3a2", "#404080"),
                               labels = labels_legend) +
    labs(fill = NULL) +
    theme(text = element_text(size=20),
          axis.text=element_text(size=20),
          axis.title = element_text(size = 28, face="bold"))
  
  return(p)
}

p_french <- plot_confusion('fr')
p_english <- plot_confusion('en')


ggplot2::ggsave(plot = p_french, filename = "./output_ret_soc_v2/fig01_inheritance_predicted.pdf",
                width = 13, height = 9)
ggplot2::ggsave(plot = p_french, filename = "./output_ret_soc_v2/fig01_inheritance_predicted.png",
                width = 13, height = 9)
ggplot2::ggsave(plot = p_english, filename = "./output_ret_soc_v2/fig01_inheritance_predicted_DT.pdf",
                width = 13, height = 9)
data.table::fwrite(p_french$data[, .N, by = c('variable','value')], file = "./output_ret_soc_v2/fig01_data.csv")



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
simulations[,'sexe' := get("SEXE")]

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


# K(AGE) ET rK(AGE) -----------------------------

kage_fr <- plot_K_age2(simulations,EP_2015, method = "median", has_ricardian = FALSE,
                       trans = "div_1000", trans_survey = "div_1000",
                       lang = "fr")

kage_eng <- plot_K_age2(simulations,EP_2015, method = "median", has_ricardian = FALSE,
                        trans = "div_1000", trans_survey = "div_1000")

ggplot2::ggsave(plot = kage_fr, filename = "./output_ret_soc_v2/fig01_kage2.pdf",
                width = 13, height = 9)
ggplot2::ggsave(plot = kage_fr, filename = "./output_ret_soc_v2/fig01_kage2.png",
                width = 13, height = 9)
data.table::fwrite(kage_fr$data[,.SD,.SDcols = c('annee','age','predict','source')],
                   "./output_ret_soc_v2/fig01_data.csv")

ggplot2::ggsave(plot = kage_eng, filename = "./output_ret_soc_v2/fig01_kage2_DT.pdf",
                width = 13, height = 9)

EP_2015[,'rKY' := r*get("PATRI_NET")/(get("labor_income") + r*get("PATRI_NET"))]

rkage <- plot_K_age2(simulations,EP_2015, method = "median", has_ricardian = FALSE,
                     wealth_var = "rKY", wealth_var_survey = "rKY",
                     lang = "eng") +
  ggplot2::scale_y_continuous(labels = scales::percent)
rkage_fr <- plot_K_age2(simulations,EP_2015, method = "median", has_ricardian = FALSE,
                        wealth_var = "rKY", wealth_var_survey = "rKY",
                        lang = "fr") +
  ggplot2::scale_y_continuous(labels = scales::percent)


ggplot2::ggsave(plot = rkage, filename = "./output_ret_soc_v2/fig01b_rkage_DT.pdf",
                width = 13, height = 9)


ggplot2::ggsave(plot = rkage_fr, filename = "./output_ret_soc_v2/fig01b_rkage.pdf",
                width = 13, height = 9)
ggplot2::ggsave(plot = rkage_fr, filename = "./output_ret_soc_v2/fig01b_rkage.png",
                width = 13, height = 9)
data.table::fwrite(rkage_fr$data[,.SD,.SDcols = c('annee','age','predict','source')],
                   "./output_ret_soc_v2/fig01b_data.csv")


# PLOT DES MOMENTS -----------------------------------


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


plot_moment_age_wide <-function(df_moment2, ages = c(30,65),lang = c("fr","eng")){
  
  lang <- match.arg(lang)
  
  tempdat <- data.table::copy(df_moment2)
  tempdat[,'Nmoment' := data.table::fifelse(
    cumsum(weight) <= 1, paste0(as.character(20+(Nmoment-1)*5),"-",
                                as.character(20+Nmoment*5)),
    paste0(as.character(30+(Nmoment-14)*5),"-",
           as.character(30+(Nmoment-13)*5))
  )]
  tempdat[, 'cumsum' := cumsum(weight)]
  tempdat[, 'moment' := 1 + as.numeric(cumsum>1)]
  tempdat[, c("cumsum") := NULL]
  
  tempdat <- split(tempdat, by = "moment")
  tempdat <- lapply(tempdat, data.table::melt, id.vars = c("Nmoment", "moment"))
  
  if (lang == "fr"){
    labs1 <- list(x = "Age",
                  y = "Arcsinh(patrimoine médian)",
                  labels = c("Simulée","Observée"),
                  col = "Poids dans l'estimation (densité)",
                  y2 = 'Densité')
  } else{
    labs1 <- list(x = "Age",
                  y = "arcsinh(Median wealth)",
                  labels = c("Simulated","Observed"),
                  col = "Weight in estimation (density)",
                  y2 = "Density")  
  }
  
  m1 <- ggplot2::ggplot(tempdat[["1"]][variable != "weight"]) +
    ggplot2::geom_bar(ggplot2::aes(x = Nmoment, y = value,
                                   fill = variable), position = "dodge",
                      stat='identity', width=.5) +
    geom_line(data = tempdat[["1"]][variable == "weight"],
              aes(x = Nmoment, y = 100*value, color = labs1$col, group = 1)) +
    geom_point(data = tempdat[["1"]][variable == "weight"],
               aes(x = Nmoment, y = 100*value, color = labs1$col)) +
    ggplot2::labs(x = labs1$x, y = labs1$y) +
    ggplot2::scale_fill_manual(values = c('moment_simulations' = "#69b3a2",
                                          'moment_data' = "#404080"),
                               labels = labs1$labels) +
    ggplot2::scale_color_manual('', values = "red", breaks = labs1$col) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse=TRUE)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(y = labs1$y,
                  fill = NULL) +
    ggplot2::theme(text = ggplot2::element_text(size=28),
                   axis.text=ggplot2::element_text(size=22),
                   axis.text.x = ggplot2::element_text(angle = 45, hjust=1),
                   axis.title = element_text(face = "bold"))
  
  m1 
  
  if (lang == "fr"){
    labs2 <- list(x = "Age",
                  y = "Evolution sur trois ans\ndu patrimoine (2015-2018)",
                  labels = c("Simulée","Observée"),
                  col = "Poids dans l'estimation",
                  y2 = 'Densité')  
  } else{
    labs2 <- list(x = "Age",
                  y =  "Median Wealth growth rate \nbetween 2015 and 2018",
                  labels = c("Simulated","Observed"),
                  col = "Weight in estimation",
                  y2 = "Density")  
  }
  
  m2 <- ggplot2::ggplot(tempdat[["2"]][variable != "weight"], ggplot2::aes(x = Nmoment,
                                                                           y = value,
                                                                           color = variable,
                                                                           shape = variable,
                                                                           group = variable)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    geom_line(data = tempdat[["2"]][variable == "weight"],
              aes(x = Nmoment, y = value, color = "weight", group = 1), linetype = "dashed") +
    geom_point(data = tempdat[["2"]][variable == "weight"],
               aes(x = Nmoment, y = value, color = "weight"), alpha = 0.4, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0L) +
    ggplot2::labs(x = labs2$x,
                  y = labs2$y) +
    ggplot2::scale_color_manual(values = c('moment_simulations' = 'black',
                                           'moment_data' = 'royalblue',
                                           "weight" = "red"),
                                labels = c(labs2$labels, labs2$col)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(y = labs2$y,
                  x = labs2$x, title = "") +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::theme(legend.title=ggplot2::element_blank()) +
    ggplot2::theme(text = ggplot2::element_text(size=28),
                   axis.text=ggplot2::element_text(size=22),
                   axis.text.x = ggplot2::element_text(angle = 45, hjust=1),
                   axis.title = element_text(face = "bold")) +
    guides(shape="none")
  
  m2b <- ggplot2::ggplot(tempdat[["2"]][variable == "weight"]) +
    ggplot2::geom_bar(ggplot2::aes(x = Nmoment - 13, y = value), stat = 'identity') +
    ggplot2::geom_point(ggplot2::aes(x = Nmoment - 13, y = value), color = 'red') +
    ggplot2::labs(y = labs2$y2, x = labs2$x) +
    ggplot2::scale_y_reverse()
  
  return(
    list(m1, m2)
  )
  
}

plots <- plot_moment_age_wide(df_moment2)
moment1 <- plots[[1]]
moment2 <- plots[[2]]
plots_eng <- plot_moment_age_wide(df_moment2, lang = "eng")
moment1_eng <- plots_eng[[1]]
moment2_eng <- plots_eng[[2]]

ggplot2::ggsave(plot = moment1, "./output_ret_soc_v2/fig03_moment1.pdf",
                width = 18, height = 20)
ggplot2::ggsave(plot = moment2, "./output_ret_soc_v2/fig03_moment2.pdf",
                width = 18, height = 20)
ggplot2::ggsave(plot = moment1, "./output_ret_soc_v2/fig03_moment1.png",
                width = 18, height = 20)
ggplot2::ggsave(plot = moment2, "./output_ret_soc_v2/fig03_moment2.png",
                width = 18, height = 20)
ggplot2::ggsave(plot = moment1_eng, "./output_ret_soc_v2/fig03_moment1_DT.pdf",
                width = 18, height = 20)
ggplot2::ggsave(plot = moment2_eng, "./output_ret_soc_v2/fig03_moment2_DT.pdf",
                width = 18, height = 20)
data.table::fwrite(plots[[1]]$data, "./output_ret_soc_v2/fig03_moment1.csv")
data.table::fwrite(plots[[2]]$data, "./output_ret_soc_v2/fig03_moment2.csv")


# Figure 5: fit  ===============================


EP_2018[, 'net_fin_wealth' := get('PATRI_NET')]
EP_2015[, 'net_fin_wealth' := get('PATRI_NET')]



plot_fit_distribution <- function(EP_2018, simulations, lang = c('fr','eng')){
  
  lang <- match.arg(lang)
  
  g2 <- ifelse(lang == "eng",
               "Simulated","Simulée")
  g1 <- ifelse(lang == "eng",
               "Observed", "Observée")
  list_p <- c(seq(1,9.5, by = 0.5), 9.9)/10
  df <- data.table::data.table(
    g1 =
      get_quantiles(EP_2018, 'PATRI_NET', "POND_TRANS", p = list_p),
    g2 = as.numeric(
      simulations[annee==2018, round(quantile(get('wealth'), probs = list_p))]
    ),
    "q" = 100*list_p
  )
  data.table::setnames(df, old = c('g1','g2'), new = c(g1,g2))
  df <- data.table::melt(df, id.vars = "q")
  
  
  if (lang == "fr"){
    labs1 <- list(x = "Quantile de la distribution du patrimoine net",
                  y = "Arcsinh(patrimoine)",
                  labels = c("Observé",
                             "Simulé"))
  } else{
    labs1 <- list(x = "Quantile of net wealth",
                  y = "arcinsh(Net wealth)",
                  labels = c("Observed",
                             "Simulated"))
  }
  
  p_fit <- ggplot(df) +
    # geom_line(aes(x = q, y = value, color = variable,
    #               size = grepl("simulé", variable))) +
    geom_line(aes(x = q, y = asinh(value), color = variable)) +
    geom_point(aes(x = q, y = asinh(value), color = variable)) +
    # geom_point(aes(x = q, y = value, color = variable)) +
    scale_size_manual(values = c(0.1, 2), 
                      labels = labs1$labels) +
    geom_hline(yintercept = 0) +
    theme_bw() +
    theme(legend.position="bottom", legend.box="vertical", legend.margin=margin()) +
    guides(col = guide_legend(nrow = 1)) +
    labs(x = labs1$x,
         y = labs1$y,
         size = NULL,
         color = NULL) +
    theme(text = element_text(size=20),
          axis.text=element_text(size=20),
          axis.title = element_text(size = 28, face = "bold") )
  
  
  p_fit_level <- ggplot(df) +
    # geom_line(aes(x = q, y = value, color = variable,
    #               size = grepl("simulé", variable))) +
    geom_line(aes(x = q, y = value, color = variable)) +
    geom_point(aes(x = q, y = value, color = variable)) +
    # geom_point(aes(x = q, y = value, color = variable)) +
    scale_size_manual(values = c(0.1, 2), 
                      labels = labs1$labels) +
    geom_hline(yintercept = 0) +
    theme_bw() +
    theme(legend.position="bottom", legend.box="vertical", legend.margin=margin()) +
    guides(col = guide_legend(nrow = 2)) +
    labs(x = labs1$x,
         y = labs1$y,
         size = NULL,
         color = NULL) +
    theme(text = element_text(size=20),
          axis.text=element_text(size=20),
          axis.title = element_text(size = 28, face = "bold") )
  # scale_y_continuous(trans = tn)
  
  return(list(p_fit, p_fit_level))
}

p_fit1 <- plot_fit_distribution(EP_2018, simulations)
p_fit2 <- plot_fit_distribution(EP_2018, simulations, "eng")

ggplot2::ggsave(plot = p_fit1[[1]], "./output_ret_soc_v2/fig05_fit.png",
                width = 12, height = 8)
ggplot2::ggsave(plot = p_fit1[[1]], "./output_ret_soc_v2/fig05_fit.pdf",
                width = 12, height = 8)
ggplot2::ggsave(plot = p_fit1[[2]], "./output_ret_soc_v2/fig05_fit_level.png",
                width = 12, height = 8)
data.table::fwrite(p_fit1[[1]]$data, file = "./output_ret_soc_v2/fig05_data.csv")
ggplot2::ggsave(plot = p_fit2[[1]], "./output_ret_soc_v2/fig05_fit_DT.pdf",
                width = 16, height = 8)
ggplot2::ggsave(plot = p_fit2[[2]], "./output_ret_soc_v2/fig05_fit_level_DT.pdf",
                width = 16, height = 8)


# top income shares --------------------------

simul_copy <- data.table::copy(simulations)

plot_top_shares2 <- function(simul_copy, threshold = 0.99, lang = c("eng","fr")){
  
  lang <- match.arg(lang)
  
  if (lang == "fr"){
    lab <- list(x = "Année", y = sprintf("Part du top %s%%", 100*(1 - threshold)),
                labels = c("Revenus du travail","Revenu total",'Patrimoine net')
    )
  } else{
    lab <- list(x = "Year", y = sprintf("Top %s%% share", 100*(1 - threshold)),
                labels = c("Labor income","Total income",'Wealth')
    )
    
  }
  
  cols <- setNames(c('darkgreen', 'royalblue3', 'hotpink4'),
                   c(sprintf("top%s labor income", 100*(1 - threshold)),
                     sprintf("top%s total income", 100*(1 - threshold)),
                     sprintf("top%s wealth", 100*(1 - threshold)))
  )
  
  
  p3 <- capitulation::plot_top_share(simul_copy, threshold = threshold, 
                                     negative_values = "truncate") +
    theme_bw() + 
    theme(legend.position = "bottom") +
    labs(x = lab$x, y = lab$y,
         color = NULL) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme(axis.text=element_text(size=24),
          axis.title=element_text(size=24,face="bold"),
          legend.text=element_text(size=24)) +
    scale_colour_manual(
      # palette = "Set1",
      values = cols,
      labels = lab$labels
    )
  
  return(p3)
  
}

p3 <- plot_top_shares2(simul_copy, threshold = 0.9, lang = "eng")
p3_fr <- plot_top_shares2(simul_copy, threshold = 0.9, lang = "fr")
ggsave(plot = p3, sprintf("./output_ret_soc_v2/top10_DT.pdf", dir), width = 12, height = 8)
ggsave(plot = p3_fr, sprintf("./output_ret_soc_v2/top10.png", dir), width = 12, height = 8)
ggsave(plot = p3_fr, sprintf("./output_ret_soc_v2/top10.pdf", dir), width = 12, height = 8)
data.table::fwrite(p3$data, "./output_ret_soc_v2/top10.csv")


simul_copy <- data.table::copy(simulations)
p3 <- plot_top_shares2(simul_copy, threshold = 0.99, lang = "eng")
p3_fr <- plot_top_shares2(simul_copy, threshold = 0.99, lang = "fr")

ggsave(plot = p3, sprintf("./output_ret_soc_v2/top1_DT.pdf", dir), width = 12, height = 8)
ggsave(plot = p3_fr, sprintf("./output_ret_soc_v2/top1.png", dir), width = 12, height = 8)
ggsave(plot = p3_fr, sprintf("./output_ret_soc_v2/top1.pdf", dir), width = 12, height = 8)
data.table::fwrite(p3$data, "./output_ret_soc_v2/top1.csv")

plot_gini2 <- function(simul_copy, lang = c("English","French")){
  
  lang <- match.arg(lang)
  
  if (lang == "French"){
    lab <- list(x = "Année", y = "Indice de Gini",
                labels = c("Revenus du travail","Revenu total", 'Patrimoine net')
    )
  } else{
    lab <- list(x = "Year", y = "Gini index",
                labels = c("Labor income","Total income", 'Wealth')
    )
    
  }
  
  cols <- setNames(c('darkgreen', 'royalblue3', 'hotpink4'),
                   c("revenu", "Y", "wealth")
  )
  
  p3 <- capitulation::plot_gini(simul_copy,
                                langage = "English",
                                labels = c("revenu", "Y", "wealth"),
                                vars = c("revenu", "Y", "wealth"),
                                negative_values = "truncate") +
    theme(axis.text=element_text(size=24),
          axis.title=element_text(size=24,face="bold"),
          legend.text=element_text(size=24)) +
    labs(y = lab$y, x = lab$x) +
    scale_colour_manual(
      # palette = "Set1",
      values = cols,
      labels = lab$labels
    )
  
  p3
  
  return(p3)
  
}

p2 <- plot_gini2(simul_copy)
p2_fr <- plot_gini2(simul_copy, "French")
ggsave(plot = p2, sprintf("./output_ret_soc_v2/gini_evolution_noneg_DT.pdf", dir), width = 12, height = 8)
ggsave(plot = p2_fr, sprintf("./output_ret_soc_v2/gini_evolution_noneg.png", dir), width = 12, height = 8)
ggsave(plot = p2_fr, sprintf("./output_ret_soc_v2/gini_evolution_noneg.pdf", dir), width = 12, height = 8)
data.table::fwrite(p2$data, "./output_ret_soc_v2/gini.csv")



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
    list(probit, inheritance_model), type = "latex",
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

inheritance_distrib <- menages_structural2[, .SD[1], by = "Id"]

tablelight:::summary_(
  inheritance_distrib[inheritance_distrib$H_given != 0], xvar = "hg",
  stats = c("1Q", 'median',"mean",'3Q','P90', 'N')
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


EP_2015[,'wealth_trunc' := pmax(0, get('PATRI_NET'))]
EP_2015[,'wealth_trunc' := get('PATRI_NET') + exp(rnorm(nrow(EP_2015)))]

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
  "\\textsc{Group} & \\textsc{Labor income} & \\textsc{Total income} & \\textsc{Net wealth} \\\\",
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


# GMM TABLE --------------------

out <- readRDS("./gamma-loop/gamma_1.rds")

cat(
  tablelight::light_table(
    out,
    add.lines = paste0(
      "Model estimated by minimum distance using aggregate moments ",
      "from microsimulated data and wealth surveys. ",
      "Moments in wealth survey are defined in Table \\ref{tab:data}.",
      " The same moments are computed in microsimulated data."
    ),
    dep.var.labels = "\\textsc{Estimates}",
    column.labels = "",
    title = "Estimation results",
    label = "tab: estimation table",
    covariate.labels = "$\\beta$"
  ),
  sep = "\n"
)


# ROBUSTNESS TABLE -----------------

out2 <- readRDS("./gamma-loop/gamma_08.rds")
out3 <- readRDS("./gamma-loop/gamma_12.rds")

cat(
  tablelight::light_table(
    list(out2, out3, out),
    add.lines = paste0(
      "Model estimated by minimum distance using aggregate moments ",
      "from microsimulated data and wealth surveys. ",
      "Moments in wealth survey are defined in Table \\ref{tab:data}.",
      " The same moments are computed in microsimulated data."
    ),
    dep.var.labels = "Exogeneous risk aversion coefficient $\\gamma$",
    column.labels = c("$\\gamma$=0.8","$\\gamma=1.2$", "$\\gamma$ = 1"),
    stats.var.separate = 3,
    title = "Estimated parameters for different values of $\\gamma$",
    label = "tab: robustness r GMM",
    covariate.labels = "$\\beta$"
  ),
  sep = "\n"
)
