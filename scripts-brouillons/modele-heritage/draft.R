library(tablelight)

source("functions.R")


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


data.table::fwrite(estim_data, "./modele-heritage/estimsample.csv")

estim_data[is.na(get("MTHER")), c("MTHER") := 0]
estim_data[, inherited := (MTHER != 0) ]
estim_data$inherited <- factor(as.numeric(estim_data$inherited))
estim_data <- estim_data[order(MTHER)]

estim_data <- na.omit(estim_data, cols = c("inherited","tr_age","tr_agfinetu","SEXE", "lw", "MTHER"))


# SELECTION MODEL -----------------


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


# MODEL WITHOUT SELECTION --------


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
  formulaMEAN = "MTHER ~ factor(SEXE) + lw + factor(tr_age) + factor(tr_agfinetu) + N_heritiers",
  constantSD = TRUE,
  threshparam = lbounds,
  start_method = "search"
)
class(inheritance_model) <- c("oglm","oglmx")
summary(inheritance_model)


# predict -------

prediction_2step <- REtage:::predict_two_steps(probit = probit,
                                      intreg = inheritance_model, estim_data, scale = "class", lbounds = lbounds)

prediction <- prediction_2step


confusion <- data.frame('Observed' = estim_data$MTHER, 'Predicted' = prediction_2step$prediction, "rn" = seq_len(nrow(estim_data)))
confusion <- data.table::melt(data.table::setDT(confusion), id.vars = "rn")
  
library(ggplot2) 

p <- ggplot(confusion) +
  geom_histogram(aes(x = factor(value),fill = variable), stat = "count", alpha=0.6, position = 'dodge') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  scale_x_discrete(labels= c("No inheritance", REtage:::get_labs(lbounds))) +
  theme(legend.title=element_blank(), legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust=1)) +
  labs(x = NULL, y = 'Number of individuals')
p
ggsave(filename = "V:/DG75-G210/Lino/microsimulation/estimation/modele-heritage/predict.png", plot = p,
       width = 12, height = 8)


# MODEL WITH SELECTION ------------

estim_data[, inherited2 := factor(as.numeric(inherited))]
estim_data[MTHER == 0, MTHER := NA]


inheritance_model <- sampleSelection::selection(
  selection = as.formula('inherited2 ~ factor(tr_age) + factor(tr_agfinetu)'),
  outcome = as.formula("factor(MTHER) ~ as.factor(SEXE) + lw"),
  boundaries = c(-Inf, lbounds, Inf),
  data = estim_data[order(inherited2)]
)

inheritance_model <- oglm::oglmx(
  formulaMEAN = as.formula("MTHER ~ as.factor(SEXE) + lw"),
  selection = as.formula('inherited ~ factor(tr_age) + factor(tr_agfinetu)'),
  threshparam  = c(-Inf, lbounds, Inf),
  data = estim_data
)



# OTHER MODELS --------------------

inheritance_model_1_div <- REtage::ordered_model_threshold(
  data = data.frame(estim_data[order(MTHER)]),
  formula = "MTHER ~ lw + N_heritiers",
  link = "logit",
  constantSD = TRUE,
  thresholds = lbounds
)


inheritance_model_1b <- REtage::ordered_model_threshold(
  data = data.frame(estim_data[order(MTHER)]),
  formula = "MTHER ~ lw",
  link = "probit",
  constantSD = TRUE,
  thresholds = lbounds
)


inheritance_model_2 <- REtage::ordered_model_threshold(
  data = data.frame(estim_data[order(MTHER)]),
  formula = "MTHER ~ lw + tr_age",
  link = "logit",
  constantSD = TRUE,
  thresholds = lbounds
)

inheritance_model_2b <- REtage::ordered_model_threshold(
  data = data.frame(estim_data[order(MTHER)]),
  formula = "MTHER ~ lw + tr_age",
  link = "probit",
  constantSD = TRUE,
  thresholds = lbounds
)


inheritance_model_3 <- REtage::ordered_model_threshold(
  data = data.frame(estim_data[order(MTHER)]),
  formula = "MTHER ~ lw + tr_age + tr_agfinetu",
  link = "logit",
  constantSD = TRUE,
  thresholds = lbounds
)
inheritance_model_4 <- REtage::ordered_model_threshold(
  data = data.frame(estim_data[order(MTHER)]),
  formula = "MTHER ~ lw + tr_age + tr_agfinetu + SEXE",
  link = "logit",
  constantSD = TRUE,
  thresholds = lbounds
)


tablelight::view_html(
  tablelight:::light_table(
    list(
      inheritance_model_1b,
      inheritance_model_2b,
      inheritance_model_1,
      inheritance_model_2,
      inheritance_model_2,
      inheritance_model_3,
      inheritance_model_4
    ),
    type = "html",
    stats.list = c("n","lln","bic","link","sigma"),
    dep.var.labels = c("probit","logit"),
    dep.var.separate = 2L,
    column.labels = c("(1)","(2)")
  )
)

tablelight::view_html(
  tablelight::light_table(
    inheritance_model_1,
    stats.list = c("n","lln","bic","link","sigma"),
    type = "html"
  )
)


stargazer::stargazer(
  inheritance_model_1,
  inheritance_model_2,
  inheritance_model_3,
  inheritance_model_4,
  type = "html"
)
