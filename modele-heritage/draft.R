library(tablelight)

source("functions.R")


EP_data <- wealthyR:::prepare_inheritance_sample(
  path_survey =  "../Enquete Patrimoine"
)



inheritance_data <- REtage::prepare_estimation(EP_data)


bounds <- c(3,8,15,30,60,100,150,200,250)*1000
lbounds <- log(bounds)

estim_data <- inheritance_data[get('revenu')>0]



estim_data <-data.table::copy(inheritance_data)#[get('income')>0]

estim_data[,'MTHER' := as.numeric(as.character(MTHER))]
estim_data[is.na(get("MTHER")), c("MTHER") := min(get("MTHER"))]
estim_data <- estim_data[order(MTHER)]
estim_data[,'age' := get('AGE')]
# estim_data[,'findet' := get('AGFINETU')]

estim_data <- estim_data[get('age') < 80]

estim_data$inherited <- factor(as.numeric(estim_data$inherited))

estim_data[, c('N_heritiers') := .N, by = c("annee","IDENT","IDENTTRANS")]

data.table::fwrite(estim_data, "./modele-heritage/estimsample.csv")


# MODEL WITH SELECTION ------------

inheritance_model <- sampleSelection::selection(
  selection = as.formula('inherited ~ factor(tr_age) + factor(tr_agfinetu)'),
  outcome = as.formula("MTHER ~ as.factor(SEXE) + lw"),
  boundaries = c(-Inf, lbounds, Inf),
  data = estim_data
)

inheritance_model <- oglm::oglmx(
  formulaMEAN = as.formula("MTHER ~ as.factor(SEXE) + lw"),
  selection = as.formula('inherited ~ factor(tr_age) + factor(tr_agfinetu)'),
  threshparam  = c(-Inf, lbounds, Inf),
  data = estim_data[1:1000]
)



# MODEL WITHTOUT SELECTION --------

estim_data <- estim_data[get('income')>0]
estim_data <- estim_data[!is.na(MTHER )]
inheritance_model_1 <- REtage::ordered_model_threshold(
  data = data.frame(estim_data[order(MTHER)]),
  formula = "MTHER ~ lw",
  link = "logit",
  constantSD = TRUE,
  thresholds = lbounds
)


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
