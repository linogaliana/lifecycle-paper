library(tablelight)

unzip("~/Destinie.zip", exdir="~")
unzip("~/Enquete Patrimoine.zip", exdir="~")


EP_data <- wealthyR:::prepare_inheritance_sample(
  path_survey =  "~/Enquete Patrimoine"
)



inheritance_data <- REtage::prepare_estimation(EP_data)


bounds <- c(3,8,15,30,60,100,150,200,250)*1000
lbounds <- log(bounds)

estim_data <- inheritance_data[get('revenu')>0]

estim_data[,'MTHER' := as.numeric(as.character(MTHER))]
estim_data <- estim_data[order(MTHER)]


inheritance_model_1 <- REtage::ordered_model_threshold(
  data = data.frame(estim_data[order(MTHER)]),
  formula = "MTHER ~ lw",
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
