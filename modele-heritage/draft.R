library(tablelight)

unzip("~/Destinie.zip", exdir="~")
unzip("~/Enquete Patrimoine.zip", exdir="~")


EP_data <- wealthyR:::prepare_inheritance_sample(
  path_survey =  "~/Enquete Patrimoine"
)



inheritance_data <- prepare_estimation(EP_data)


bounds <- c(3,8,15,30,60,100,150,200,250)*1000
lbounds <- log(bounds)

estim_data <- inheritance_data[get('revenu')>0]

estim_data[,'MTHER' := as.numeric(as.character(MTHER))]
estim_data <- estim_data[order(MTHER)]


inheritance_model <- REtage::ordered_model_threshold(
  data = data.frame(estim_data[order(MTHER)]),
  formula = "MTHER ~ lw + tr_agfinetu",
  link = "probit",
  constantSD = TRUE,
  thresholds = lbounds
)



tablelight::view_html(
  tablelight:::light_table(
    inheritance_model,
    type = "html"
  )
)
