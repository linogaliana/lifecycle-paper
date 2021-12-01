
model_r3 <- readRDS("./output.rds")
model_r1 <- readRDS("./output_r01.rds")
model_r2 <- readRDS("./output_r02.rds")
model_r4 <- readRDS("./output_r4.rds")
model_r5 <- readRDS("./output_r05.rds")


tablelight::view_html(
  tablelight::light_table(
    list(model_r1, model_r2,
         model_r3,
         model_r4, model_r5),
    type = "html",
    column.labels = c("(r=1\\%)",
                      "(r=2\\%)",
                      "(r=3\\%)",
                      "(r=4\\%)",
                      "(r=5\\%)"),
    title = "Estimated parameters for different values of r",
    label = "tab: robustness r GMM",
    dep.var.labels = "Exogeneous interest rate",
    covariate.labels = c("$\\beta$", "$\\gamma$"),
    add.lines = paste(c("Model estimated by minimum distance using aggregate moments",
                        "from microsimulated data and wealth surveys.",
                        "Moments in wealth survey are defined in Table",
                        "\\ref{tab:data}. The same moments are computed in
                    microsimulated data."), collapse = " ")
  )
)


cat(
  tablelight::light_table(
    list(model_r1, model_r2,
         model_r3,
         model_r4, model_r5),
    column.labels = c("(r=1\\%)",
                      "(r=2\\%)",
                      "(r=3\\%)",
                      "(r=4\\%)",
                      "(r=5\\%)"),
    title = "Estimated parameters for different values of $r$",
    label = "tab: robustness r GMM",
    dep.var.labels = "Exogeneous interest rate",
    covariate.labels = c("$\\beta$", "$\\gamma$"),
    stats.var.separate = 5,
    add.lines = paste(c("Model estimated by minimum distance using aggregate moments",
                        "from microsimulated data and wealth surveys.",
                        "Moments in wealth survey are defined in Table",
                        "\\ref{tab:data}. The same moments are computed in",
                        "microsimulated data."), collapse = " ")
  ),
  sep = "\n"
)

cat(
  tablelight::light_table(
    model_r3,
    dep.var.labels = c("\\textsc{Estimates}"),
    title = "Estimation results",
    label = "tab: estimation table",
    covariate.labels = c("$\\beta$", "$\\gamma$"),
    column.labels = NULL,
    add.lines = paste(c("Model estimated by minimum distance using aggregate moments",
                        "from microsimulated data and wealth surveys.",
                        "Moments in wealth survey are defined in Table",
                        "\\ref{tab:data}. The same moments are computed in",
                        "microsimulated data."), collapse = " ")
  ),
  sep = "\n"
)
