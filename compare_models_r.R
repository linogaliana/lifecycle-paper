
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
    column.labels = c("(r=1%)",
                         "(r=2%)",
                         "(r=3%)",
                         "(r=4%)",
                         "(r=5%)")
  )
)
