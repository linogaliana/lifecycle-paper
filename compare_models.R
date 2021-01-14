output_r2 <- readRDS("./output_r02.rds")
output_r3 <- readRDS("./output.rds")
output_r4 <- readRDS("./output_r4.rds")

tablelight::view_html(
  tablelight::light_table(
    list(output_r2, output_r3, output_r4),
    type = "html",
    column.labels = c("r=2%", "r=3%", "r=4%")
  )
)

print_delta <- function(model, r){
  beta <- as.numeric(model$estimates$theta_hat['beta'])
  delta <- 1/beta - 1
  return(c("delta" = delta, "r - delta" = r - delta))
}
print_delta(output_r2, .02)
print_delta(output_r3, .03)
print_delta(output_r4, .04)

