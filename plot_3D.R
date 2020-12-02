library(plotly)


results3 <- aws.s3::s3read_using(FUN = data.table::fread,
                                 object = "grid.csv",
                                 opts = list("region" = ""),
                                 bucket = "groupe-788")

results4 <- data.table::copy(results3)
results4[,'labels' := paste0('beta: ', get("beta"),
                             '<br>',
                             'gamma: ',  get("gamma"),
                             '<br>',
                             'loss: ',  get("loss")
)] 


fig <- plot_ly(
  data = results4,
  x = ~beta, 
  y = ~gamma, 
  z = ~loss,
  intensity=~loss,
  text=~labels,
  type = "mesh3d",
  colorscale = "Hot",
  hoverinfo = 'text'
)

fig



widget_file_size <- function(p) {
  d <- tempdir()
  withr::with_dir(d, htmlwidgets::saveWidget(p, "index.html"))
  f <- file.path(d, "index.html")
  mb <- round(file.info(f)$size / 1e6, 3)
  message("File is: ", mb," MB")
}

widget_file_size(fig)


## DATA PREPARATION ------------

source("functions.R")
source("import_functions.R")

# theta <- c(beta = '0.975', gamma = '0.6')
# 
# 
betas <- seq(from = 0.969, to = 0.985, by = 0.0005)
gammas <- seq(0.4, 6, by = 0.1)

grid <- data.frame(
  expand.grid(beta = betas, gamma = gammas)
)

grid <- split(grid, 1:nrow(grid))



system.time(
  plot_theta <- lapply(grid, function(theta){
    epseps <- report_epsilon(
      # theta = c(r = r,
      #           beta = beta,
      #           gamma = gamma),
      r = r,
      beta = theta$beta,
      gamma = theta$gamma,
      # model_function = wealthyR:::model_capitulation,
      population = population,
      EP_2015 = EP_2015,
      EP_2018 = EP_2018,
      EP_lon = EP_lon,
      verbose = FALSE)
    return(
      data.frame(beta = theta$beta,
                 gamma = theta$gamma,
                 loss = epseps)
    )
  })
)

results <- data.table::rbindlist(plot_theta)


results2 <- aws.s3::s3read_using(
  FUN = data.table::fread,
  bucket = "groupe-788",
  object = "grid.csv",
  region = ""
)
results2 <- data.table::rbindlist(list(results, results2), fill = TRUE, use.names = TRUE)
results3 <- unique(results2)


mini = out[ , .SD[which.min(loss)], by = beta]
ggplot2::ggplot(mini) + ggplot2::geom_line(ggplot2::aes(x = beta, y = gamma))


# aws.s3::s3write_using(results3, FUN = data.table::fwrite,
#                       bucket = "groupe-788",
#                       object = "grid.csv",
#                       opts = list("region" = ""))

