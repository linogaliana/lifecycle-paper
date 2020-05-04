label_moments <- function(N_moments = 26L,
                          scale = "log",
                          ages = c(20,80),
                          quantiles = c(0.1,0.5,0.9),
                          survey_years = c(2015,2018),
                          stats = c("mean","sd"),
                          select_moments = NULL){
  
  scale1 <-  ifelse(N_moments<=2, yes = "log",
                    no = scale)
  
  moment1 <- data.frame(
    label = "Pic de la richesse médiane",
    Nmoment = 1L,
    scale = scale1,
    year = survey_years[1],
    champs = "Toute population"
  )
  
  
  
  scale2 <-  ifelse(N_moments<=2, yes = "level",
                    no = scale)
  
  
  moment2 <- data.frame(
    label = "Médiane de la variation de la richesse individuelle",
    Nmoment = 2L,
    scale = scale2,
    year = paste0(survey_years[1],"-",survey_years[2]),
    champs = paste0(min(ages), "-", max(ages))
  )
  
  
  moments <- data.table::rbindlist(list(moment1,moment2))
  
  
  moment1bis <- data.frame(
    label = "Pic de la richesse médiane",
    Nmoment = 3L,
    scale = scale,
    year = survey_years[2],
    champs = "Toute population"
  )
  
  
  moments <- data.table::rbindlist(list(moments,moment1bis))
  
  
  moments_wave1 <- data.frame(
    label = paste0("Richesse en coupe: ", stats),
    Nmoment = 3L + 1:length(stats),
    scale = scale,
    year = survey_years[1],
    champs = "Toute population"
  )
  
  moments <- data.table::rbindlist(list(moments,
                                        moments_wave1))
  
  moments_wave2 <- data.frame(
    label = paste0("Richesse en coupe: ", stats),
    Nmoment = max(moments$Nmoment) + 1:length(stats),
    scale = scale,
    year = survey_years[2],
    champs = "Toute population"
  )
  
  
  moments <- data.table::rbindlist(list(moments,
                                        moments_wave2))
  
  
  
  moments_age <- data.frame(
    label =  paste0("Richesse médiane tranche d'age (",
                    seq(min(ages), max(ages), by = 5),
                    "-",
                    seq(min(ages), max(ages), by = 5)+5,
                    "ans)"),
    Nmoment = max(moments$Nmoment) + 1:length(seq(min(ages), max(ages), by = 5)),
    scale = scale,
    year = survey_years[1],
    champs = paste0(seq(min(ages), max(ages), by = 5),
                    "-",
                    seq(min(ages), max(ages), by = 5)+5,
                    "ans")
  )
  
  moments <- data.table::rbindlist(list(moments,
                                        moments_age))
  
  
  moments_wave1_quantile <- data.frame(
    label = paste0("Richesse en coupe: quantile ", quantiles),
    Nmoment = max(moments$Nmoment) + 1:length(quantiles),
    scale = scale,
    year = survey_years[1],
    champs = "Toute population"
  )
  
  moments <- data.table::rbindlist(list(moments,
                                        moments_wave1_quantile))
  
  
  
  
  moments_wave2_quantile <- data.frame(
    label = paste0("Richesse en coupe: quantile ", quantiles),
    Nmoment = max(moments$Nmoment) + 1:length(quantiles),
    scale = scale,
    year = survey_years[2],
    champs = "Toute population"
  )
  
  moments <- data.table::rbindlist(list(moments,
                                        moments_wave2_quantile))
  
  
  
  
  moments_age_pic <- data.frame(
    label = "Age de l'acme de richesse (tranche de 5 ans)",
    Nmoment = max(moments$Nmoment) + 1:2,
    scale = scale,
    year = survey_years,
    champs = sprintf("Between %s and %s", min(ages), max(ages))
  )
  
  
  moments <- data.table::rbindlist(list(moments,
                                        moments_age_pic))
  
  
  moments <- moments[get('Nmoment') <= N_moments]
  if (!is.null(select_moments)) {
    moments <- moments[get("Nmoment") %in% select_moments]
  }
  
  return(moments)
}


