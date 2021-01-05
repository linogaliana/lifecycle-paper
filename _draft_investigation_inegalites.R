threshold = 0.9

compute_top <- function(indiv, xvar){
  #indiv2 <- indiv[get(age_var)>=get(findet_var)]
  indiv2 <- data.table::copy(indiv)
  indiv2[,c("top_labor") := NULL]
  indiv2[,c("top_labor") := lapply(.SD, function(x) x > quantile(x, na.rm = TRUE, probs = threshold)),
         .SDcols = xvar, by = "annee"]
  indiv2[,'top_labor' := as.character(data.table::fifelse(get('top_labor'),
                                                          sprintf('top%s labor income', 100*(1 - threshold)),
                                                          'others'))]
  
  tempdf <- indiv2[, .('share' = sum(get(xvar), na.rm = TRUE)), by = c("top_labor", "annee")]
  tempdf[,c('share') := get('share')/sum(get('share')), by = "annee"]
  tempdf <- tempdf[get("top_labor") != "others"]
  return(tempdf[annee==2015])
}


df1 <- wealthyR::individualize_EP(path_data = path_data, year = 2015)
df1_indiv <- wealthyR::individualize_EP(path_data = path_data, year = 2015,
                                        individualize_income = TRUE)

df1[, 'incomevar' := ZSALAIRES_I + ZRETRAITES_I + ZCHOMAGE_I]
df1_indiv[, 'incomevar' := ZSALAIRES + ZRETRAITES + ZCHOMAGE]
df1[, 'annee' := 2015]
df1[, 'annee' := 2015]

compute_top(df1, 'incomevar')
compute_top(df1_indiv, 'incomevar')


df1_indiv[,'yhousehold' := ZSALAIRES + ZRETRAITES + ZCHOMAGE]
df1_indiv[,'yindiv' := ZSALAIRES_I + ZRETRAITES_I + ZCHOMAGE_I]
df1_indiv[,'yconjoint' := yhousehold - yindiv]


champs_destinie <- indiv[annee == 2015 & age > findet & nspouses>1]
champs_ep <- df1_indiv[AGE > AGFINETU & N_adulte > 1]

cor(champs_ep$yconjoint, champs_ep$yindiv)
cor(champs_destinie$salaire_indiv, champs_destinie$salaire_conjoint)


library(ggplot2)
ggplot(champs_destinie[sample(0.1*.N)]) +
  geom_point(aes(x = log(1+salaire_indiv), y = log(1+salaire_conjoint))) +
  geom_abline(intercept = 0, slope = 1, color = "red")
  

ggplot(champs_ep[sample(0.1*.N)]) +
  geom_point(aes(x = log(1+yindiv), y = log(1+yconjoint))) +
  geom_abline(intercept = 0, slope = 1, color = "red")


compute_top(df1_indiv, 'yindiv')
compute_top(df1_indiv, 'yhousehold')

compute_top(indiv[annee == 2015], 'salaire_indiv')
compute_top(indiv[annee == 2015], 'salaire')
compute_top(indiv[annee == 2015 & age > findet], 'salaire_indiv')
compute_top(indiv[annee == 2015 & age > findet], 'salaire')


compute_top(household_table[annee == 2015], 'y_real')
compute_top(household_table[annee == 2015], 'salaire')



indiv[,'y_individualized' := get('salaire_tot')/get("nspouses")]

# DESTINIE ---------

# INITIAL INCOME
compute_top(indiv[annee == 2015], 'salaire_indiv')
compute_top(indiv[annee == 2015], 'y_individualized')


compute_top(indiv[annee == 2015 & age > findet], 'salaire_indiv')
compute_top(indiv[annee == 2015 & age > findet], 'y_individualized')

cor(indiv$salaire_indiv, indiv$salaire_conjoint)
cor(df1_indiv$yindiv, df1_indiv$yconjoint)

# ENQUETE PATRIMOINE ------

