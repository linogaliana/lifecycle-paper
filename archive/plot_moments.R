simulations <- capitulation::life_cycle_model(data = data_microsimulated, 
                                              r = r,
                                              gamma = gamma,
                                              beta = beta,
                                              r_low = r_low,
                                              r_high = r_high, 
                                              income_var = "y_indiv",
                                              weight_var = NULL,
                                              wealthvar_survey = "K_observed", 
                                              Hgiven_var = "hg",
                                              Hreceived_var = "hr")


# MOMENT 1/ PIC ----------------------

plot_moment_pic(EP_2015 = EP_2015,
                EP_2018 = EP_2018,
                simulations = simulations,
                scale = "log")




# MOMENT 2 -------
# VARIATION K


plot_moment_dK(
  EP_lon = EP_lon, simulations = simulations,
  scale = "level"
)

# MOMENTS AGES MULTIPLES ----------------


plot_moment_age_multiple(
  EP_2015 = EP_2015,
  EP_2018 = EP_2018,
  simulations = simulations,
  scale = "level"
)


