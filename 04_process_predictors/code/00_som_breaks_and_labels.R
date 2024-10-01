#### DEFINE BREAKS AND LABELS FOR SOMALIA CATEGORY

# Define breaks and labels for different groups for categorisation
breaks_and_labels <- list(
  acled_event_rate = list(
    breaks = c(0, 0.0000001, 1, 2, 1000000),
    labels = c("0.00", "0.01 to 0.99", "1.00 to 1.99", ">= 2.00")
  ),
  acled_fatalities_rate = list(
    breaks = c(0, 0.0000001, 1, 2, 1000000),
    labels = c("0.00", "0.01 to 0.99", "1.00 to 1.99", ">= 2.00")
  ),
  aidworkers_killed_rate = list(
    breaks = c(0, 0.0000001, 100000),
    labels = c("0", ">= 1")
  ),
  aidworkers_wounded_rate = list(
    breaks = c(0, 0.0000001, 100000),
    labels = c("0", ">= 1")
  ),
  cholera_cases_rate = list(
    breaks = c(0, 0.0000001, 100000),
    labels = c("0", ">= 1")
  ),
  dep_rate = list(
    breaks = c(0, 0.0000001, 0.01, 0.02, 10),
    labels = c("0.000", "0.001 to 0.009", "0.010 to 0.019", "> = 0.020")
  ),
  malaria_cases_rate = list(
    breaks = c(0, 0.0000001, 50, 10000),
    labels = c("0", "1 to 49", ">= 50")
  ),
  measles_cases_rate = list(
    breaks = c(0, 0.0000001, 100000),
    labels = c("0", ">= 1")
  ),
  prop_idp = list(
    breaks = c(0, 0.25, 0.50, 0.75, 1.00),
    labels = c("< 25%", "25 to 49%", "50 to 74%", ">= 75%")
  ),
  rainfall = list(
    breaks = c(0, 50, 100, 150, 200, Inf),
    labels = c(
      "< 50mm", "50 to 99mm", "100 to 149mm", "150 to 199mm",
      ">= 200mm"
    )
  ),
  cdi_lag5 = list(
    breaks = c(0, 0.4, 0.6, 0.8, 1.0, Inf),
    labels = c(
      "< 0.4", "0.4 - 0.59", "0.6 - 0.79", "0.8 - 0.99",
      ">= 1"
    )
  ),
  sam_admissions_rate = list(
    breaks = c(0, 100, 199, 100000),
    labels = c("< 100", "100 to 199", ">= 200")
  ),
  tot_goat_cereal_smooth = list(
    breaks = c(0, 200000, 300000, 400000, 1000000),
    labels = c(
      "<200,000", "200,000 to 299,999", "300,000 to 399,999",
      ">= 400,000"
    )
  ),
  tot_wage_cereal_smooth = list(
    breaks = c(0, 20000, 30000, 40000, 1000000),
    labels = c("<20,000", "20,000 to 29,999", "30,000 to 39,999", ">= 40,000")
  ),
  water_price_smooth = list(
    breaks = c(0, 10000, 22000, 34000, Inf),
    labels = c("<10,000", "10,000 to 19,999", "20,000 to 29,999", ">= 30,000")
  )
)