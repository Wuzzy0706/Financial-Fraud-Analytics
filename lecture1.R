library(tidyverse)
library(lubridate)

transactions <- read_csv("transactions.csv", show_col_types = FALSE)

glimpse(transactions)
summary(transactions)

amount_summary <- 
  summarise(
    transactions,
    mean_amount = mean(amount),
    median_amount = median(amount),
    p95_amount = quantile(amount, .95),
    number_of_transactions = n()
  )

glimpse(amount_summary)
write_csv(amount_summary, "outputs/amount_summary.csv")

# The red-flag table in the lecture uses device information,
# a high number of transactions in a short period, and location information.
flagged <-
  mutate(
    transactions,
    flag_amount = amount > quantile(amount, .95),
    flag_new_device = device_new == 1,
    flag_velocity = velocity_1h >= 4,
    flag_location = cross_border == 1,
    red_flag_count = flag_new_device + flag_velocity + flag_location,
    suspicious_case = red_flag_count >= 1
  )

glimpse(flagged)

summary_table <-
  summarise(
    flagged,
    rows = n(),
    accounts = n_distinct(account_id),
    fraudulent = sum(confirmed_fraud),
    nonfraudulent = sum(confirmed_fraud == 0),
    fraud_rate = mean(confirmed_fraud),
    missing_amount = sum(is.na(amount)),
    duplicate_tx_id = n() - n_distinct(tx_id),
    device_information = sum(flag_new_device),
    high_velocity = sum(flag_velocity),
    location_information = sum(flag_location),
    at_least_one_red_flag = sum(red_flag_count >= 1),
    two_or_more_red_flags = sum(red_flag_count >= 2)
  )

glimpse(summary_table)

flag_rates <-
  summarise(
    flagged,
    device_information = mean(flag_new_device),
    high_velocity = mean(flag_velocity),
    location_information = mean(flag_location),
    at_least_one_red_flag = mean(red_flag_count >= 1),
    two_or_more_red_flags = mean(red_flag_count >= 2)
  )

glimpse(flag_rates)

write_csv(summary_table, "outputs/summary.csv")
write_csv(flag_rates, "outputs/flag_rates.csv")
write_csv(flagged, "outputs/flagged_transactions.csv")
