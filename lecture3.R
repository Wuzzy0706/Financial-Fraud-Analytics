# The CSV file in data/ is supplied original synthetic teaching data.
# The comments below explain the purpose of each executable line for students.
# They are written in English to match the lecture slides and output tables.

# Install these packages once if they are not already installed.
# install.packages(c("readr", "ggplot2"))

# Load readr so that the script can read and write CSV files.
library(readr)
# Load ggplot2 so that the script can draw the residual diagnostic plot.
library(ggplot2)

# Fix the random seed so that fold assignments are reproducible.
set.seed(741003)
# Create the output folder if it does not already exist.
dir.create("outputs", showWarnings = FALSE, recursive = TRUE)


# 1. Import the analysis table and define the targets ---------------------

# Read the supplied transaction-level analysis table from the data folder.
transactions <- read_csv(
  # Use a relative path so that students can run the script from lab/L03.
  "L03_synthetic_feature_table.csv",
  # Suppress the automatic column-type message from readr.
  show_col_types = FALSE
)

# Store transaction IDs as character values rather than numeric values.
transactions$txn_id <- as.character(transactions$txn_id)
# Store customer IDs as character values because they are identifiers.
transactions$customer_id <- as.character(transactions$customer_id)
# Convert the observation date to an R Date for chronological splitting.
transactions$observed_at <- as.Date(transactions$observed_at)

# Convert channel to a factor and set the intended reference level to api.
transactions$channel <- factor(
  # Supply the original channel values.
  transactions$channel,
  # Define all allowed channel levels and their order.
  levels = c("api", "mobile", "web")
)

# Convert merchant risk to a factor and set low as the reference level.
transactions$merchant_risk_band <- factor(
  # Supply the original merchant-risk values.
  transactions$merchant_risk_band,
  # Define all allowed risk levels and their order.
  levels = c("low", "medium", "high")
)

# Store the binary target as integer values 0 and 1.
transactions$fraud_confirmed <- as.integer(transactions$fraud_confirmed)
# Store the continuous loss target as a numeric value.
transactions$estimated_loss_amount <- as.numeric(transactions$estimated_loss_amount)

# List every column that this exercise expects to find in the input table.
required_columns <- c(
  # Unique transaction identifier.
  "txn_id",
  # Customer identifier used to inspect repeated customers.
  "customer_id",
  # Date used for the time-ordered sample split.
  "observed_at",
  # Transaction amount available at decision time.
  "amount",
  # Amount relative to the customer's historical median.
  "amount_vs_customer_median",
  # Number of recent transactions used as a velocity feature.
  "velocity_24h",
  # Indicator for whether the device is new.
  "new_device",
  # Distance from the customer's normal geographic location.
  "geo_distance_km",
  # Age of the beneficiary in days.
  "beneficiary_age_days",
  # Transaction channel.
  "channel",
  # Merchant risk category.
  "merchant_risk_band",
  # Binary fraud outcome used by logistic regression.
  "fraud_confirmed",
  # Continuous loss outcome used by linear regression.
  "estimated_loss_amount"
)

# Find expected columns that are absent from the imported table.
missing_columns <- setdiff(required_columns, names(transactions))

# Stop immediately if the table is missing a required variable.
if (length(missing_columns) > 0) {
  # Show the missing names so that the data problem is easy to diagnose.
  stop("Missing required columns: ", paste(missing_columns, collapse = ", "))
}

# Check that the binary target does not contain missing labels.
if (anyNA(transactions$fraud_confirmed)) {
  # Stop because a missing target cannot be used to fit the models.
  stop("fraud_confirmed contains missing values.")
}

# Check that the binary target uses only the values 0 and 1.
if (!all(transactions$fraud_confirmed %in% c(0L, 1L))) {
  # Stop if another coding scheme has been supplied.
  stop("fraud_confirmed must contain only 0 and 1.")
}

# Check that the continuous loss target has no missing values.
if (anyNA(transactions$estimated_loss_amount)) {
  # Stop because missing outcomes cannot be summarized or modeled here.
  stop("estimated_loss_amount contains missing values.")
}

# Check that the loss target is not negative.
if (any(transactions$estimated_loss_amount < 0)) {
  # Stop because a negative loss is outside the teaching definition.
  stop("estimated_loss_amount must be non-negative.")
}

# Check that each transaction ID identifies one row only.
if (anyDuplicated(transactions$txn_id) > 0) {
  # Stop because duplicated IDs would make the unit of analysis ambiguous.
  stop("txn_id must be unique.")
}

# Document the unit, event, targets, timing, and repeated-entity structure.
target_specification <- data.frame(
  # Name each target-definition item.
  item = c(
    # Define the observation unit.
    "unit",
    # Define the observed event.
    "event",
    # Define the binary target.
    "binary_target",
    # Define the continuous target.
    "continuous_target",
    # Define predictor timing.
    "predictor_timing",
    # Define label timing.
    "label_timing",
    # Define repeated-entity structure.
    "repeated_entities"
  ),
  # Give the teaching definition for each item in the same order.
  definition = c(
    # State the unit of analysis.
    "One digital transaction.",
    # State when the event is observed.
    "Transaction observed at observed_at.",
    # State how the binary outcome is coded.
    "fraud_confirmed = 1 after review; 0 otherwise.",
    # State how the continuous loss outcome is used.
    "estimated_loss_amount is an outcome-only loss target.",
    # State the predictor-availability rule.
    "Classifier predictors must be available at the decision time.",
    # State the label-maturity assumption.
    "The supplied synthetic fraud labels are treated as complete; no maturity date is supplied.",
    # State that customers may appear in multiple rows.
    "A customer may contribute multiple transaction rows."
  ),
  # Keep text columns as text rather than converting them to factors.
  stringsAsFactors = FALSE
)

# Count missing values in every column of the imported table.
missing_summary <- data.frame(
  # Record the column names.
  variable = names(transactions),
  # Count missing entries column by column.
  missing = as.integer(colSums(is.na(transactions))),
  # Keep the variable names as character values.
  stringsAsFactors = FALSE
)

# Create a one-row quality summary for the analysis table.
target_quality <- data.frame(
  # Count all transaction rows.
  rows = nrow(transactions),
  # Count all variables.
  columns = ncol(transactions),
  # Count distinct transaction IDs.
  unique_transactions = length(unique(transactions$txn_id)),
  # Compare row count with distinct transaction count.
  duplicate_transaction_ids = nrow(transactions) - length(unique(transactions$txn_id)),
  # Count distinct customers.
  unique_customers = length(unique(transactions$customer_id)),
  # Find the first observed date.
  first_observed_date = min(transactions$observed_at),
  # Find the last observed date.
  last_observed_date = max(transactions$observed_at),
  # Count confirmed fraud rows.
  confirmed_fraud = sum(transactions$fraud_confirmed == 1),
  # Count rows not confirmed as fraud.
  not_confirmed_fraud = sum(transactions$fraud_confirmed == 0),
  # Calculate the overall fraud rate.
  fraud_rate = mean(transactions$fraud_confirmed),
  # Calculate the average loss amount.
  mean_loss_amount = mean(transactions$estimated_loss_amount),
  # Find the largest loss amount.
  max_loss_amount = max(transactions$estimated_loss_amount)
)

# Count rows for each value of the binary target.
target_counts <- table(transactions$fraud_confirmed)
# Turn the target counts into a small data frame for export.
target_distribution <- data.frame(
  # Convert the table names back to integer target values.
  fraud_confirmed = as.integer(names(target_counts)),
  # Store the number of rows in each target class.
  rows = as.integer(target_counts),
  # Keep the target values as numeric values.
  stringsAsFactors = FALSE
)

# Add a human-readable label for each target class.
target_distribution$target_label <- ifelse(
  # Test whether each target value indicates confirmed fraud.
  target_distribution$fraud_confirmed == 1,
  # Label target value 1 as confirmed fraud.
  "confirmed fraud",
  # Label target value 0 as not confirmed fraud.
  "not confirmed fraud"
)

# Convert class counts into proportions of all rows.
target_distribution$rate <- target_distribution$rows / sum(target_distribution$rows)
# Keep the output columns in a student-friendly order.
target_distribution <- target_distribution[
  # Select every row and the four named output columns.
  , c("fraud_confirmed", "target_label", "rows", "rate")
]

# Print the target specification for an immediate console review.
print(target_specification)
# Print the missing-value summary for an immediate console review.
print(missing_summary)
# Print the data-quality summary for an immediate console review.
print(target_quality)
# Print the target distribution for an immediate console review.
print(target_distribution)


# 2. Create chronological development and test samples -------------------

# Represent the future-use question with a time-ordered split.
# Use earlier rows to estimate the model.
# Use middle rows to support development choices.
# Keep later rows untouched until the procedure is fixed.
# Define the last date included in the training sample.
cut_1 <- as.Date("2025-03-15")
# Define the last date included in the validation sample.
cut_2 <- as.Date("2025-04-15")


train <- transactions[
  # Keep observations on or before cut_1 for model training.
  transactions$observed_at <= cut_1,
  # Keep all columns.
  , 
  # Preserve a data frame even if only one row is selected.
  drop = FALSE
  ]

# Keep observations after cut_1 and on or before cut_2 for validation.
validation <- transactions[transactions$observed_at > cut_1 & transactions$observed_at <= cut_2, ,drop = FALSE
]

# Keep observations after cut_2 as the final test sample.
test <- transactions[transactions$observed_at > cut_2, , drop = FALSE]

# Check that all three samples contain observations.
if (nrow(train) == 0 || nrow(validation) == 0 || nrow(test) == 0) {
  # Stop if any time period is empty.
  stop("All three chronological samples must contain rows.")
}

# Check that no transaction ID appears in two samples.
if (
  # Check for overlap between training and validation.
  length(intersect(train$txn_id, validation$txn_id)) > 0 ||
    # Check for overlap between training and test.
    length(intersect(train$txn_id, test$txn_id)) > 0 ||
    # Check for overlap between validation and test.
    length(intersect(validation$txn_id, test$txn_id)) > 0
 ) { # Enter this block when two chronological samples share an ID.
  # Stop if the transaction-level split is not mutually exclusive.
  stop("Chronological samples must not share transaction IDs.")
}

# Define a reusable function for summarizing one sample.
make_split_summary <- function(data, sample_name) {
  # Return one row containing dates, counts, and the fraud rate.
  data.frame(
    # Store the sample label.
    sample = sample_name,
    # Count rows in this sample.
    rows = nrow(data),
    # Record the first observed date.
    first_date = min(data$observed_at),
    # Record the last observed date.
    last_date = max(data$observed_at),
    # Count confirmed fraud rows.
    confirmed_fraud = sum(data$fraud_confirmed == 1),
    # Count rows not confirmed as fraud.
    not_confirmed_fraud = sum(data$fraud_confirmed == 0),
    # Calculate the sample fraud rate.
    fraud_rate = mean(data$fraud_confirmed),
    # Keep the sample label as text.
    stringsAsFactors = FALSE
  )
}

# Combine one summary row for each chronological sample.
split_summary <- rbind(
  # Summarize the training period.
  make_split_summary(train, "train"),
  # Summarize the validation period.
  make_split_summary(validation, "validation"),
  # Summarize the final test period.
  make_split_summary(test, "test")
)

# List distinct training customers and label their sample.
train_customers <- data.frame(
  # Keep each distinct customer ID from training.
  customer_id = unique(train$customer_id),
  # Label all these customer IDs as training customers.
  sample = "train",
  # Keep the label as text.
  stringsAsFactors = FALSE
)

# List distinct validation customers and label their sample.
validation_customers <- data.frame(
  # Keep each distinct customer ID from validation.
  customer_id = unique(validation$customer_id),
  # Label all these customer IDs as validation customers.
  sample = "validation",
  # Keep the label as text.
  stringsAsFactors = FALSE
)

# List distinct test customers and label their sample.
test_customers <- data.frame(
  # Keep each distinct customer ID from test.
  customer_id = unique(test$customer_id),
  # Label all these customer IDs as test customers.
  sample = "test",
  # Keep the label as text.
  stringsAsFactors = FALSE
)

# Combine customer membership from all three samples.
customers_by_sample <- rbind(
  # Add training customer membership.
  train_customers,
  # Add validation customer membership.
  validation_customers,
  # Add test customer membership.
  test_customers
)

# Count how many samples contain each customer.
number_of_samples <- table(customers_by_sample$customer_id)

# Summarize customer overlap across the time split.
customer_split_summary <- data.frame(
  # Count all distinct customers.
  customers = length(number_of_samples),
  # Count customers appearing in more than one sample.
  customers_in_multiple_samples = sum(number_of_samples > 1),
  # Find the largest number of samples containing one customer.
  maximum_samples_per_customer = max(number_of_samples)
)


# Print the transaction-level split summary.
print(split_summary)
# Print the customer-overlap summary.
print(customer_split_summary)


# 3. Prepare decision-time transformations -------------------------------

# Define transformations that are available at decision time.
add_decision_time_transformations <- function(data) {
  # Copy the input data before adding new columns.
  result <- data
  # Use log1p to calculate log(1 + amount), including amount equal to zero.
  result$log_amount <- log1p(result$amount)
  # Reduce the scale of geographic distance with log(1 + distance).
  result$log_geo_distance_km <- log1p(result$geo_distance_km)
  # Reduce the scale of beneficiary age with log(1 + age).
  result$log_beneficiary_age_days <- log1p(result$beneficiary_age_days)
  # Return the original columns plus the three transformed columns.
  result
}

# Apply the same fixed transformations to the training sample.
train <- add_decision_time_transformations(train)
# Apply the same fixed transformations to the validation sample.
validation <- add_decision_time_transformations(validation)
# Apply the same fixed transformations to the test sample.
test <- add_decision_time_transformations(test)

# Document where each preprocessing step is fitted and applied.
preprocessing_scope <- data.frame(
  # Name each preprocessing step.
  preprocessing_step = c(
    # Document the logarithmic transformations.
    "logarithmic transformations",
    # Document the fixed scorecard bins.
    "scorecard bins",
    # Document the WOE mapping.
    "WOE values"
  ),
  # State how each preprocessing step is determined.
  fit_on = c(
    # The log formulas are fixed in advance.
    "fixed decision-time formula",
    # The bin boundaries are fixed in this script.
    "fixed boundaries documented in this script",
    # WOE values are estimated from training only.
    "training sample only"
  ),
  # State which samples receive the step.
  applied_to = c(
    # Apply the log formulas to every sample.
    "train, validation, and test",
    # Apply the fixed bins to every sample.
    "train, validation, and test",
    # Apply the training WOE mapping without refitting it.
    "train, validation, and test"
  ),
  # Keep the documentation fields as character values.
  stringsAsFactors = FALSE
)

# 4. Fit linear regression for the continuous target ----------------------

# Define the linear-regression formula for the continuous loss target.
linear_formula <- estimated_loss_amount ~
  # Include transformed amount and decision-time numeric predictors.
  log_amount + velocity_24h + new_device +
  # Include transformed distance and beneficiary-age predictors.
  log_geo_distance_km + log_beneficiary_age_days +
  # Include the two categorical predictors.
  channel + merchant_risk_band

# Fit ordinary least-squares linear regression on training data only.
linear_fit <- lm(linear_formula, data = train)
# Print the standard regression summary for classroom interpretation.
print(summary(linear_fit))

# Extract the coefficient matrix from the linear-model summary.
linear_coefficient_matrix <- as.data.frame(summary(linear_fit)$coefficients)
# Build a clearly named coefficient table for export.
linear_coefficients <- data.frame(
  # Store the coefficient names, including the intercept.
  term = rownames(linear_coefficient_matrix),
  # Store the estimated coefficient values.
  estimate = linear_coefficient_matrix[["Estimate"]],
  # Store the standard errors.
  standard_error = linear_coefficient_matrix[["Std. Error"]],
  # Store the t statistics.
  test_statistic = linear_coefficient_matrix[["t value"]],
  # Store the two-sided p values.
  p_value = linear_coefficient_matrix[["Pr(>|t|)"]],
  # Keep the term names as text.
  stringsAsFactors = FALSE
)

# Create training diagnostics containing fitted values and residuals.
linear_training_diagnostics <- data.frame(
  # Keep the transaction identifier for tracing observations.
  txn_id = train$txn_id,
  # Keep the observation date.
  observed_at = train$observed_at,
  # Keep the observed loss outcome.
  estimated_loss_amount = train$estimated_loss_amount,
  # Calculate the model fitted value for every training row.
  fitted_value = as.numeric(fitted(linear_fit)),
  # Calculate observed outcome minus fitted value.
  residual = as.numeric(residuals(linear_fit)),
  # Keep text columns as text.
  stringsAsFactors = FALSE
)

# Predict the continuous loss target for the untouched test period.
linear_test_predictions <- data.frame(
  # Keep the transaction identifier.
  txn_id = test$txn_id,
  # Keep the observation date.
  observed_at = test$observed_at,
  # Keep the observed test loss for later comparison.
  estimated_loss_amount = test$estimated_loss_amount,
  # Apply the fitted linear model to the test predictors.
  predicted_loss_amount = as.numeric(predict(linear_fit, newdata = test)),
  # Keep text columns as text.
  stringsAsFactors = FALSE
)

# Start a residual-versus-fitted diagnostic plot.
linear_residual_plot <- ggplot(
  # Use the training diagnostic table as the plotting data.
  linear_training_diagnostics,
  # Put fitted values on the x-axis and residuals on the y-axis.
  aes(x = fitted_value, y = residual)
) + # Add the point layer to the residual plot.
  # Draw one point for each training transaction.
  geom_point(alpha = 0.45, colour = "#1B5E20") +
  # Add a horizontal reference line at residual equal to zero.
  geom_hline(
    # Set the reference line height.
    yintercept = 0,
    # Use a dashed line so it is visually distinct from points.
    linetype = "dashed",
    # Use a neutral grey colour for the reference line.
    colour = "#8A8F83"
  ) + # Add the zero-residual reference line.
  # Add a descriptive title and axis labels.
  labs(
    # Describe the diagnostic being shown.
    title = "Linear regression residuals versus fitted values",
    # Label the horizontal axis.
    x = "Fitted loss amount",
    # Label the vertical axis.
    y = "Residual"
  ) + # Add the plot title and axis labels.
  # Use a clean theme with a readable base font size.
  theme_minimal(base_size = 12)

# Save the linear-model coefficient table.
write_csv(linear_coefficients, "outputs/L03_linear_coefficients.csv")
# Save the training fitted values and residuals.
write_csv(linear_training_diagnostics, "outputs/L03_linear_training_diagnostics.csv")
# Save the test-period loss predictions.
write_csv(linear_test_predictions, "outputs/L03_linear_test_predictions.csv")

# Save the residual diagnostic plot as a PNG file.
ggsave(
  # Set the output file path.
  "outputs/L03_linear_residuals.png",
  # Supply the plot object created above.
  linear_residual_plot,
  # Set the plot width in inches.
  width = 7,
  # Set the plot height in inches.
  height = 5,
  # Set the output resolution.
  dpi = 160
)


# 5. Fit and interpret logistic regression -------------------------------

# Define the candidate terms that can be considered by add1().
logistic_scope <- ~
  # Include the continuous decision-time predictors and transformations.
  log_amount + amount_vs_customer_median + velocity_24h +
  # Include the device, distance, and beneficiary-age predictors.
  new_device + log_geo_distance_km + log_beneficiary_age_days +
  # Include the two categorical predictors.
  channel + merchant_risk_band

# Define the full logistic-regression formula for the binary target.
logistic_formula <- fraud_confirmed ~
  # Include amount, relative amount, and velocity.
  log_amount + amount_vs_customer_median + velocity_24h +
  # Include device, distance, and beneficiary-age information.
  new_device + log_geo_distance_km + log_beneficiary_age_days +
  # Include channel and merchant risk as categorical predictors.
  channel + merchant_risk_band

# Fit logistic regression on the training sample.
logistic_fit <- glm(
  # Supply the binary-outcome formula.
  logistic_formula,
  # Use only the training observations for estimation.
  data = train,
  # Use the binomial family for a binary target.
  family = binomial()
)

# Print the logistic-model summary for coefficient interpretation.
print(summary(logistic_fit))

# Define a helper function that extracts common GLM coefficient fields.
extract_glm_coefficients <- function(fit) {
  # Extract the coefficient matrix from the fitted model summary.
  coefficient_matrix <- as.data.frame(summary(fit)$coefficients)
  # Return a consistently named coefficient table.
  data.frame(
    # Store the term names, including the intercept and factor contrasts.
    term = rownames(coefficient_matrix),
    # Store the estimated log-odds coefficients.
    estimate = coefficient_matrix[["Estimate"]],
    # Store the standard errors.
    standard_error = coefficient_matrix[["Std. Error"]],
    # Store the z statistics for the logistic model.
    test_statistic = coefficient_matrix[["z value"]],
    # Store the two-sided p values.
    p_value = coefficient_matrix[["Pr(>|z|)" ]],
    # Keep term labels as text.
    stringsAsFactors = FALSE
  )
}

# List the model terms that are numeric rather than factor contrasts.
numeric_model_terms <- c(
  # Log-transformed transaction amount.
  "log_amount",
  # Amount relative to the customer median.
  "amount_vs_customer_median",
  # Recent transaction velocity.
  "velocity_24h",
  # New-device indicator.
  "new_device",
  # Log-transformed geographic distance.
  "log_geo_distance_km",
  # Log-transformed beneficiary age.
  "log_beneficiary_age_days"
)

# Extract the fitted logistic coefficients.
logistic_coefficients <- extract_glm_coefficients(logistic_fit)
# Convert each coefficient from log odds to an odds ratio.
logistic_coefficients$odds_ratio <- exp(logistic_coefficients$estimate)

# Create an empty column for the change in the model term that doubles odds.
logistic_coefficients$odds_doubling_change_in_model_term <- NA_real_

# Identify rows that correspond to the named numeric model terms.
valid_terms <- logistic_coefficients$term %in% numeric_model_terms

# Exclude zero coefficients because division by zero is undefined.
valid_terms <- valid_terms & logistic_coefficients$estimate != 0

# Calculate log(2) divided by the coefficient for valid numeric terms.
logistic_coefficients$odds_doubling_change_in_model_term[valid_terms] <-
  # Divide log(2) by each valid model coefficient.
  log(2) / logistic_coefficients$estimate[valid_terms]


# Save the logistic coefficient interpretation table.
write_csv(logistic_coefficients, "outputs/L03_logistic_coefficients.csv")

# Print the coefficient table.
print(logistic_coefficients)

# Start with the validation data before adding model predictions.
validation_scored <- validation

# Predict fraud probabilities for validation rows.
validation_scored$fraud_prob <- as.numeric(
  # Request response-scale probabilities for validation rows.
  predict(logistic_fit, newdata = validation, type = "response")
)

# Start with the test data before adding model predictions.
test_scored <- test
# Predict fraud probabilities for test rows.
test_scored$fraud_prob <- as.numeric(
  # Request response-scale probabilities for test rows.
  predict(logistic_fit, newdata = test, type = "response")
)

# Save only identification, outcome, and probability columns for validation.
write_csv(
  # Select the validation prediction columns before writing.
  validation_scored[
    # Select only the four columns intended for the prediction file.
    , c("txn_id", "observed_at", "fraud_confirmed", "fraud_prob")
    # Pass the selected validation rows to write_csv().
  ],
  # Save the validation predictions to this CSV path.
  "outputs/L03_validation_predictions.csv"
)

# Save only identification, outcome, and probability columns for test.
write_csv(
  # Select the test prediction columns before writing.
  test_scored[
    # Select only the four columns intended for the prediction file.
    , c("txn_id", "observed_at", "fraud_confirmed", "fraud_prob")
    # Pass the selected test rows to write_csv().
  ],
  # Save the test predictions to this CSV path.
  "outputs/L03_test_predictions.csv"
)

# 7. Calculate training-only WOE values and fit a scorecard ---------------

# List the scorecard characteristics that will receive fixed bins and WOE.
scorecard_characteristics <- c(
  # Bin the transaction amount.
  "amount_bin",
  # Bin recent transaction velocity.
  "velocity_bin",
  # Convert the new-device indicator into labelled bins.
  "new_device_bin",
  # Bin geographic distance.
  "geo_band",
  # Bin beneficiary age.
  "beneficiary_age_band",
  # Bin the amount-to-customer-median ratio.
  "amount_ratio_band",
  # Treat transaction channel as a scorecard characteristic.
  "channel_bin",
  # Treat merchant risk band as a scorecard characteristic.
  "merchant_risk_band_bin"
)

# Define a function that adds the fixed scorecard bins to a data frame.
make_scorecard_bins <- function(data) {
  # Copy the input data before adding scorecard variables.
  result <- data

  # Cut amount into four fixed intervals.
  result$amount_bin <- cut(
    # Use the original amount variable.
    result$amount,
    # Define the interval boundaries, including open-ended tails.
    breaks = c(-Inf, 100, 300, 500, Inf),
    # Make intervals left-closed and right-open: [a, b).
    right = FALSE,
    # Give each interval a readable label.
    labels = c("<100", "100-300", "300-500", ">=500"),
    # Include the lowest value in the first interval.
    include.lowest = TRUE
  )

  # Cut 24-hour velocity into three fixed intervals.
  result$velocity_bin <- cut(
    # Use the transaction velocity variable.
    result$velocity_24h,
    # Define the velocity boundaries.
    breaks = c(-Inf, 1, 3, Inf),
    # Use left-closed and right-open intervals.
    right = FALSE,
    # Label zero, one-to-two, and three-or-more transactions.
    labels = c("0", "1-2", "3+"),
    # Include the lowest value in the first interval.
    include.lowest = TRUE
  )

  # Convert the numeric device indicator into readable factor levels.
  result$new_device_bin <- factor(
    # Label 1 as a new device and 0 as a known device.
    ifelse(result$new_device == 1, "new device", "known device"),
    # Use known device as the reference level.
    levels = c("known device", "new device")
  )

  # Cut geographic distance into three fixed bands.
  result$geo_band <- cut(
    # Use the original distance variable.
    result$geo_distance_km,
    # Define the distance boundaries.
    breaks = c(-Inf, 10, 50, Inf),
    # Use left-closed and right-open intervals.
    right = FALSE,
    # Label the geographic-distance bands.
    labels = c("<10", "10-50", ">=50"),
    # Include the lowest value in the first interval.
    include.lowest = TRUE
  )

  # Cut beneficiary age into four fixed bands.
  result$beneficiary_age_band <- cut(
    # Use beneficiary age in days.
    result$beneficiary_age_days,
    # Define the beneficiary-age boundaries.
    breaks = c(-Inf, 30, 90, 180, Inf),
    # Use left-closed and right-open intervals.
    right = FALSE,
    # Label each age band.
    labels = c("<30", "30-90", "90-180", ">=180"),
    # Include the lowest value in the first interval.
    include.lowest = TRUE
  )

  # Cut the amount-to-median ratio into four fixed bands.
  result$amount_ratio_band <- cut(
    # Use the ratio available at decision time.
    result$amount_vs_customer_median,
    # Define the ratio boundaries.
    breaks = c(-Inf, 0.75, 1.5, 3, Inf),
    # Use left-closed and right-open intervals.
    right = FALSE,
    # Label each ratio band.
    labels = c("<0.75", "0.75-1.5", "1.5-3", ">=3"),
    # Include the lowest value in the first interval.
    include.lowest = TRUE
  )

  # Copy the channel levels into a scorecard factor.
  result$channel_bin <- factor(
    # Convert the original factor to text before rebuilding its levels.
    as.character(result$channel),
    # Preserve api as the first and reference level.
    levels = c("api", "mobile", "web")
  )

  # Copy the merchant risk levels into a scorecard factor.
  result$merchant_risk_band_bin <- factor(
    # Convert the original factor to text before rebuilding its levels.
    as.character(result$merchant_risk_band),
    # Preserve low as the first and reference level.
    levels = c("low", "medium", "high")
  )

  # Return the original data plus all scorecard bins.
  result
}

# Apply exactly the same fixed bin boundaries to training data.
train_binned <- make_scorecard_bins(train)
# Apply exactly the same fixed bin boundaries to validation data.
validation_binned <- make_scorecard_bins(validation)
# Apply exactly the same fixed bin boundaries to test data.
test_binned <- make_scorecard_bins(test)

# Define a function that calculates WOE using one training characteristic.
calculate_woe <- function(data, characteristic) {
  # Read the complete ordered level list for this characteristic.
  characteristic_levels <- levels(data[[characteristic]])
  
  # Count all non-fraud training observations.
  nonfraud_total <- sum(data$fraud_confirmed == 0)
  
  # Count all fraud training observations.
  fraud_total <- sum(data$fraud_confirmed == 1)

  # Convert bin values to a factor containing every fixed level.
  bin_values <- factor(
    # Convert the current characteristic values to text for matching.
    as.character(data[[characteristic]]),
    # Force the mapping to use the complete training level list.
    levels = characteristic_levels
  )
  # Convert the target to a factor with columns 0 and 1.
  class_values <- factor(
    # Supply the observed binary target values.
    data$fraud_confirmed,
    # Force the class order to be non-fraud first and fraud second.
    levels = c(0L, 1L)
  )
  # Count observations by bin and fraud class.
  counts <- table(bin_values, class_values)

  # Create one WOE row for every bin, including bins with zero counts.
  result <- data.frame(
    # Record which characteristic produced the row.
    variable = characteristic,
    # Record that the WOE mapping was fitted on training data.
    fit_sample = "train",
    # Store the ordered bin labels.
    bin = characteristic_levels,
    # Store the count of non-fraud observations in each bin.
    nonfraud_count = as.integer(counts[, "0"]),
    # Store the count of fraud observations in each bin.
    fraud_count = as.integer(counts[, "1"]),
    # Keep the result labels as text.
    stringsAsFactors = FALSE
  )

  # Convert non-fraud bin counts into a distribution across non-fraud rows.
  result$nonfraud_distribution <- result$nonfraud_count / nonfraud_total
  
  # Convert fraud bin counts into a distribution across fraud rows.
  result$fraud_distribution <- result$fraud_count / fraud_total
  
  # Calculate WOE as log(non-fraud distribution / fraud distribution).
  result$woe <- log(
    # Divide the non-fraud distribution by the fraud distribution.
    result$nonfraud_distribution / result$fraud_distribution
  )

  # Reject bins with no observations from either target class.
  if (any(result$nonfraud_count == 0 | result$fraud_count == 0)) {
    # Tell the student which characteristic needs coarser bins.
    stop(
      # Explain why a zero class count prevents a direct WOE calculation.
      "WOE requires both classes in every bin for ",
      # Identify the characteristic that contains the problematic bin.
      characteristic,
      # Suggest the teaching remedy.
      ". Coarsen the fixed bins."
    )
  }

  # Return the WOE mapping for this characteristic.
  result
}

# Create an empty WOE table before adding one characteristic at a time.
woe_table <- data.frame(
  # Reserve the characteristic-name column.
  variable = character(0),
  # Reserve the training-sample label column.
  fit_sample = character(0),
  # Reserve the bin-label column.
  bin = character(0),
  # Reserve the non-fraud count column.
  nonfraud_count = integer(0),
  # Reserve the fraud count column.
  fraud_count = integer(0),
  # Reserve the non-fraud distribution column.
  nonfraud_distribution = numeric(0),
  # Reserve the fraud distribution column.
  fraud_distribution = numeric(0),
  # Reserve the WOE column.
  woe = numeric(0),
  # Keep text columns as text.
  stringsAsFactors = FALSE
)

# Calculate and append the WOE mapping for every characteristic.
for (i in seq_along(scorecard_characteristics)) {
  # Calculate the mapping for the characteristic at position i.
  one_characteristic <- calculate_woe(
    # Use the training-binned data to fit WOE.
    train_binned,
    # Supply the current characteristic name.
    scorecard_characteristics[i]
  )
  # Append the current characteristic to the complete WOE table.
  woe_table <- rbind(woe_table, one_characteristic)
}

# Remove row names created by repeated rbind() calls.
rownames(woe_table) <- NULL

# Define a function that applies training WOE values to any sample.
apply_woe_mapping <- function(data, mapping, characteristics) {
  # Copy the input data before adding WOE columns.
  result <- data

  # Apply one lookup mapping for each scorecard characteristic.
  for (characteristic in characteristics) {
    # Name the new WOE column after the characteristic.
    woe_column <- paste0("woe_", characteristic)
    # Select WOE-table rows belonging to the current characteristic.
    mapping_rows <- mapping$variable == characteristic
    # Extract the ordered bin labels from the mapping.
    lookup_bins <- mapping$bin[mapping_rows]
    # Extract the corresponding WOE values from the mapping.
    lookup_woe <- mapping$woe[mapping_rows]

    # Match each sample bin to its training WOE value.
    result[[woe_column]] <- lookup_woe[
      # Convert sample bins to text before matching the training labels.
      match(as.character(result[[characteristic]]), lookup_bins)
    ]

    # Stop if a new or missing bin cannot be mapped.
    if (anyNA(result[[woe_column]])) {
      # Report the characteristic with the failed lookup.
      stop(
        # Explain that the sample contains an unmapped bin.
        "Unseen or missing scorecard bin while applying ",
        # Identify the characteristic with the failed lookup.
        characteristic
      )
    }
  }

  # Return the original data plus all mapped WOE columns.
  result
}

# Apply training-fitted WOE mappings to the training sample.
train_woe <- apply_woe_mapping(
  # Apply the mapping to the training-binned data.
  train_binned,
  # Use the WOE values fitted above.
  woe_table,
  # Apply every listed scorecard characteristic.
  scorecard_characteristics
)

# Apply the unchanged training-fitted WOE mappings to validation.
validation_woe <- apply_woe_mapping(
  # Apply the mapping to validation-binned data.
  validation_binned,
  # Reuse the training WOE mapping without refitting.
  woe_table,
  # Apply every listed scorecard characteristic.
  scorecard_characteristics
)

# Apply the unchanged training-fitted WOE mappings to test.
test_woe <- apply_woe_mapping(
  # Apply the mapping to test-binned data.
  test_binned,
  # Reuse the training WOE mapping without refitting.
  woe_table,
  # Apply every listed scorecard characteristic.
  scorecard_characteristics
)

# Define the logistic formula using WOE-transformed characteristics.
scorecard_woe_formula <- fraud_confirmed ~
  # Include the WOE amount and velocity variables.
  woe_amount_bin + woe_velocity_bin + woe_new_device_bin +
  # Include WOE distance, age, and amount-ratio variables.
  woe_geo_band + woe_beneficiary_age_band + woe_amount_ratio_band +
  # Include WOE channel and merchant-risk variables.
  woe_channel_bin + woe_merchant_risk_band_bin

# Fit the WOE logistic scorecard on training data only.
scorecard_fit <- glm(
  # Supply the WOE-based logistic formula.
  scorecard_woe_formula,
  # Use the training sample containing training-fitted WOE values.
  data = train_woe,
  # Use the binomial family for the binary target.
  family = binomial()
)

# Extract the WOE-scorecard coefficient table.
scorecard_coefficients <- extract_glm_coefficients(scorecard_fit)
# Add odds ratios to the scorecard coefficient table.
scorecard_coefficients$odds_ratio <- exp(scorecard_coefficients$estimate)

# Set the scorecard reference score when fraud odds equal 1:1.
base_score <- 500
# Define the reference fraud odds as P(fraud) / P(nonfraud).
base_fraud_odds <- 1
# Define the points-to-double-odds value from the teaching convention.
pdo <- 20
# Convert the PDO value into the factor used with natural log odds.
score_factor <- pdo / log(2)
# Calculate the offset that gives base_score at base_fraud_odds.
score_offset <- base_score - score_factor * log(base_fraud_odds)
# Extract the fitted intercept from the WOE logistic model.
scorecard_intercept <- unname(coef(scorecard_fit)[["(Intercept)"]])
# Calculate the fixed base points contributed by offset and intercept.
base_points <- score_offset + scorecard_intercept * score_factor

# Document the scorecard scaling parameters for students.
scorecard_parameters <- data.frame(
  # Name each scorecard parameter.
  parameter = c(
    "base_score",
    "base_fraud_odds",
    "pdo",
    "factor",
    "offset",
    "intercept",
    "base_points"
  ),
  # Store the numeric value of each parameter.
  value = c(
    base_score,
    base_fraud_odds,
    pdo,
    score_factor,
    score_offset,
    scorecard_intercept,
    base_points
  ),
  # Explain the role of every parameter.
  definition = c(
    "Score when fraud odds are 1:1.",
    "Reference fraud odds P(fraud) / P(nonfraud).",
    "Points added when fraud odds double.",
    "PDO divided by log(2).",
    "Score adjustment for the reference odds.",
    "Intercept from the WOE logistic model.",
    "Offset plus scaled model intercept."
  ),
  # Keep parameter names and definitions as text.
  stringsAsFactors = FALSE
)

# Map each original scorecard characteristic to its WOE model term.
scorecard_term_map <- data.frame(
  # Store the characteristic names.
  variable = scorecard_characteristics,
  # Build the corresponding woe_ term names.
  term = paste0("woe_", scorecard_characteristics),
  # Keep labels as text.
  stringsAsFactors = FALSE
)

# Start the scorecard-points table from the WOE mapping.
scorecard_points <- woe_table
# Add the WOE model term corresponding to each characteristic.
scorecard_points$term <- scorecard_term_map$term[
  # Match each WOE row's variable name to the term map.
  match(scorecard_points$variable, scorecard_term_map$variable)
]
# Add the fitted logistic coefficient for each WOE term.
scorecard_points$coefficient <- scorecard_coefficients$estimate[
  # Match each WOE term to its fitted coefficient.
  match(scorecard_points$term, scorecard_coefficients$term)
]
# Calculate each bin's contribution to the log-odds score.
scorecard_points$log_odds_contribution <-
  # Multiply the fitted coefficient by the bin WOE value.
  scorecard_points$coefficient * scorecard_points$woe
# Convert each raw log-odds contribution into scaled scorecard points.
scorecard_points$points <-
  scorecard_points$log_odds_contribution * score_factor
# Keep only the columns needed for the teaching scorecard table.
scorecard_points <- scorecard_points[
  # Keep every row and choose the columns listed below.
  , c(
    # Characteristic name.
    "variable",
    # Training sample label.
    "fit_sample",
    # Bin label.
    "bin",
    # Non-fraud count.
    "nonfraud_count",
    # Fraud count.
    "fraud_count",
    # Non-fraud distribution.
    "nonfraud_distribution",
    # Fraud distribution.
    "fraud_distribution",
    # Weight of evidence.
    "woe",
    # Fitted WOE coefficient.
    "coefficient",
    # Contribution to the log-odds score.
    "log_odds_contribution",
    # Scaled points contributed by this characteristic-bin combination.
    "points"
  )
]

# Store the WOE term names used by the scorecard prediction function.
scorecard_woe_terms <- scorecard_term_map$term

# Define a function that manually reconstructs the WOE model log odds.
scorecard_linear_predictor <- function(data, fit, terms) {
  # Extract all fitted coefficients, including the intercept.
  fitted_coefficients <- coef(fit)

  # Check that every requested WOE term has a usable coefficient.
  if (any(is.na(fitted_coefficients[terms]))) {
    # Stop rather than silently producing an incomplete score.
    stop("The scorecard fit is missing a coefficient.")
  }

  # Check that the fitted intercept is available.
  if (is.na(fitted_coefficients[["(Intercept)"]])) {
    # Stop rather than silently producing an incomplete score.
    stop("The scorecard fit is missing an intercept.")
  }

  # Start the total WOE contribution for every row at zero.
  total_contribution <- numeric(nrow(data))

  # Add coefficient times WOE for each scorecard term.
  for (term in terms) {
    # Add the current term's contribution to every row's total.
    total_contribution <- total_contribution +
      # Multiply the current WOE value by its fitted coefficient.
      data[[term]] * unname(fitted_coefficients[[term]])
  }

  # Add the intercept and return raw fraud log odds for each row.
  unname(fitted_coefficients[["(Intercept)"]]) + total_contribution
}

# Define a function that creates one sample's additive scorecard output.
make_scorecard_predictions <- function(
  data,
  fit,
  terms,
  sample_name,
  points_table,
  characteristics
) {
  # Calculate the raw fraud log odds from the WOE logistic model.
  log_odds <- scorecard_linear_predictor(data, fit, terms)

  # Create the basic transaction-level output columns.
  result <- data.frame(
    # Record whether the row belongs to train, validation, or test.
    sample = sample_name,
    # Keep the transaction identifier.
    txn_id = data$txn_id,
    # Keep the observation date.
    observed_at = data$observed_at,
    # Keep the observed fraud outcome.
    fraud_confirmed = data$fraud_confirmed,
    # Keep raw model log odds for teaching and checking.
    log_odds = log_odds,
    # Repeat the fixed base points for every transaction.
    base_points = base_points,
    # Start the final scaled score at the base points.
    total_score = base_points,
    # Keep text columns as text.
    stringsAsFactors = FALSE
  )

  # Add one points column for every scorecard characteristic.
  for (characteristic in characteristics) {
    # Name the points column after the characteristic.
    points_column <- paste0("points_", characteristic)
    # Select lookup rows for the current characteristic.
    mapping_rows <- points_table$variable == characteristic
    # Extract the bin labels from the common scorecard table.
    lookup_bins <- points_table$bin[mapping_rows]
    # Extract the scaled points from the common scorecard table.
    lookup_points <- points_table$points[mapping_rows]

    # Match each transaction's bin to its points contribution.
    result[[points_column]] <- lookup_points[
      match(as.character(data[[characteristic]]), lookup_bins)
    ]

    # Stop if a transaction has a bin absent from the common lookup table.
    if (anyNA(result[[points_column]])) {
      # Identify the characteristic with the failed lookup.
      stop("Unseen or missing scorecard bin while calculating ", characteristic)
    }

    # Add this characteristic's points to the transaction's total score.
    result$total_score <- result$total_score + result[[points_column]]
  }

  # Convert the scaled score back to raw log odds and then to probability.
  result$fraud_prob <- plogis(
    (result$total_score - score_offset) / score_factor
  )

  # Return one additive scorecard result row for every transaction.
  result
}

# Score the training sample with the common WOE scorecard.
train_scorecard <- make_scorecard_predictions(
  # Supply training rows containing the mapped WOE variables.
  train_woe,
  # Supply the fitted WOE logistic model.
  scorecard_fit,
  # Supply the ordered WOE model term names.
  scorecard_woe_terms,
  # Label these rows as training rows.
  "train",
  # Supply the common bin-to-points lookup table.
  scorecard_points,
  # Supply every scorecard characteristic.
  scorecard_characteristics
)

# Score the validation sample with the same WOE scorecard.
validation_scorecard <- make_scorecard_predictions(
  # Supply validation rows containing the mapped WOE variables.
  validation_woe,
  # Supply the fitted WOE logistic model.
  scorecard_fit,
  # Supply the ordered WOE model term names.
  scorecard_woe_terms,
  # Label these rows as validation rows.
  "validation",
  # Supply the common bin-to-points lookup table.
  scorecard_points,
  # Supply every scorecard characteristic.
  scorecard_characteristics
)

# Score the untouched test sample with the same WOE scorecard.
test_scorecard <- make_scorecard_predictions(
  # Supply test rows containing the mapped WOE variables.
  test_woe,
  # Supply the fitted WOE logistic model.
  scorecard_fit,
  # Supply the ordered WOE model term names.
  scorecard_woe_terms,
  # Label these rows as test rows.
  "test",
  # Supply the common bin-to-points lookup table.
  scorecard_points,
  # Supply every scorecard characteristic.
  scorecard_characteristics
)

# Combine all transaction-level scorecard results into one table.
scorecard_predictions <- rbind(
  # Add training scorecard rows first.
  train_scorecard,
  # Add validation scorecard rows second.
  validation_scorecard,
  # Add test scorecard rows last.
  test_scorecard
)

# Calculate model probabilities for the training sample.
train_model_probability <- as.numeric(
  predict(scorecard_fit, newdata = train_woe, type = "response")
)
# Calculate model probabilities for the validation sample.
validation_model_probability <- as.numeric(
  predict(scorecard_fit, newdata = validation_woe, type = "response")
)
# Calculate model probabilities for the test sample.
test_model_probability <- as.numeric(
  predict(scorecard_fit, newdata = test_woe, type = "response")
)

# Compare scaled scorecard probabilities with predict.glm() for all samples.
scorecard_probability_check <- max(
  # Compare training probabilities.
  abs(train_scorecard$fraud_prob - train_model_probability),
  # Compare validation probabilities.
  abs(validation_scorecard$fraud_prob - validation_model_probability),
  # Compare test probabilities.
  abs(test_scorecard$fraud_prob - test_model_probability)
)

# Compare additive scaled points with scaled raw model log odds for all rows.
scorecard_score_check <- max(
  # Calculate absolute differences before finding the maximum.
  abs(
    # Use the score built by adding base points and bin points.
    scorecard_predictions$total_score -
      # Convert raw model log odds with Factor and Offset.
      (score_offset + score_factor * scorecard_predictions$log_odds)
  )
)

# Stop if the scorecard probability calculation is not reproducible.
if (scorecard_probability_check > 1e-10) {
  # A small floating-point difference is acceptable; a large difference is not.
  stop("Scorecard probability does not reproduce the WOE logistic prediction.")
}

# Stop if additive points do not reproduce the scaled model score.
if (scorecard_score_check > 1e-10) {
  # This check protects the teaching scorecard addition formula.
  stop("Additive scorecard points do not reproduce the scaled model score.")
}

# Save the training-only WOE mapping.
write_csv(woe_table, "outputs/L03_woe_table.csv")
# Save the scorecard coefficient table.
write_csv(scorecard_coefficients, "outputs/L03_scorecard_coefficients.csv")
# Save the Factor, Offset, PDO, and base-points parameters.
write_csv(scorecard_parameters, "outputs/L03_scorecard_parameters.csv")
# Save the bin-level scorecard contributions.
write_csv(scorecard_points, "outputs/L03_scorecard_points.csv")
# Save the training scorecard predictions.
write_csv(train_scorecard, "outputs/L03_train_scorecard.csv")
# Save validation scorecard predictions.
write_csv(validation_scorecard, "outputs/L03_validation_scorecard.csv")
# Save test scorecard predictions.
write_csv(test_scorecard, "outputs/L03_test_scorecard.csv")
# Save all train, validation, and test scorecard predictions together.
write_csv(scorecard_predictions, "outputs/L03_scorecard_predictions.csv")

# Print the WOE mapping for classroom inspection.
print(woe_table)
# Print the scorecard coefficients.
print(scorecard_coefficients)
# Print the Factor and Offset parameters.
print(scorecard_parameters)
# Print the bin-level scorecard contributions.
print(scorecard_points)
# Print the first ten combined scorecard predictions.
print(head(scorecard_predictions, 10))


# 8. Compact end-of-exercise summary --------------------------------------

# Create a compact summary of the exercise outputs.
exercise_summary <- data.frame(
  # Record the training sample size.
  train_rows = nrow(train),
  # Record the validation sample size.
  validation_rows = nrow(validation),
  # Record the test sample size.
  test_rows = nrow(test),
  # Record the training fraud rate.
  train_fraud_rate = mean(train$fraud_confirmed),
  # Record the validation fraud rate.
  validation_fraud_rate = mean(validation$fraud_confirmed),
  # Record the test fraud rate.
  test_fraud_rate = mean(test$fraud_confirmed),
  # Name the continuous target used by linear regression.
  continuous_target = "estimated_loss_amount",
  # Count non-intercept coefficients in the linear model.
  linear_model_terms = length(coef(linear_fit)) - 1,
  # Count non-intercept coefficients in the full logistic model.
  logistic_model_terms = length(coef(logistic_fit)) - 1,
  # Count scorecard characteristics.
  scorecard_characteristics = length(scorecard_characteristics),
  # Record the Factor used to scale log odds into points.
  score_factor = score_factor,
  # Record the Offset used to shift the score scale.
  score_offset = score_offset,
  # Record the maximum additive-score check error.
  scorecard_score_check = scorecard_score_check,
  # Store the scorecard probability-reproduction check.
  scorecard_probability_check = scorecard_probability_check
)

# Save the compact exercise summary.
write_csv(exercise_summary, "outputs/L03_exercise_summary.csv")
# Print the compact exercise summary.
print(exercise_summary)
