# A complete workflow for transaction fraud data analysis preprocessing and PCA dimensionality reduction -----

library(tidyverse)
# including dplyr\readr\ggplot2\forcats\tibble for input Cleaning Transformat Visualised.
library(lubridate)
# Used for parsing and calculating dates and times, for exmaple:as_datatime()\as.Date().
set.seed(741002)
# Make sure that any random operations later on can be repeated.
# Create an output folder if it does not already exist.
dir.create("outputs", showWarnings = FALSE, recursive = TRUE)


# 1. Import the two supplied synthetic source tables ---------------------

transactions <- read_csv(
  "L02_transactions.csv",
  show_col_types = FALSE
) 
# Not display the informations of column

customer_accounts <- read_csv(
  "L02_customer_accounts.csv",
  show_col_types = FALSE
)


# 2. Inspect variable types, missing values, and the label --------------

transactions <- mutate( 
  transactions,
  event_time = as_datetime(event_time, tz = "UTC"), # Store event time as UTC date-time.
  account_id = as.character(account_id), # Treat account_id as an identifier, not a number.
  case_outcome = factor( # Treat the outcome as a categorical variable.
    case_outcome, # Use the original case-outcome values.
    levels = c("legitimate", "confirmed_fraud", "pending") # Set the label order.
  )
)

glimpse(transactions) # Usage: glimpse(x,width = Null, ...) check the structure.
glimpse(customer_accounts) # Check the structure.

# Count missing values in each transaction column.
transaction_missing <- colSums(is.na(transactions)) # is.na() usage: check the NA in dataframe or vector.
transaction_missing

# Count missing values in each account column.
customer_missing <- colSums(is.na(customer_accounts))
customer_missing 

# Count observations in each case-outcome category.
case_counts <- count(transactions, case_outcome) 
case_counts # Show the sample size for each case outcome category

# 3. Check keys, duplicate data, missing values, and invalid values -------

# Summarise the account key and the income field.
customer_key_check <- summarise(
  customer_accounts,
  rows = n(), # Count all account rows.
  unique_account_keys = n_distinct(account_id), # Count distinct account IDs.
  missing_income = sum(is.na(declared_income_hkd)), # Count missing income values.
  duplicate_account_keys = n() - n_distinct(account_id) # Count repeated account IDs.
)

glimpse(customer_key_check)

# Count the number of rows for each transaction ID.
duplicate_transactions <- count( # Create a transaction-ID frequency table.
  transactions,
  transaction_id, # Count rows for each transaction ID.
  name = "rows_with_same_key" # Name the count column clearly.
)

# Keep only transaction IDs that occur more than once.
duplicate_transactions <- filter(
  duplicate_transactions, # Use the frequency table created above.
  rows_with_same_key > 1 # A count above one indicates repeated data.
)

# Display the duplicate transaction IDs.
duplicate_transactions # Inspect the repeated transaction key.

# Find non-positive transaction amounts.
invalid_amounts <- filter(
  transactions,
  !is.na(amount_hkd), # Ignore amounts that are already missing.
  amount_hkd <= 0 # Flag zero or negative amounts.
)

# Display invalid transaction amounts.
invalid_amounts # Decide how these values should be treated before analysis.


# 4. Clean the transaction table and add account context ----------------

# Keep the first row for each transaction ID in the current row order.
transactions_clean <- distinct( # Keep the frist row.
  transactions,
  transaction_id, # Use transaction_id as the key.
  .keep_all = TRUE # Keep all other columns from the retained row.
)

# Replace non-positive amounts with numeric missing values.
transactions_clean <- mutate(
  transactions_clean, 
  amount_hkd = if_else( # Apply a condition to every amount.
    amount_hkd <= 0, # Test whether the amount is zero or negative.
    NA_real_, # Replace an invalid numeric amount with NA.
    amount_hkd # Keep a valid amount unchanged.
  )
)

# Define the boundaries for the amount categories.（定义金额分项边界：负无穷、100、500、2000、正无穷）
amount_breaks <- c(-Inf, 100, 500, 2000, Inf)

# Define readable labels for the amount categories. 
amount_labels <- c(
  "below_100", 
  "100_to_500", 
  "500_to_2000", 
  "above_2000"
) 

# Create a categorical amount variable.
transactions_clean <- mutate(
  transactions_clean,
  amount_band = cut( # Convert a numeric amount into categories.
    amount_hkd, # Categorise the cleaned amount.
    breaks = amount_breaks, # Use the boundaries defined above.
    labels = amount_labels, # Use the readable labels defined above.
    right = FALSE # Include the left boundary and exclude the right boundary. example: [100, 500)
  )
)

# Attach one row of account context to each transaction.
merged_transactions <- left_join(
  transactions_clean, # Keep the transaction table as the left table.
  customer_accounts, # Add columns from the account table.
  by = "account_id" # Match rows using account_id.
)

# Calculate account age at the end of the period.
merged_transactions <- mutate(
  merged_transactions,
  account_open_date = as.Date(account_open_date), # Convert the opening date to Date.
  account_age_days = as.numeric( # Calculate account age in days.
    as.Date("2025-04-30") - account_open_date
  )
)


# 5. Validate the merge result -------------------------------------------

rows_before_merge <- nrow(transactions_clean)
rows_after_merge <- nrow(merged_transactions)

# Count repeated transaction IDs after duplicate removal.
duplicate_transaction_rows <- sum(duplicated(transactions_clean$transaction_id))

# Count transactions with no matched account country.
unmatched_account_keys <- sum(is.na(merged_transactions$account_country))

rows_before_merge
rows_after_merge
duplicate_transaction_rows
unmatched_account_keys

# Combine the merge checks into one table.
merge_check <- tibble( # Create a compact merge-validation table.
  transaction_rows_before_merge = rows_before_merge,
  transaction_rows_after_merge = rows_after_merge,
  duplicate_transaction_rows = duplicate_transaction_rows,
  unique_transaction_keys_after_merge = n_distinct(merged_transactions$transaction_id), 
  unmatched_account_keys = unmatched_account_keys, 
  customer_key_duplicates = nrow(customer_accounts) - n_distinct(customer_accounts$account_id) 
) 

# Display the merge-validation table.
glimpse(merge_check) # Confirm that the table still has one row per transaction.


# 6. Create a stratified sample ----------------------------

set.seed(741002)

# Create groups by case outcome before sampling.
sample_groups <- group_by( # Prepare a stratified sampling operation.
  merged_transactions, 
  case_outcome # Create one sampling group for each case outcome.
)

# Select 80% of rows within every case-outcome group.
analysis_sample <- slice_sample( 
  sample_groups, 
  prop = 0.80 # Keep 80% of each case-outcome group.
)

# Remove the grouping after sampling.
analysis_sample <- ungroup(analysis_sample) # Return to an ordinary tibble.

# Sort the sample in chronological order.
analysis_sample <- arrange(analysis_sample, event_time) # Order rows by event time.

# Create numeric transformations and retain the categorical amount band.
analysis_sample <- mutate(
  analysis_sample,
  amount_hkd_log = log1p(amount_hkd), # Reduce the effect of large amounts.
  amount_z = as.numeric(scale(amount_hkd)), # Standardise amount_hkd using a z-score.
  amount_band = fct_na_value_to_level( # Give missing bands an explicit category.
    amount_band, # Use the existing amount-band variable.
    level = "missing_amount" # Name the category for missing amounts.
  )
)
#amount_hkd_log=log1p(amount_hkd):对金额做log（1+x)变换，缓解大额值得影响
#amount_z = as.numeric(scale(amount_hkd)):对金额做标准化，得到z分数
#amount_band = fect_na_value_to_level(...):把amount_band中的NA转成显式因子水平 "missing_amount"

# 7. Prepare the input and run Principal Components Analysis -------------

# Select only numeric explanatory variables for PCA.
pca_variables <- select( # Create the PCA input table.
  analysis_sample,
  amount_hkd_log, # Include the log-transformed amount.
  velocity_24h, 
  amount_vs_account_median, # Include amount relative to the account median.
  geo_distance_km, 
  failed_login_count_24h, 
  beneficiary_age_days,
  account_age_days 
) 

# Replace missing PCA values with the median of their own variable.
pca_variables <- mutate( 
  pca_variables,
  across( # Apply the same operation to every PCA column.
    everything(), # Select all columns in pca_variables.
    ~ replace_na(.x, median(.x, na.rm = TRUE)) # Replace missing values with the column median.
  ) 
)

# Fit Principal Components Analysis to the prepared numeric variables.
pca_fit <- prcomp( # Calculate principal components.
  pca_variables, # Use the imputed PCA input table.
  center = TRUE, # Centre each variable around its mean.
  scale. = TRUE # Put variables on a comparable standardised scale (z-score).
)


# 8. Read explained variance, loadings, and component scores -------------

# Square the standard deviations to obtain component eigenvalues.
eigenvalues <- pca_fit$sdev ^ 2

# Calculate the total variance across all components.
total_variance <- sum(eigenvalues)

# Calculate each component's proportion of variance.
proportion_variance <- eigenvalues / total_variance

# Calculate cumulative explained variance.
cumulative_variance <- cumsum(proportion_variance)

# Store the explained-variance results in a table.
explained_variance <- tibble(
  component = paste0("PC", seq_along(eigenvalues)), # Name components PC1, PC2, and so on.
  eigenvalue = eigenvalues,
  proportion_of_variance = proportion_variance,
  cumulative_proportion = cumulative_variance
)

# Convert the PCA rotation matrix into a tibble of loadings.
loadings <- as_tibble( # Convert the matrix to a tidy table.
  pca_fit$rotation, # Use the PCA rotation matrix.
  rownames = "variable" 
)

# Keep identifiers that will be attached to the PCA scores.
pca_identifiers <- select( # Create an identifier table for the observations.
  analysis_sample, 
  transaction_id, # Keep the transaction ID.
  case_outcome # Keep the outcome for description only.
)

# Convert the PCA score matrix to a tibble.
pca_scores_only <- as_tibble(pca_fit$x)

# Attach transaction identifiers and case outcomes to the scores.
component_scores <- bind_cols( # Combine identifiers with transformed scores.
  pca_identifiers, # Add the transaction ID and case outcome.
  pca_scores_only # Add PC1, PC2, and the remaining component scores.
)

# Display the explained-variance table.
explained_variance

# Display the first loading rows.
head(loadings)

# Display the first component scores.
head(component_scores)

# Add the component scores back to the sampled analysis table.
analysis_with_scores <- left_join( # Create a table containing original fields and PC scores.
  analysis_sample, # Keep the original sampled transaction rows.
  component_scores, # Add the component-score columns.
  by = c("transaction_id", "case_outcome") # Match each score to its transaction.
) 

