# Build the survival outcome: time to MACE (days2mace) and event indicator (mace = 1 if MACE occurred, 0 if censored)
y <- Surv(time = train_data$days2mace, event = train_data$mace)

# Predictor matrix: drop the first five columns (ID, outcome, weight and other non-predictor variables)
x <- data

# Observation weights for the MACE outcome (one per row, same order as x and y)
weight <- train_data$w_mace

# Fix the random seed for reproducibility
set.seed(123)

# Define the hyperparameter grid: every combination of learning rate and tree depth (3 x 3 = 9 models)
hyper_grid <- expand.grid(
  shrinkage = c(.001, .01, .05),   # learning rate: contribution of each tree to the ensemble
  interaction.depth = c(1, 2, 3),  # maximum depth of each tree (1 = main effects only, 2-3 = interactions)
  optimal_trees = 0,               # placeholder: number of trees minimising the validation error
  min_RMSE = 0                     # placeholder: minimum validation error (for coxph this is the partial-likelihood deviance, not an RMSE)
)

# Reset the seed before the tuning loop
set.seed(123)

# Loop over each row of the grid, i.e. each hyperparameter combination
for (i in 1:nrow(hyper_grid)) {
  
  # Fit a gradient boosting Cox model with the i-th combination of hyperparameters
  gbm.tune <- gbm(
    formula = y ~ .,                                      # survival outcome regressed on all columns of x
    data = x,                                             # predictor data
    distribution = "coxph",                               # Cox proportional hazards loss (partial likelihood)
    n.trees = 1000,                                       # maximum number of boosting iterations
    interaction.depth = hyper_grid$interaction.depth[i],  # tree depth for this combination
    shrinkage = hyper_grid$shrinkage[i],                  # learning rate for this combination
    n.cores = NULL,                                       # number of cores (only used with cv.folds > 1)
    weights = weight,                                     # observation weights
    train.fraction = .75,                                 # first 75% of rows used for training, last 25% for validation
    verbose = FALSE                                       # suppress iteration output
  )
  
  # Store the number of trees at which the validation error is minimal
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  
  # Store the minimum validation error reached by this combination
  hyper_grid$min_RMSE[i] <- min(gbm.tune$valid.error)
}

# Load dplyr for data manipulation
library(dplyr)

# Sort the grid from lowest to highest validation error and show the top 10 combinations
hyper_grid %>%
  dplyr::arrange(min_RMSE) %>%
  head(10)

# Reset the seed before fitting the final model
set.seed(123)

# Fit the final model on the full training data with the selected hyperparameters
gbm1 <- gbm(
  formula = y ~ .,          # same outcome and predictors as in tuning
  data = x,                 # full predictor data
  distribution = "coxph",   # Cox proportional hazards loss
  n.trees = 1000,           # number of trees
  interaction.depth = 3,    # selected tree depth
  shrinkage = 0.001,        # selected learning rate
  train.fraction = 1,       # use all rows for training (no validation split)
  n.cores = NULL,           # number of cores
  verbose = FALSE,          # suppress iteration output
  weights = weight          # observation weights
)

# Load vip for variable importance plots
library(vip)

# Compute and display the variable importance plot of the final model
a <- vip(gbm1); a