# Load necessary libraries
my_packs <- c("tidyverse", "tidymodels", "glmnet",
              "discrim", "rsample", "yardstick", 
              "parsnip", "parallel", "doFuture")

for(pack in my_packs) {
  library(pack, character.only = TRUE)
}

# Importing the data 
tourney_year <- 2024
n_params <- 30
prop_train <- 0.80
num_cores <- 5
order <- "second"  

parallel::detectCores(logical = FALSE)

# Fitting models ----------------------------------------------------------

for(womens in c(TRUE, FALSE)) {
design_response_file <- paste0(tourney_year, "/design_response_", tourney_year, "_", ifelse(womens, "W", "M"), ".rds")
design_response <- read_rds(design_response_file)

model_type <- "classification"

# Modeling data
set.seed(1994)
model_data <- bind_cols(tibble(outcome = factor(design_response[[paste0("designMatFit", ifelse(order == "first", "", "2"))]][[ifelse(model_type == "classification",
                                                                                     "response_Awin",
                                                                                     "response_Amov")]],
                                                levels = c("TRUE", "FALSE"))),
                        design_response[[paste0("designMatFit", ifelse(order == "first", "", "2"))]]$design)

# Splitting into training and test sets
data_split <- initial_split(model_data, prop = prop_train)
data_train <- training(data_split)
data_test  <-  testing(data_split)

train_folds <- vfold_cv(data_train, v = 10)

# Available parsnip models: https://www.tidymodels.org/find/parsnip/

# Define the lasso-penalized logistic regression model
lasso_mod <- logistic_reg(penalty = tune(), mixture = 1) |>
  set_engine("glmnet")

# Define Linear Discriminant Analysis (LDA) model
lda_mod <- discrim_linear() |>
  set_engine("MASS")

# Define single-layer neural network
nnet_mod <- mlp(
  mode = model_type,
  hidden_units = tune(),
  penalty = tune(),
  epochs = tune()
) |>
set_engine("nnet") |> 
  set_mode(model_type)

# Making basic first-order recipe
order1_recipe <- recipe(outcome ~ ., data = data_train)

# Only logistic model if second-order
if(order == "second") {
  # Making workflow
  mods_list <- list(lasso = lasso_mod) 
} else {
  # Making workflow
  mods_list <- list(lasso = lasso_mod, 
                    lda = lda_mod,
                    nnet = nnet_mod)
}

model_workflow <- workflow_set(models = mods_list,
                               preproc = list("formula" = outcome ~ .))

# Specify tuning parameter grids
lasso_grid <- tibble(penalty = c(0, 10^seq(-4, -1, length.out = n_params)))

# xgb_grid <- grid_latin_hypercube(
#   tree_depth(),
#   min_n(),
#   loss_reduction(),
#   sample_size = sample_prop(),
#   finalize(mtry(), data_train),
#   learn_rate(),
#   size = n_params
# )

nnet_grid <- grid_latin_hypercube(
  hidden_units(range = c(1L, 5L)),
  penalty(range = c(-10, 0), trans = log10_trans()),
  epochs(range = c(1000L, 20000L)),
  size = n_params
)

# Run the cross-validation for both models
eval_metrics <- metric_set(accuracy, roc_auc)
final_eval_metric <- "accuracy"

models_file <- paste0(tourney_year, "/workflow_set_", tourney_year, 
                      "_order", ifelse(order == "first", "1", "2"),
                      "_nparams", n_params, "_", 
                      "proptrain", prop_train, "_",
                      ifelse(womens, "W", "M"), ".rds")

if(file.exists(models_file) == FALSE) {
  
  registerDoFuture()
  cl <- makeCluster(num_cores)
  plan(cluster, workers = cl)
  
  control_grid(allow_par = TRUE,
               parallel_over = "resamples",
               pkgs = my_packs)
  
  full_workflow <- model_workflow |>
  option_add(grid = lasso_grid, id = "formula_lasso")

# Add other models besides logistic regression if first-order
if(order == "first") {
  full_workflow <- full_workflow |> 
    option_add(grid = nnet_grid, id = "formula_nnet")
}

overall_res <- full_workflow |>
  workflow_map("tune_grid", resamples = train_folds, 
               metrics = eval_metrics, verbose = TRUE)

# Saving model fits
write_rds(overall_res, file = models_file)
} else {
  overall_res <- read_rds(models_file)
}

# Visualizing performance across tuning parameters
# Code from https://juliasilge.com/blog/xgboost-tune-volleyball/
autoplot(overall_res, metric = "accuracy", id = "formula_lasso") +
  ggthemes::theme_few()

# Function to calculate prediction accuracy on test set
calculate_test_accuracy <- function(model_id, wf_set = overall_res, train_data = data_train, test_data = data_test) {
  
  # Get the best model from the grid
  best_mod <- wf_set |> 
    extract_workflow_set_result(id = model_id) |> 
    select_best(final_eval_metric)
  
  # Update the workflow for the optimal parameters
  final_wf <- model_workflow |> 
    extract_workflow(id = model_id) |> 
    finalize_workflow(best_mod)
  
  # Fit the final model
  final_mod <- fit(final_wf, data = train_data) |> 
    extract_fit_parsnip()
  
  # Predictions on test set
  ret <- predict(final_mod, new_data = test_data |> 
                   dplyr::select(-outcome)) |>
    bind_cols(test_data) |> 
    metrics(truth = outcome, estimate = .pred_class) |> 
    dplyr::mutate(model = model_id)
  
  return(ret)
}

if(order == "first") {
# Visualizing performances of models
autoplot(overall_res) +
  guides(pch = "none") +
  ggthemes::theme_few() +
  theme(legend.position = "bottom")

autoplot(overall_res, metric = "accuracy", id = "formula_nnet") +
  ggthemes::theme_few()

test_result <- map_dfr(.x = overall_res$wflow_id, .f = calculate_test_accuracy) |> 
  dplyr::arrange(.metric, desc(.estimate))

test_result
} else {
  calculate_test_accuracy(model_id = "formula_lasso")
}
}

# Final predictions -------------------------------------------------------

library(tidyverse)

# Importing sample submission file

sample_submission <- read_csv(paste0(tourney_year, "/march-machine-learning-mania-", tourney_year, "/sample_submission.csv"))

sample_preds_old <- read_csv("2023/march-machine-learning-mania-2023/SampleSubmission2023.csv")

sample_seeds <- read_csv(paste0(tourney_year, "/march-machine-learning-mania-", tourney_year, "/", tourney_year, "_tourney_seeds.csv"))

sample_preds_new <- map_dfr(.x = c("M", "W"), .f = function(tournament, seeds = sample_seeds, trny_year = tourney_year) {
  seeds <- seeds |> 
    dplyr::filter(Tournament == tournament)
  
  ret <- expand_grid(team1 = seeds$TeamID,
                                  team2 = seeds$TeamID) |> 
    dplyr::filter(team1 < team2) |> 
    dplyr::mutate(Tournament = tournament,
                  ID = str_c(trny_year, "_", team1, "_", team2),
                  Pred = 0.5) |> 
    dplyr::select(Tournament, ID, Pred)
  
  return(ret)
})


