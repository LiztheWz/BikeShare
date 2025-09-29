library(tidyverse)
library(vroom)
library(patchwork)
library(tidymodels)
library(glmnet)
library(bonsai)
library(lightgbm)
library(agua)

train_bike <- vroom("train.csv")

# COMMON EDA
# ==============================================================
# casual_v_registered <- ggplot(data=train_bike, aes(x = casual, y = registered)) +
#   geom_point() + geom_smooth(se = FALSE)
# 
# bar_weather <- ggplot(data = train_bike, aes(x = weather, fill = factor(weather))) +
#   geom_bar()
# 
# season_boxplot <- ggplot(data = train_bike, aes(x = count, 
#                                                y = humidity, 
#                                                group = season,
#                                                fill = factor(season))) +
#   geom_boxplot()
# 
# temp_v_count <- ggplot(data=train_bike, aes(x = temp, y = count)) +
#   geom_point() + geom_smooth(se = FALSE)
# 
# (casual_v_registered + bar_weather) / (season_boxplot + temp_v_count)




# CLEANING / FEATURE ENGINEERING
# ==============================================================
train_bike <- train_bike[, -c(10,11)]
train_bike$count <- log(train_bike$count)

bike_recipe <- recipe(count ~., data = train_bike) %>% 
  step_mutate(weather = ifelse(weather == 4, 3, weather)) %>%
  step_mutate(weather = factor(weather)) %>%
  step_time(datetime, features = "hour") %>%
  step_mutate(season = factor(season)) %>%
  step_date(datetime, features = "dow") %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  update_role(datetime, new_role = "ID")


# LINEAR REGRESSION WORKFLOW
# ==============================================================
# my_linear_model <- linear_reg() %>%
#   set_engine("lm") %>%
#   set_mode("regression")
# 
# bike_workflow <- workflow() %>%
#   add_recipe(bike_recipe) %>%
#   add_model(my_linear_model) %>%
#   fit(data = train_bike)

# test_bike <- vroom("test.csv")
# 
# predictions <- bike_workflow %>% 
#   predict(new_data = test_bike) %>%
#   mutate(count = exp(.pred)) %>%
#   bind_cols(test_bike %>% select(datetime)) %>%
#   select(datetime, count) %>%
#   mutate(datetime = format(datetime, "%Y-%m-%d %H:%M:%S"))
# 
# 
# vroom_write(predictions, "linear_regression_predictions.csv", delim = ',')


# REGRESSION TREES
# ==============================================================
# tree_mod <- decision_tree(tree_depth = tune(),
#                         cost_complexity = tune(),
#                         min_n = tune()) %>%
#   set_engine("rpart") %>% set_mode("regression")
# test_bike <- vroom("test.csv")



# PENALIZED REGRESSION
# ==============================================================

# GUESSING PENALTY AND MIXTURE
# COMBO 1 =====================
# preg_model <- linear_reg(penalty = 1, mixture = 0.99) %>%
#   set_engine("glmnet")

# COMBO 2 =====================
# preg_model <- linear_reg(penalty = 0.0001, mixture = 0.5) %>%
#   set_engine("glmnet")

# COMBO 3 =====================
# preg_model <- linear_reg(penalty = 0.000001, mixture = 0.1) %>%
#   set_engine("glmnet")

# COMBO 4 =====================
# preg_model <- linear_reg(penalty = 1e-50, mixture = 1e-50) %>%
#   set_engine("glmnet")

# COMBO 5 =====================
# preg_model <- linear_reg(penalty = 1e-9999, mixture = 1e-999) %>%
#   set_engine("glmnet")

# preg_workflow <- workflow() %>% 
#   add_recipe(bike_recipe) %>%
#   add_model(preg_model) %>%
#   fit(data = train_bike)

# preg_wf <- workflow() %>%
#  add_recipe(bike_recipe) %>%
#  add_model(tree_mod)

#grid_of_tuning_params <- grid_regular(tree_depth(),
#                                       cost_complexity(),
#                                       min_n(),
#                                       levels = 5)

# test_bike <- vroom("test.csv")
# 
# predictions <- preg_wf %>% 
#   predict(new_data = test_bike) %>%
#   mutate(count = exp(.pred)) %>%
#   bind_cols(test_bike %>% select(datetime)) %>%
#   select(datetime, count) %>%
#   mutate(datetime = format(datetime, "%Y-%m-%d %H:%M:%S"))
# 
# 
# vroom_write(predictions, "penalized_regression_predictions.csv", delim = ',')




# TUNING PARAMETERS
# ==============================================================
#tune_model <- linear_reg(penalty = tune(),
#                         mixture = tune()) %>%
#  set_engine("glmnet")

#grid_of_tuning_params <- grid_regular(penalty(),
#                                      mixture(),
#                                      levels = 5)


# folds <- vfold_cv(train_bike, v = 10, repeats = 1)
# 
# CV_results <- preg_wf %>%
#   tune_grid(resamples = folds,
#             grid = grid_of_tuning_params,
#             metrics = metric_set(rmse, mae))
#   
# bestTune <- CV_results %>%
#   select_best(metric = "rmse")
# 
# final_wf <- preg_wf %>%
#   finalize_workflow(bestTune) %>%
#   fit(data = train_bike)

# test_bike <- vroom("test.csv")
# 
# predictions <- final_wf %>% 
#   predict(new_data = test_bike) %>%
#   mutate(count = exp(.pred)) %>%
#   bind_cols(test_bike %>% select(datetime)) %>%
#   select(datetime, count) %>%
#   mutate(datetime = format(datetime, "%Y-%m-%d %H:%M:%S"))
# 
# 
# vroom_write(predictions, "tuning_models_predictions.csv", delim = ',')



# RANDOM FORESTS
# ==============================================================
# forest_mod <- rand_forest(mtry = tune(),
#                           min_n = tune(),
#                           trees = 500) %>%
#   set_engine("ranger") %>% set_mode("regression")
# 
# 
# forest_wf <- workflow() %>%
#   add_recipe(bike_recipe) %>%
#   add_model(forest_mod)
# 
# 
# forest_grid <- grid_regular(mtry(range = c(1,9)),
#                             min_n(),
#                             levels = 5)
# 
# folds <- vfold_cv(train_bike, v = 10, repeats = 1)
# 
# CV_results <- forest_wf %>%
#   tune_grid(resamples = folds,
#             grid = forest_grid,
#             metrics = metric_set(rmse, mae))
# 
# bestTune <- CV_results %>%
#   select_best(metric = "rmse")
# 
# final_wf <- forest_wf %>%
#   finalize_workflow(bestTune) %>%
#   fit(data = train_bike)
# 
# test_bike <- vroom("test.csv")
# 
# predictions <- final_wf %>% 
#   predict(new_data = test_bike) %>%
#   mutate(count = exp(.pred)) %>%
#   bind_cols(test_bike %>% select(datetime)) %>%
#   select(datetime, count) %>%
#   mutate(datetime = format(datetime, "%Y-%m-%d %H:%M:%S"))
# 
# 
# vroom_write(predictions, "random_forests_predictions.csv", delim = ',')




# BOOSTED TREES & BART
# ==============================================================
# bart_model <- bart(trees = tune()) %>%
#   set_engine("dbarts") %>%
#   set_mode("regression")
# 
# bart_wf <- workflow() %>%
#   add_recipe(bike_recipe) %>%
#   add_model(bart_model)
# 
# 
# bart_grid <- grid_regular(trees(),
#                             levels = 2)
# 
# folds <- vfold_cv(train_bike, v = 10, repeats = 1)
# 
# CV_results <- bart_wf %>%
#   tune_grid(resamples = folds,
#             grid = bart_grid,
#             metrics = metric_set(rmse, mae))
# 
# bestTune <- CV_results %>%
#   select_best(metric = "rmse")
# 
# final_wf <- bart_wf %>%
#   finalize_workflow(bestTune) %>%
#   fit(data = train_bike)

# test_bike <- vroom("test.csv")
# 
# predictions <- final_wf %>% 
#   predict(new_data = test_bike) %>%
#   mutate(count = exp(.pred)) %>%
#   bind_cols(test_bike %>% select(datetime)) %>%
#   select(datetime, count) %>%
#   mutate(datetime = format(datetime, "%Y-%m-%d %H:%M:%S"))
# 
# 
# vroom_write(predictions, "bart_predictions.csv", delim = ',')


# STACKING MODELS
# ==============================================================
h2o::h2o.init()

auto_model <- auto_ml() %>%
  set_engine("h2o", max_runtime_secs = 120, max_models = 5) %>%
  set_mode("regression")

automl_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(auto_model) %>%
  fit(data = train_bike)

test_bike <- vroom("test.csv")

predictions <- automl_wf %>% 
  predict(new_data = test_bike) %>%
  mutate(count = exp(.pred)) %>%
  bind_cols(test_bike %>% select(datetime)) %>%
  select(datetime, count) %>%
  mutate(datetime = format(datetime, "%Y-%m-%d %H:%M:%S"))


vroom_write(predictions, "stacking_predictions.csv", delim = ',')
