library(tidyverse)
library(vroom)
library(patchwork)
library(tidymodels)
library(glmnet)

train_bike <- vroom("train.csv")


# COMMON EDA
casual_v_registered <- ggplot(data=train_bike, aes(x = casual, y = registered)) +
  geom_point() + geom_smooth(se = FALSE)

bar_weather <- ggplot(data = train_bike, aes(x = weather, fill = factor(weather))) +
  geom_bar()

season_boxplot <- ggplot(data = train_bike, aes(x = count, 
                                               y = humidity, 
                                               group = season,
                                               fill = factor(season))) +
  geom_boxplot()

temp_v_count <- ggplot(data=train_bike, aes(x = temp, y = count)) +
  geom_point() + geom_smooth(se = FALSE)

(casual_v_registered + bar_weather) / (season_boxplot + temp_v_count)



# CLEANING / FEATURE ENGINEERING
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


# PENALIZED REGRESSION

# GUESSING PENALTY AND MIXTURE
# COMBO 1
preg_model <- linear_reg(penalty = 1, mixture = 0.99) %>%
  set_engine("glmnet")

# COMBO 2
preg_model <- linear_reg(penalty = 0.0001, mixture = 0.5) %>%
  set_engine("glmnet")

# COMBO 3
preg_model <- linear_reg(penalty = 0.000001, mixture = 0.1) %>%
  set_engine("glmnet")

# COMBO 4
preg_model <- linear_reg(penalty = 1e-50, mixture = 1e-50) %>%
  set_engine("glmnet")

# COMBO 5
preg_model <- linear_reg(penalty = 1e-9999, mixture = 1e-999) %>%
  set_engine("glmnet")

preg_workflow <- workflow() %>% 
  add_recipe(bike_recipe) %>%
  add_model(preg_model) %>%
  fit(data = train_bike)




# TUNING PARAMETERS

tune_model <- linear_reg(penalty = tune(),
                         mixture = tune()) %>%
  set_engine("glmnet")

preg_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(tune_model)

grid_of_tuning_params <- grid_regular(penalty(),
                                      mixture(),
                                      levels = 20)

folds <- vfold_cv(train_bike, v = 10, repeats = 5)

CV_results <- preg_wf %>%
  tune_grid(resamples = folds,
            grid = grid_of_tuning_params,
            metrics = metric_set(rmse, mae))

#collect_metrics(CV_results) %>%
#  filter(.metric == "rmse") %>%
#  ggplot(data = ., aes(x = penalty, y = mean, color = factor(mixture))) +
#  geom_line()
  
bestTune <- CV_results %>%
  select_best(metric = "rmse")

final_wf <- preg_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data = train_bike)





# LINEAR REGRESSION WORKFLOW

my_linear_model <- linear_reg() %>% 
  set_engine("lm") %>% 
  set_mode("regression")

bike_workflow <- workflow() %>% 
  add_recipe(bike_recipe) %>%
  add_model(my_linear_model) %>%
  fit(data = train_bike)




# PREDICTIONS
test_bike <- vroom("test.csv")

predictions <- final_wf %>% 
  predict(new_data = test_bike) %>%
  mutate(count = exp(.pred)) %>%
  bind_cols(test_bike %>% select(datetime)) %>%
  select(datetime, count) %>%
  mutate(datetime = format(datetime, "%Y-%m-%d %H:%M:%S"))
  

vroom_write(predictions, "bike_predictions4.csv", delim = ',')


