library(tidyverse)
library(vroom)
library(patchwork)
library(tidymodels)

train_bike <- vroom("train.csv")

casual_v_registered <- ggplot(data=bikeshare, aes(x = casual, y = registered)) +
  geom_point() + geom_smooth(se = FALSE)

bar_weather <- ggplot(data = bikeshare, aes(x = weather, fill = factor(weather))) +
  geom_bar()

season_boxplot <- ggplot(data = bikeshare, aes(x = count, 
                                               y = humidity, 
                                               group = season,
                                               fill = factor(season))) +
  geom_boxplot()

temp_v_count <- ggplot(data=bikeshare, aes(x = temp, y = count)) +
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
  step_date(datetime, features = "dow")


# LINEAR REGRESSION WORKFLOW

my_linear_model <- linear_reg() %>% 
  set_engine("lm") %>% 
  set_mode("regression")

bike_workflow <- workflow() %>% 
  add_recipe(bike_recipe) %>%
  add_model(my_linear_model) %>%
  fit(data = train_bike)

test_bike <- vroom("test.csv")
predictions <- exp(predict(bike_workflow, new_data = test_bike))

# predictions <- predict(my_linear_model, new_data = test_bike)

output <- test_bike %>% 
  select(datetime) %>%
  bind_cols(predictions) %>%
  rename(count = .pred) %>%
  mutate(datetime = as.character(format(datetime)))

# output$count <- ifelse(output$count < 0, 0, output$count)

vroom_write(output, "bike_predictions2.csv", delim = ',')


