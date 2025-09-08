library(tidyverse)
library(vroom)
library(patchwork)

bikeshare <- vroom("train.csv")

dplyr::glimpse(bikeshare)
skimr::skim(bikeshare)
DataExplorer::plot_intro(bikeshare)
DataExplorer::plot_correlation(bikeshare)
DataExplorer::plot_bar(bikeshare)
DataExplorer::plot_histogram(bikeshare)


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

