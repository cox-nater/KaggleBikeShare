library(tidyverse)
library(tidymodels)
library(vroom)

bike_train <- vroom('C:/BYU/2023(5) Fall/STAT 348/KaggleBikeShare/train.csv',
              show_col_types = FALSE)
bike_train <- bike_train %>%
  mutate(weather = replace(weather, weather == 4, 3))

my_recipe <- recipe(y~., bike_train) %>%
  step_time(timestamp, features=c("hour")) %>% #creats new column with hour as veriable
  step_select(datetime, season, holiday, workingday, weather, temp, atemp, humidity, windspeed, count, -casual, -registered)
prepped_recipe <- prep(my_recipe)
bake(prepped_recipe, new_data= bike_train_clean)

  