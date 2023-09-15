library(tidyverse)
library(tidymodels)
library(vroom)

bike_train <- vroom('C:/BYU/2023(5) Fall/STAT 348/KaggleBikeShare/train.csv',
              show_col_types = FALSE) %>%
  select(datetime, season, holiday, workingday, weather, temp, atemp, humidity, windspeed, count)
bike_test <- vroom('C:/BYU/2023(5) Fall/STAT 348/KaggleBikeShare/test.csv',
                    show_col_types = FALSE)

bike_train <- bike_train %>%
  mutate(weather = replace(weather, weather == 4, 3))

my_recipe <- recipe(count~., bike_train) %>%
  step_time(datetime, features=c("hour")) %>% #creates new column with hour as variable
  step_rename(hour = datetime_hour)
prepped_recipe <- prep(my_recipe)
bake(prepped_recipe, new_data= bike_train)


###Linear Regression###

my_mod <- linear_reg() %>% # type of model
  set_engine("lm") # Engine = what r function to use

bike_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_mod) %>%
  fit(data = bike_train) #fit the workflow

bike_pred <- predict(bike_workflow, new_data=bike_test)# use to fit the prediction
bike_pred[bike_pred < 0] <- 0 
bike_pred<-cbind(bike_test$datetime,bike_pred) %>%
  rename(datetime = 'bike_test$datetime', count = '.pred')
vroom_write(bike_pred, "bike_pred.csv")

