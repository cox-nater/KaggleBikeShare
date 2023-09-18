library(tidyverse)
library(tidymodels)
library(vroom)

column_types <- cols(
  datetime = col_character(),
  season = col_double(),
  holiday = col_double(),
  workingday = col_double(),
  weather = col_double(),
  temp = col_double(),
  atemp = col_double(),
  humidity = col_double(),
  windspeed = col_double(),
  count = col_double()
)

bike_train <- vroom('C:/BYU/2023(5) Fall/STAT 348/KaggleBikeShare/train.csv',
              show_col_types = FALSE) %>%
  select(datetime, season, holiday, workingday, weather, temp, atemp, humidity, windspeed, count)
bike_test1 <- read_csv('C:/BYU/2023(5) Fall/STAT 348/KaggleBikeShare/test.csv', col_types = column_types)

bike_test <-vroom('C:/BYU/2023(5) Fall/STAT 348/KaggleBikeShare/test.csv',
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
bike_pred<-cbind(bike_test1$datetime, bike_pred) %>%
  rename(datetime = 'bike_test1$datetime', count = '.pred')

vroom_write(bike_pred, "bike_pred.csv", delim = ",")
###Poisson Regression###

library(poissonreg)
pois_mod <- poisson_reg() %>%
  set_engine("glm")

bike_pois_workflow <- workflow()%>%
  add_recipe(my_recipe) %>%
  add_model(pois_mod) %>%
  fit(data = bike_train)

bike_predictions <- predict(bike_pois_workflow, new_data=bike_test) %>%
  bind_cols(., bike_test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and predictions
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write prediction file to CSV
vroom_write(x=bike_predictions, file="./TestPreds.csv", delim=",")
