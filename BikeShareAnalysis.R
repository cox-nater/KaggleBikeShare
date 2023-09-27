library(tidyverse)
library(tidymodels)
library(vroom)

bike_train <- vroom('C:/BYU/2023(5) Fall/STAT 348/KaggleBikeShare/train.csv',
              show_col_types = FALSE) %>%
  select(datetime, season, weather, temp, humidity, windspeed, count) %>%
  mutate(count= log(count))

bike_test <-vroom('C:/BYU/2023(5) Fall/STAT 348/KaggleBikeShare/test.csv',
                   show_col_types = FALSE)
bike_train <- bike_train %>%
  mutate(weather = replace(weather, weather == 4, 3))

my_recipe <- recipe(count~., bike_train) %>%
  step_time(datetime, features=c("hour"), keep_original_cols = F) %>% #creates new column with hour as variable
  step_dummy(all_nominal_predictors())%>%
  step_normalize(all_numeric_predictors())
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

#vroom_write(bike_pred, "bike_pred.csv", delim = ",")
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
#vroom_write(x=bike_predictions, file="./TestPreds.csv", delim=",")


###Penalized###
my_recipe <- recipe(count~., bike_train) %>%
  step_time(datetime, features=c("hour"), keep_original_cols = F) %>% 
  step_mutate(hour = as.factor(datetime_hour))%>%
  step_dummy(all_nominal_predictors())%>%
  step_normalize(all_numeric_predictors())
prepped_recipe <- prep(my_recipe)
bake(prepped_recipe, new_data= bike_train)

preg_model <- linear_reg(penalty=0, mixture=tune.25) %>%
  set_engine("glmnet")
preg_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(preg_model) %>%
  fit(data=bike_train)

preg_modelv <- linear_reg(penalty=tune(), mixture=tune()) %>%
  set_engine("glmnet")
preg_wf <- workflow() %>%
add_recipe(my_recipe) %>%
add_model(preg_modelv) 

tuning_grid <- grid_regular(penalty(),mixture(), levels = 5)
folds <- vfold_cv(bike_train, v = 10, repeats =1)
CV_results <- preg_wf %>%
  tune_grid(resamples=folds,
  grid=tuning_grid,
  metrics=metric_set(rmse, mae, rsq))

collect_metrics(CV_results) %>%
  filter(.metric=="rmse") %>%
  ggplot(data=., aes(x=penalty, y=mean, color=factor(mixture)))+
  geom_line()

bestTune <- CV_results %>%
  select_best("rmse")
final_wf <- preg_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data = bike_train)

final_wf %>% 
  predict(new_data = bike_train)


bike_predictions <- predict(final_wf, new_data=bike_test) %>%
  bind_cols(., bike_test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and predictions
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) %>%
  mutate(count= exp(count))

#vroom_write(x=bike_predictions, file="./TestPreds.csv", delim=",")

### Regression Trees###
my_mod <- decision_tree(tree_depth = tune(),
                        cost_complexity = tune(),
                        min_n= tune())%>%
  set_engine("rpart") %>%
  set_mode("regression")

tuning_grid <- grid_regular(tree_depth(),cost_complexity(), min_n(), levels = 3)

preg_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_mod) 

folds <- vfold_cv(bike_train, v = 5, repeats =1)
CV_results <- preg_wf %>%
  tune_grid(resamples=folds,
            grid=tuning_grid,
            metrics=metric_set(rmse, mae, rsq))

collect_metrics(CV_results) %>%
  filter(.metric=="rmse")

bestTune <- CV_results %>%
  select_best("rmse")
final_wf <- preg_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data = bike_train)

final_wf %>% 
  predict(new_data = bike_train)


bike_predictions <- predict(final_wf, new_data=bike_test) %>%
  bind_cols(., bike_test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and predictions
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) %>%
  mutate(count= exp(count))

vroom_write(x=bike_predictions, file="./TestPreds.csv", delim=",")
