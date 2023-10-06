library(tidyverse)
library(tidymodels)
library(vroom)
library(poissonreg)
library(stacks)

bike_train <- vroom('C:/BYU/2023(5) Fall/STAT 348/KaggleBikeShare/train.csv',
                    show_col_types = FALSE) %>%
  select(datetime, season, holiday, workingday, weather, temp, humidity, windspeed, count) %>%
  mutate(count= log(count))

bike_test <-vroom('C:/BYU/2023(5) Fall/STAT 348/KaggleBikeShare/test.csv',
                  show_col_types = FALSE)

my_recipe <- recipe(count~., bike_train) %>%
  step_time(datetime, features=c("hour"), keep_original_cols = T) %>% 
  step_date(datetime, features=c("year"), keep_original_cols = F) %>%
  step_mutate(season = as.factor(season),
              holiday = as.factor(holiday),
              weather = replace(weather, weather == 4, 3),
              weather= as.factor(weather),
              workingday= as.factor(workingday),
              hour= as.factor(datetime_hour),
              year = as.factor(datetime_year)
  )%>%
  step_dummy(all_nominal_predictors())%>%
  step_normalize(all_numeric_predictors())
prepped_recipe <- prep(my_recipe)
bake(prepped_recipe, new_data= bike_train)


untunedModel <- control_stack_grid()
tunedModel <- control_stack_resamples()

folds <- vfold_cv(bike_train, v = 5, repeats =1)

##########################
###Linear Regression###
##########################

lin_reg <- 
  linear_reg() %>% 
  set_engine("lm") 

lin_reg_wf <- workflow() %>%
  add_model(lin_reg) %>%
  add_recipe(my_recipe)

lin_reg_model <- fit_resamples( lin_reg_wf,
                                resamples = folds,
                                metrics = metric_set(rmse,mae,rsq),
                                control = tunedModel)
########################
###Poisson Regression###
########################

pois_reg <- 
  poisson_reg() %>%
  set_engine("glm")

bike_pois_wf <- workflow()%>%
  add_recipe(my_recipe) %>%
  add_model(pois_reg)

pois_model <- fit_resamples( bike_pois_wf,
                             resamples = folds,
                             metrics = metric_set(rmse,mae,rsq),
                             control = tunedModel)

##########################
###Penalized Regression###
##########################

preg <- linear_reg(penalty=tune(),
                         mixture=tune()) %>%
  set_engine("glmnet") # Engine = what r function to use

preg_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(preg)

preg_tuning_grid <- grid_regular(penalty(),
                                 mixture(),
                                 levels = 5)

preg_models <- preg_wf %>%
  tune_grid(resamples=folds,
            grid=preg_tuning_grid,
            metric=metric_set(rmse,mae,rsq),
            control = untunedModel)

#####################
### Random Forests###
#####################

randf <- rand_forest(mtry = tune(),
                      min_n= tune(),
                      trees = 500)%>%
  set_engine("ranger") %>%
  set_mode("regression")

randf_tuning_grid <- grid_regular(mtry(c(25,30)), min_n(), levels = 3)
rfspec <- rand_forest(mtry = 30,
                     min_n= 21,
                     trees = 500)%>%
  set_engine("ranger") %>%
  set_mode("regression")

randf_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(rfspec) 

rf_models <- randf_wf %>%
  tune_grid(resamples=folds,
            #grid=randf_tuning_grid,
            metrics=metric_set(rmse),
            control = untunedModel)

###################
###stacked Model###
###################


bike_stack <- stacks() %>%
  add_candidates(lin_reg_model)%>%
  add_candidates(preg_models) %>%
  add_candidates(pois_model) %>%
  add_candidates(rf_models)

as_tibble(bike_stack)

fitted_bike_stack <- bike_stack %>%
  blend_predictions() %>%
  fit_members()

stacked_predictions <- predict(fitted_bike_stack, new_data=bike_test) %>%
  bind_cols(., bike_test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and predictions
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) %>%
  mutate(count= exp(count))

vroom_write(x=stacked_predictions, file="./TestPreds.csv", delim=",")

