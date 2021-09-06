#=====================================#
#     Machine Learning Model Design   #
#           Board Games Geek          #
#=====================================#

# Goal --------
# Design and select the best model for this problem
# With model competition on RMSE and MAE metrics 

# Libraries ----
library(tidyverse)
library(tidymodels)
library(vip)
theme_set(theme_light())

## Load Dataset ----
train <- read_csv('input/train.csv') %>%
  select(-'...1')

test <- read_csv('input/test.csv') %>%
  select(-'...1')

## Modeling ----
set.seed(1234)
split <- initial_split(train %>%
  select(-c(game_id,category2:category12)) %>% na.omit(),prop = .8)

train_set <- training(split)
test_set <- testing(split)
train_folds <- train_set %>%
  vfold_cv(10)

mset <- metric_set(rmse,mae)

## Two models ----=
set.seed(5678)
lin_spec <- linear_reg(penalty = tune()) %>%
  set_engine('lm')

xgb_spec <- boost_tree("regression",
                      mtry = tune(),
                      trees = tune(),
                      learn_rate = tune()) %>%
  set_engine("xgboost")

## Recipes -----
lin_rec <- recipe(geek_rating ~.,data=train_set) %>%
  step_zv(all_predictors()) 
xgb_rec <- recipe(geek_rating ~min_players+max_players+avg_time+num_votes+age,data=train_set) %>%
  step_zv(all_predictors())


## Workflows -----

lin_wf <- workflow() %>%
  add_recipe(lin_rec)%>%
  add_model(lin_spec)

xgb_wf <- workflow() %>%
  add_recipe(xgb_rec) %>%
  add_model(xgb_spec)

## Tune model ------

model_control <- control_grid(save_pred = TRUE, save_workflow = TRUE)

linear_cv <- lin_wf %>%
  fit_resamples(train_folds)


line_tune <- lin_wf %>%
  tune_grid(train_folds,
            grid = crossing(penalty = 10 ^ seq(-5, -2, .1)),
            metrics = mset,
            control = model_control)



xgb_cv <- xgb_wf %>%
  tune_grid(train_folds,
            grid = crossing(trees = c(1000, 3000, 5000),
                            learn_rate = c(c(.005, .01)),
                            mtry = 2:6),
            metrics = mset,
            control=model_control)


## Finding the best performance metrics ------


show_best(linear_cv,'rmse')

# A tibble: 1 × 6
#  .metric .estimator     mean     n  std_err .config             
#  <chr>   <chr>         <dbl> <int>    <dbl> <chr>               
# rmse    standard   3.22e-13    10 3.65e-14 Preprocessor1_Model1

xgb_cv %>%
  show_best('rmse')


# mtry trees learn_rate .metric .estimator   mean     n std_err .config        
#  <int> <dbl>      <dbl> <chr>   <chr>       <dbl> <int>   <dbl> <chr>          
#1     4  5000       0.01 rmse    standard   0.0673    10 0.00173 Preprocessor1_…
#2     5  5000       0.01 rmse    standard   0.0682    10 0.00188 Preprocessor1_…
#3     6  5000       0.01 rmse    standard   0.0682    10 0.00188 Preprocessor1_…
#4     3  5000       0.01 rmse    standard   0.0686    10 0.00192 Preprocessor1_…
#5     2  5000       0.01 rmse    standard   0.0735    10 0.00179 Preprocessor1_…


xgboost_final_param<-xgb_cv %>% 
  show_best("rmse") %>%
  ungroup()%>%
  dplyr::slice(1) %>%
  select(mtry:learn_rate)

# A tibble: 1 × 3
#   mtry trees learn_rate
#  <int> <dbl>      <dbl>
#1     4  5000       0.01

# The best performance model : Xgbost 
# With this configuration

xgb_cv %>% 
  show_best("rmse") %>%
  ungroup()%>%
  dplyr::slice(1) 


# A tibble: 1 × 9
#   mtry trees learn_rate .metric .estimator   mean     n std_err .config        
#  <int> <dbl>      <dbl> <chr>   <chr>       <dbl> <int>   <dbl> <chr>          
#1     4  5000       0.01 rmse    standard   0.0673    10 0.00173 Preprocessor1_…

xgb_wf <- xgb_wf %>%
  finalize_workflow(xgboost_final_param) %>%
  last_fit(split)


xgb_wf %>%
  collect_metrics()


# A tibble: 2 × 4
#  .metric .estimator .estimate .config             
#  <chr>   <chr>          <dbl> <chr>               
#1 rmse    standard      0.0692 Preprocessor1_Model1
#2 rsq     standard      0.944  Preprocessor1_Model1 

# Save Model ----
dir.create('models')

xgb_wf %>%
    saveRDS('models/prediction_model.rds')

## Predictions -----

predict(xgb_wf$.workflow[[1]],test[19,]) 

# A tibble: 1 × 1
#  .pred
#  <dbl>
#1  6.43