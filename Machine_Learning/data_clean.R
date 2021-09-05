#==============================#
#     Cleaning Data Process    #
#       Board Games Geek       #
#==============================#

# Goal --------
# Create and tune the optimal data format and values for 
# Machine Learning process 

# Libraries ----------
library(tidyverse)
library(stringr)
library(lubridate)

# Load dataset -------
train <- read.csv('~/Desktop/kaggle_competitions/Data/s01e01/train.csv')
test <- read.csv('~/Desktop/kaggle_competitions/Data/s01e01/test.csv')


## Finding skewed data ----

histograms <- function(x) {
  train %>%
  ggplot(aes(x = train %>% select(x) %>% pull()))+
  geom_vline(xintercept = mean(train %>% select(x) %>% pull()), color = 'red',linetype = 'dashed')+
  geom_histogram(alpha=.5) +
  labs(x = paste0('# of ',x %>% str_replace(.,'_',' ')),
       title = paste0(x %>% str_replace(.,'_',' '),' Histogram'))
}

# Extract only numeric data 
columns_row<-train %>%
  select(-game_id) %>%
  select(where(is.numeric)) %>%
  colnames()

all_histograms <- lapply(columns_row,histograms)

# Filter and feature engineering steps ----

train <-train %>%
  filter(min_players <= 33,
         min_players >0,
         max_players >0,
         min_time > 0,
         year >= 2004 & year < 2018 ,
         num_votes < 2000,
         age >0 & age <= 20) %>%
  separate_rows(mechanic, sep = ', ')

test <- test %>%
  filter(min_players <= 33,
         min_players >0,
         max_players >0,
         min_time > 0,
         year >= 2004 & year < 2018 ,
         num_votes < 2000,
         age >0 & age <= 20) %>%
  separate_rows(mechanic, sep = ', ')


## Data Format -------



train <- train %>%
        transform(
            names = factor(names),
            category1 = factor(category1),
            designer = factor(designer)
    )
test <- test %>%
         transform(
            names = factor(names),
            category1 = factor(category1),
            designer = factor(designer)
    )

dir.create('input')


train %>%
    write.csv('input/train.csv')

test %>%
    write.csv('input/test.csv')
