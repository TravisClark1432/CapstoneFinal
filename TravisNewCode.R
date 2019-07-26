library(dplyr)
library(tidyr)
library(tidyverse)
library(haven)
library(sjmisc)
library(lubridate) 
library(caTools)
library(caret)
setwd("C:/Users/travis.clark/Desktop")
complaints <- read_csv("TravisDataset.csv")
glimpse(complaints)

#dataset cleaned
df_ml <- complaints %>% filter(
  State %in% c('CA', 'FL', 'TX', 'GA'),
  Company %in% c('WELLS FARGO & COMPANY', 'EQUIFAX, INC.','BANK OF AMERICA, NATIONAL ASSOCIATION')
) %>% mutate(Month = month(`Date sent to company`,label = TRUE)) %>% 
  select(Product, Company, State, `Submitted via`, Month, `Timely response?`)


df_ml$Timely_respone<- ifelse(df_ml$'Timely response?' == "Timely response?",1,0)


# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(df_ml$Product, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- df_ml[-validation_index,]
# use the remaining 80% of data to training and testing the models
df_ml <- df_ml[validation_index,]






#Statistics 
(Products <- complaints %>% count(Product) %>% arrange(desc(n)))
ggplot(Products, aes(x = reorder(Product,n), y =n, fill = n)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  labs(x = NULL, y = NULL)


complaints %>% count(State) %>% arrange(desc(n)) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(State, n), y = n, fill = n)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  labs(x = NULL, y = NULL)


companies <- complaints %>% count(Company) %>% arrange(desc(n)) %>%
  top_n(10)
ggplot(companies, aes(x = reorder(Company, n), y = n, fill = n)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  labs(x = NULL, y = NULL)

#Issues and Sub-Issue plots
df1 <- complaints %>% count(Issue) %>% arrange(desc(n)) %>%
  top_n(10)
ggplot(df1, aes(x = reorder(Issue, n), y = n, fill = n)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  labs(x = NULL, y = NULL)

glimpse(df)


df2 <- complaints %>% count(`Sub-issue`) %>% arrange(desc(n)) %>%
  top_n(10)
ggplot(df2, aes(x = reorder(`Sub-issue`, n), y = n, fill = n)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  labs(x = NULL, y = NULL)

#Machine Learning 
model2 = glm(Timely_respone ~ State + Company + Month + Product, data=df_ml, family=binomial)
summary(model2)

model4 = glm(Timely_respone ~ Company + State, data=df_ml, family=binomial)
summary(model4)


str(complaints)
summary(complaints)
head(df_ml)
str(df_ml)


glimpse(df_ml)

