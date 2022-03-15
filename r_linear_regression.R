#Linear Regression
library(tidyverse)
library(dplyr)
library(janitor)

df <- read_csv("data/data.csv")

df <- df %>%
  clean_names()

sum(is.na(df))
df <- na.omit(df) 

write_csv(df, "cars2.csv")

df <- df %>%
  mutate_if(is.character,
    as.factor)

#correlation
cor(df$engine_hp, df$city_mpg)
cor(df$engine_cylinders, df$engine_hp)

#correlation matrix
cor(df[ , c("city_mpg", 
            "engine_cylinders",
            "engine_hp")])

# %>% dplyr
df %>%
  select(city_mpg, engine_cylinders,
         engine_hp) %>%
  cor()

#correlation coefficient
cor.test(df$engine_hp, df$city_mpg)


#linear regression 
lm_model <- lm(city_mpg ~ engine_hp, 
               data = df)

summary(lm_model)

#prediction
lm_model$coefficients

new_car <- lm_model$coefficients[[1]] + 
  lm_model$coefficients[[2]] * 220

new_car <- data.frame(
  engine_hp = c(240, 290, 310, 190, 450))

#predict 
new_car$city_mpg_pred <- predict(lm_model, newdata = new_car)
summary(df$engine_hp)

#
df$city_mpg_pred <- predict(lm_model, newdata = df)

#RMSE
error <- (df$city_mpg - 
  df$city_mpg_pred) **2
rmse <- sqrt(mean(error))


#Multiple Linear Regression
#city_mpg = f(engine_hp,engine_cylinders,number_of_doors,highway_mpg,popularity,msrp)
#city_mpg = b0 + b1*engine_hp + b2*engine_cylinders + b3*number_of_doors + b4*highway_mpg + b5* +popularity + b6*msrp
lm_model_v2 <- lm(city_mpg ~ engine_hp +
                  engine_cylinders +
                  number_of_doors +
                  highway_mpg +
                  popularity +
                  msrp,
                  data = df)

coefs <- lm_model_v2$coefficients

coefs[[1]] + coefs[[2]]*220 +
  coefs[[3]]*4 + coefs[[4]]*4 +
  coefs[[5]]*33 + coefs[[6]]* 2000 +
  coefs[[6]]*2999

#prediction
df$city_mpg_pred <- predict(lm_model_v2, newdata = df)
#RMSE
error <- (df$city_mpg - 
            df$city_mpg_pred) **2
rmse <- sqrt(mean(error))


#Full model
lm_model_v3 <- lm(city_mpg ~ ., data = df)

#prediction
df$city_mpg_pred <- predict(lm_model_v3)
#RMSE
error <- (df$city_mpg - 
            df$city_mpg_pred) **2
rmse <- sqrt(mean(error))


#model training 
#split data
set.seed(42)
n <- nrow(df)
id <- sample(1:n, size = n*0.8)
train_data <- df[id, ]
test_data <- df[-id, ]

#train model
model <- lm(city_mpg ~ engine_hp,
            data = train_data)

p_train <- predict(model)
error_train <- train_data$city_mpg - p_train
rmse_train <- sqrt(mean(error_train**2))

#test model 
p_test <- predict(model, newdata = test_data)
error_test <- test_data$city_mpg - p_test 
rmse_test <- sqrt(mean(error_test**2))

cat("RMSE Train:", rmse_train,
    "\nRMSE Test", rmse_test)


########################
#Logistic Regression
data_car <- df %>%
  filter(transmission_type == "MANUAL" |
           transmission_type == "AUTOMATIC")

data_car %>%
  count(transmission_type)

data_car <- data_car %>%
  mutate_if(is.character,
            as.factor)

# Manual 1/Automatic 0
data_car$transmission_type <- factor(data_car$transmission_type, 
                    levels = c("MANUAL", "AUTOMATIC"),
                    labels = c("Manual", "Automatic"))

data_car$transmission_type <- factor(data_car$transmission_type, 
                     levels = c("Automatic", "Manual"),
                      labels = c(0, 1))
  
class(data_car$transmission_type)
table(data_car$transmission_type)

#split data
set.seed(42)
n <- nrow(data_car)
id <- sample(1:n, size = n*0.8)
train_data <- data_car[id, ]
test_data <- data_car[-id, ]

#train model 
logit_model <- glm(transmission_type ~ city_mpg,
                   data = train_data,
                   family = "binomial")
p_train <- predict(logit_model, type = "response")
train_data$pred <- if_else(p_train >= 0.5, 
                           1, 0)
accuracy_train <- mean(train_data$transmission_type == train_data$pred)

#test model 
p_test <- predict(logit_model, newdata = test_data,
                  type = "response")
test_data$pred <- if_else(p_test >= 0.5, 
                           1, 0)
accuracy_test <- mean(test_data$transmission_type == test_data$pred)

cat("accuracy Train:", accuracy_train,
    "\naccuracy Test", accuracy_test)