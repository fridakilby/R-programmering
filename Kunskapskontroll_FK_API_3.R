library(pxweb)
library(dplyr)
library(Metrics)
library(tidyverse)

# fetching data from SCB --------------------------------------------------------------------
pxweb_query_list <- 
  list("Region"=c("01","03","04","09","12","18","20","1480"),
       "Drivmedel"=c("100","110","120","130","140","150","160","190"),
       "ContentsCode"=c("TK1001AA"),
       "Tid"=c("2006M01","2006M02","2006M03","2006M04","2006M05","2006M06","2006M07","2006M08","2006M09","2006M10","2006M11","2006M12","2007M01","2007M02","2007M03","2007M04","2007M05","2007M06","2007M07","2007M08","2007M09","2007M10","2007M11","2007M12","2008M01","2008M02","2008M03","2008M04","2008M05","2008M06","2008M07","2008M08","2008M09","2008M10","2008M11","2008M12","2009M01","2009M02","2009M03","2009M04","2009M05","2009M06","2009M07","2009M08","2009M09","2009M10","2009M11","2009M12","2010M01","2010M02","2010M03","2010M04","2010M05","2010M06","2010M07","2010M08","2010M09","2010M10","2010M11","2010M12","2011M01","2011M02","2011M03","2011M04","2011M05","2011M06","2011M07","2011M08","2011M09","2011M10","2011M11","2011M12","2012M01","2012M02","2012M03","2012M04","2012M05","2012M06","2012M07","2012M08","2012M09","2012M10","2012M11","2012M12","2013M01","2013M02","2013M03","2013M04","2013M05","2013M06","2013M07","2013M08","2013M09","2013M10","2013M11","2013M12","2014M01","2014M02","2014M03","2014M04","2014M05","2014M06","2014M07","2014M08","2014M09","2014M10","2014M11","2014M12","2015M01","2015M02","2015M03","2015M04","2015M05","2015M06","2015M07","2015M08","2015M09","2015M10","2015M11","2015M12","2016M01","2016M02","2016M03","2016M04","2016M05","2016M06","2016M07","2016M08","2016M09","2016M10","2016M11","2016M12","2017M01","2017M02","2017M03","2017M04","2017M05","2017M06","2017M07","2017M08","2017M09","2017M10","2017M11","2017M12","2018M01","2018M02","2018M03","2018M04","2018M05","2018M06","2018M07","2018M08","2018M09","2018M10","2018M11","2018M12","2019M01","2019M02","2019M03","2019M04","2019M05","2019M06","2019M07","2019M08","2019M09","2019M10","2019M11","2019M12","2020M01","2020M02","2020M03","2020M04","2020M05","2020M06","2020M07","2020M08","2020M09","2020M10","2020M11","2020M12","2021M01","2021M02","2021M03","2021M04","2021M05","2021M06","2021M07","2021M08","2021M09","2021M10","2021M11","2021M12","2022M01","2022M02","2022M03","2022M04","2022M05","2022M06","2022M07","2022M08","2022M09","2022M10","2022M11","2022M12","2023M01","2023M02","2023M03","2023M04","2023M05","2023M06","2023M07","2023M08","2023M09","2023M10","2023M11","2023M12","2024M01","2024M02","2024M03"))

# Download data  --------------------------------------------------------------------
px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/TK/TK1001/TK1001A/PersBilarDrivMedel",
            query = pxweb_query_list)

# Convert to data.frame  --------------------------------------------------------------------
px_data_frame_downloaded <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")


# FROM HERE WHEN YOU RESTART WITHOUT FETCHING DATA AGAIN --------------------------------------------------------------------

px_data_frame <- px_data_frame_downloaded

# Checking data --------------------------------------------------------------------

dim(px_data_frame)
head(px_data_frame)
str(px_data_frame)
summary(px_data_frame)

# Switch name on Y-variable --------------------------------------------------------------------

names(px_data_frame)[names(px_data_frame) == "Nyregistrerade personbilar"] <- "bilar"

# Divide Månad to Month and year --------------------------------------------------------------------
month <- as.data.frame(strsplit(px_data_frame$månad, "M"))

trans_month <- t(month)

px_data_frame$year <- as.numeric(paste(trans_month[,1]))
px_data_frame$month <- as.numeric(paste(trans_month[,2]))

# Renaming dataframe --------------------------------------------------------------------

px_data_frame_to_2 <- px_data_frame

# Sum cars per year and all regions together --------------------------------------------------------------------

px_data_frame2 <-
px_data_frame_to_2 %>%
  group_by(drivmedel, year) %>%
  summarise_at(vars(bilar),
               sum) %>%
  ungroup()

dim(px_data_frame2)
head(px_data_frame2)
str(px_data_frame2)
summary(px_data_frame2)

# Making Dummy Variables --------------------------------------------------------------------

px_data_frame2$fuel_el <- ifelse(px_data_frame2$drivmedel == "el", 1, 0)
px_data_frame2$fuel_bensin <- ifelse(px_data_frame2$drivmedel == "bensin", 1, 0)
px_data_frame2$fuel_diesel <- ifelse(px_data_frame2$drivmedel == "diesel", 1, 0)
px_data_frame2$fuel_elhybrid <- ifelse(px_data_frame2$drivmedel == "elhybrid", 1, 0)
px_data_frame2$fuel_laddhybrid <- ifelse(px_data_frame2$drivmedel == "laddhybrid", 1, 0)
px_data_frame2$fuel_etanol <- ifelse(px_data_frame2$drivmedel == "etanol/etanol flexifuel", 1, 0)
px_data_frame2$fuel_gas <- ifelse(px_data_frame2$drivmedel == "gas/gas flexifuel", 1, 0)

# Sheck data again--------------------------------------------------------------------
dim(px_data_frame2)
head(px_data_frame2)
str(px_data_frame2)
summary(px_data_frame2)


# Train, val, test - split model to work with different sets --------------------------------------------------------------------
# https://stackoverflow.com/questions/36068963/r-how-to-split-a-data-frame-into-training-validation-and-test-sets
spec = c(train = .6, validate = .2, test = .2)

set.seed(123)
g = sample(cut(
  seq(nrow(px_data_frame2)), 
  nrow(px_data_frame2)*cumsum(c(0,spec)),
  labels = names(spec)
))

res = split(px_data_frame2, g)

px_data_frame2_train <- res$train
px_data_frame2_val <- res$validate
px_data_frame2_test <- res$test

# Create first model --------------------------------------------------------------------
lm_ed3 <- lm(bilar ~ year+fuel_el+fuel_bensin+fuel_diesel+fuel_elhybrid+fuel_laddhybrid+fuel_etanol+fuel_gas, data = px_data_frame2_train)
summary(lm_ed3)

par(mfrow = c(2, 2))
plot(lm_ed3)
vif(lm_ed3)

# Make bilar with zero as value to NaN to be able to log Y-variable ---------------------------------------------------------------
px_data_frame2_train$bilar <- ifelse(px_data_frame2_train$bilar == 0, NaN, px_data_frame2_train$bilar)

# Create new model --------------------------------------------------------------------
lm_ed6 <- lm(log(bilar) ~ year+fuel_el+fuel_bensin+fuel_diesel+fuel_elhybrid+fuel_laddhybrid+fuel_etanol+fuel_gas, data = px_data_frame2_train)
summary(lm_ed6)

par(mfrow = c(2, 2))
plot(lm_ed6)
vif(lm_ed6)

# evalute models --------------------------------------------------------------------
val_pred_ed3 <- predict(lm_ed3, newdata = px_data_frame2_val)
val_pred_ed6 <- exp(predict(lm_ed6, newdata = px_data_frame2_val))

results <- data.frame(
  Model = c("ed3", "ed6"),
  RMSE = c(rmse(px_data_frame2_val$bilar, val_pred_ed3),
           rmse(px_data_frame2_val$bilar, val_pred_ed6)),
  Adj_R = c(summary(lm_ed3)$adj.r.squared,
            summary(lm_ed6)$adj.r.squared),
  BIC = c(BIC(lm_ed3), BIC(lm_ed6))
)
results

# evalute models with test-data --------------------------------------------------------------------
test_pred_ed3 <- predict(lm_ed3, newdata = px_data_frame2_test)
test_pred_ed6 <- exp(predict(lm_ed6, newdata = px_data_frame2_test))

results <- data.frame(
  Model = c("ed3", "ed6"),
  RMSE = c(rmse(px_data_frame2_test$bilar, test_pred_ed3),
           rmse(px_data_frame2_test$bilar, test_pred_ed6)),
  Adj_R = c(summary(lm_ed3)$adj.r.squared,
            summary(lm_ed6)$adj.r.squared),
  BIC = c(BIC(lm_ed3), BIC(lm_ed6))
)
results

summary(lm_ed6)
confint(lm_ed6)


# retrain model and predict values --------------------------------------------------------------------

px_data_frame2$bilar <- ifelse(px_data_frame2$bilar == 0, NaN, px_data_frame2$bilar)

lm_ed7 <- lm(log(bilar) ~ year+fuel_el+fuel_bensin+fuel_diesel+fuel_elhybrid+fuel_laddhybrid+fuel_etanol+fuel_gas, data = px_data_frame2)
summary(lm_ed7)

new_cars <- data.frame(
  year = c(2024,2024,2024,2024,2024,2024,2024,2024),
  fuel_el = c(1,0,0,0,0,0,0,0),
  fuel_bensin = c(0,1,0,0,0,0,0,0),
  fuel_diesel = c(0,0,1,0,0,0,0,0),
  fuel_elhybrid = c(0,0,0,1,0,0,0,0),
  fuel_laddhybrid = c(0,0,0,0,1,0,0,0),
  fuel_etanol = c(0,0,0,0,0,1,0,0),
  fuel_gas = c(0,0,0,0,0,0,1,0)
  )
cars <- exp(predict(lm_ed7, newdata = new_cars))
cars

confidence_intervals <- predict(lm_ed7, newdata = new_cars, interval = "confidence", level = 0.95)
prediction_intervals <- predict(lm_ed7, newdata = new_cars, interval = "prediction", level = 0.95)

exp(confidence_intervals)
exp(prediction_intervals)



