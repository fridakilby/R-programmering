library(MASS)  
library(leaps)  
library(Metrics)
library(readxl)
library(car)

# Load manual created dataset ----------------------------------------------
file_path_1 <- "C:/Users/frida/Documents/ec/R-programmering/Kunskapskontroll/data_manual.xlsx"
manual_data <- read_excel(file_path_1)

# Check dataset --------------------------------------------------------------------
dim(manual_data)
head(manual_data)
str(manual_data)
summary(manual_data)

# Create model manual_data--------------------------------------------------------------------
lm_md <- lm(Price ~ Miles + ModelYear + Horsepower, data = manual_data)
summary(lm_md)

par(mfrow = c(2, 2))
plot(lm_md)

# Create a dummy variable for location --------------------------------------------------------------------
manual_data$location_dalarna <- ifelse(manual_data$Location == "Dalarna", 1, 0)
manual_data$location_gbg <- ifelse(manual_data$Location == "Göteborg", 1, 0)
manual_data$location_sthlm <- ifelse(manual_data$Location == "Stockholm", 1, 0)
manual_data$location_orebro <- ifelse(manual_data$Location == "Örebro", 1, 0)
manual_data$location_uppsala <- ifelse(manual_data$Location == "Uppsala", 1, 0)
# if none of above location is gotland --------------------------------------------------------------------

# Show the updated data frame --------------------------------------------------------------------
str(manual_data)

lm_md2 <- lm(Price ~ Miles + ModelYear + Horsepower + location_dalarna + location_uppsala + location_orebro + location_sthlm + location_gbg, data = manual_data)
summary(lm_md2)
plot(lm_md2)
vif(lm_md2)

# Create model manual_data--------------------------------------------------------------------
lm_md3 <- lm(log(Price) ~ Miles + ModelYear + Horsepower, data = manual_data)
summary(lm_md3)

par(mfrow = c(2, 2))
plot(lm_md3)

lm_md4 <- lm(log(Price) ~ Miles + ModelYear + Horsepower + location_dalarna + location_uppsala + location_orebro + location_sthlm + location_gbg, data = manual_data)
summary(lm_md4)
plot(lm_md4)
vif(lm_md4)

BIC(lm_md)
BIC(lm_md2)

BIC(lm_md3)
BIC(lm_md4)

confint(lm_md4)

# New data that we want to predict --------------------------------------------------------------------
new_car <- data.frame(
  Miles = c(3000, 3000, 3000, 3000, 3000,3000),
  ModelYear = c(2022, 2022, 2022, 2022, 2022,2022),
  Horsepower = c(120, 120, 120, 120, 120,120),
  location_dalarna = c(1,0,0,0,0,0),
  location_uppsala = c(0,1,0,0,0,0),
  location_orebro = c(0,0,1,0,0,0),
  location_sthlm = c(0,0,0,1,0,0),
  location_gbg = c(0,0,0,0,1,0)
)

# Create CI & PI for predictions --------------------------------------------------------------------
confidence_intervals <- predict(lm_md4, newdata = new_car, interval = "confidence", level = 0.95)
prediction_intervals <- predict(lm_md4, newdata = new_car, interval = "prediction", level = 0.95)

exp(confidence_intervals)
exp(prediction_intervals)




# Load datascraped dataset --------------------------------------------------------------------
file_path_2 <- "C:/Users/frida/Documents/ec/R-programmering/Kunskapskontroll/data_scraping_location.xlsx"
datascraping_data <- read_excel(file_path_2)

# Check dataset --------------------------------------------------------------------
dim(datascraping_data)
head(datascraping_data)
str(datascraping_data)
summary(datascraping_data)

# Create model datascraping_data --------------------------------------------------------------------
lm_dd <- lm(Price ~ Miles + Year, data = datascraping_data)
summary(lm_dd)
plot(lm_dd)
vif(lm_dd)

# Create a dummy variable for location --------------------------------------------------------------------
datascraping_data$location_stockholm <- ifelse(datascraping_data$Location == "Stockholm", 1, 0)
datascraping_data$location_gotland <- ifelse(datascraping_data$Location == "Gotland", 1, 0)
datascraping_data$location_goteborg <- ifelse(datascraping_data$Location == "Göteborg", 1, 0)
datascraping_data$location_skane <- ifelse(datascraping_data$Location == "Skåne", 1, 0)
datascraping_data$location_sodermanland <- ifelse(datascraping_data$Location == "Södermanland", 1, 0)
datascraping_data$location_uppsala <- ifelse(datascraping_data$Location == "Uppsala", 1, 0)
datascraping_data$location_orebro <- ifelse(datascraping_data$Location == "Örebro", 1, 0)

dim(datascraping_data)

# Create model datascraping_data with location --------------------------------------------------------------------
lm_dd2 <- lm(Price ~ Miles + Year + location_stockholm + location_gotland + location_goteborg + location_skane + location_sodermanland +location_uppsala + location_orebro, data = datascraping_data)
summary(lm_dd2)
par(mfrow = c(2, 2))
plot(lm_dd2)
vif(lm_dd2)

# Checking points that can be influencal points --------------------------------------------------------------------
datascraping_data[5335,]
datascraping_data[2210,]
datascraping_data[2032,]
datascraping_data[2008,]
datascraping_data[1920,]


# Setting Price = 1 to NaN to erase point  --------------------------------------------------------------------
datascraping_data$Price <- ifelse(datascraping_data$Price == 1, NaN, datascraping_data$Price)
summary(datascraping_data)

# Create correlation matrix --------------------------------------------------------------------
corr_matrix <- round(cor(datascraping_data[,c(2,4,9,10,11,12,13,14,15)]),2)
corr_matrix

# Create same models with log on Y to check hetroskedasitet --------------------------------------------------------------------
lm_dd3 <- lm(log(Price) ~ Miles + Year, data = datascraping_data)
summary(lm_dd3)
par(mfrow = c(2, 2))
plot(lm_dd3)
vif(lm_dd3)

lm_dd4 <- lm(log(Price) ~ Miles + Year + location_stockholm + location_gotland + location_goteborg + location_skane + location_sodermanland +location_uppsala + location_orebro, data = datascraping_data)
summary(lm_dd4)
par(mfrow = c(2, 2))
plot(lm_dd4)
vif(lm_dd4)


# Evaluation for models --------------------------------------------------------------------
BIC(lm_dd3)
BIC(lm_dd4)

# New data that we want to predict
new_car <- data.frame(
  Miles = c(3000, 3000, 3000, 3000, 3000,3000,3000,3000),
  Year = c(2022, 2022, 2022, 2022, 2022,2022,2022,2022),
  location_stockholm = c(1,0,0,0,0,0,0,0),
  location_gotland = c(0,1,0,0,0,0,0,0),
  location_goteborg = c(0,0,1,0,0,0,0,0),
  location_skane = c(0,0,0,1,0,0,0,0),
  location_sodermanland = c(0,0,0,0,1,0,0,0),
  location_uppsala = c(0,0,0,0,0,1,0,0),
  location_orebro = c(0,0,0,0,0,0,1,0)
)

# Create CI & PI for predictions --------------------------------------------------------------------
confidence_intervals <- predict(lm_dd4, newdata = new_car, interval = "confidence", level = 0.95)
prediction_intervals <- predict(lm_dd4, newdata = new_car, interval = "prediction", level = 0.95)

exp(confidence_intervals)
exp(prediction_intervals)

