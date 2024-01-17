source('http://bit.ly/CEU-R-heights-2018')
# no assignment operator
## TODO compare the avg height
## TODO visualize the dataset

## Compare the average height
mean(heights, na.rm = TRUE)

## Visualize the dataset
hist(heights)

library(ggplot2)
library(data.table)
ggplot(data.table(h = heights), aes(x = h)) + geom_histogram()
ggplot(data.table(h = heights), aes(x = h)) + geom_bar()

# With the histogram--> continuous variable --> this is preferred
# bar --> discrete variable

ggplot(data.table(h = heights), aes(x = h)) + geom_boxplot()
ggplot(data.table(h = heights), aes(x = h)) + geom_density()

# First: never source anything from the internet
# rm(list = ls()) is not cleaning up everything
# things starting with a . will not be discovered by "ls"
ls(all = TRUE)
rm(list = ls(all = TRUE))

# Session -> Restart

library(data.table)
bookings <- fread("http://bit.ly/CEU-R-hotels-2018-prices")
features <- fread("http://bit.ly/CEU-R-hotels-2018-features")

## TODO count the number of bookings in Austria
# merge the two dataset without "all = TRUE"
# all means if we want to keep all the rows from x and y and well
# if we are setting it to TRUE, all rows would be preserved without any values
# left join and right join
# merge has the by argument but it will autoguess if you don't provide anything
?merge
dt <- merge(bookings, features)
dt[country == "Austria", .N]

merge(bookings, features)[country == "Austria", .N]

## TODO which hotel is the cheapest in Vienna within 1 km distance of the city centre
#one_km <- dt[distance_alter <= 1.0]
#one_km <- dt[city_actual == "Vienna"]
#one_km[, min(price)][1]

dt$single_night <- dt$price/dt$nnights

dt[distance_alter <= 1. & city_actual == "Vienna"][order(single_night)][1, single_night]

# CORRECT
dt[city_actual == "Vienna" & distance < 1][, mean(price/nnights), by = hotel_id]
# We want to correct the mean price for every hotel
dt[city_actual == "Vienna" & distance < 1][, list(price = mean(price/nnights)), by = hotel_id][order(price)][1, hotel_id]

dt[city_actual == "Vienna" & distance < 1][, list(price = mean(price/nnights)), by = hotel_id][which.min(price), hotel_id]

dt[city_actual == "Vienna" & distance < 1][, list(price = mean(price/nnights)), by = hotel_id][price == min(price), hotel_id]
?which.min


## TODO create a hotels based on features + number of bookings + avg price per night
bookings$avg_price_per_night <- bookings$price/bookings$nnights

agg <- bookings[, list(bookings = .N, by = list(hotel_id))]
hotels <- merge(features, agg)

hotels <- merge(features, bookings[, list(bookings = .N, price = mean(price/nnights)), by = list(hotel_id)])


## TODO compute the avg price per number of stars
avg_price_by_stars <- hotels[, mean(price), by = factor(stars)][order(avg_price_by_stars)]
avg_price_by_stars

# CORRECT
# dropping NAs with !is.na(stars) or ?na.omit
# renaming with list(new_name = V1)
hotels[!is.na(stars), list(price = mean(price)), by = stars]

## TODO instead of avg, let's do weighted avg
agg <- hotels[!is.na(stars), list(price = weighted.mean(price,bookings)), by = stars][order(stars)]

## TODO visualize the avg price per number of stars
ggplot(agg, aes(x = stars, y = price)) + geom_col()
# geom_bar when using only the x axis
ggplot(agg, aes(x = stars)) + geom_bar()

# Make sure all the values are printed on the x axis!
ggplot(agg, aes(x = factor(stars), y = price)) + geom_col()

## TODO do the same for each country
# by = list(stars, country)
agg <- hotels[!is.na(stars), list(price = weighted.mean(price,bookings)), by = list(stars, country)][order(stars)]

ggplot(agg, aes(x = factor(stars), y = price)) + geom_col() + facet_wrap("country", scales = "free")


## TODO - price -> discrete variable
str(hotels)
?cut
?c
hotels[, pricecat := cut (price, c(0, 100, 250, Inf))]
hotels[, .N, by = pricecat]


# Other argument for labels
hotels[, pricecat := cut (price, c(0, 100, 250, Inf), labels = c("cheap", "average", "expensive"))]
hotels[, .N, by = pricecat]

## TODO use stats to define the break points (median, IQR, mean+sd)

price_mean <- mean(hotels$price)
price_sd <- sd(hotels$price)

hotels[, pricecat := cut (price, c(0, price_mean - price_sd, price_mean + price_sd, Inf), labels = c("cheap", "average", "expensive"))]
hotels[, .N, by = pricecat]
price_mean
price_sd

## TODO use stats per country (mean+sd) to define the break points
hotels[, price_mean := mean(price), by = country]
hotels[, price_sd := sd(price), by = country]
str(hotels)
hotels[, .N, by = .(country, price_mean)]

hotels[!is.na(price_sd), pricecat := cut (price, c(
  0,
  price_mean[1] - price_sd[1],
  price_mean[1] + price_sd[1],
  Inf
  ), labels = c("cheap", "average", "expensive")), by = country]

hotels[price_sd == 0]
hotels[is.na(price_sd)]

## TODO visualize the number of hotels in each category per country
hotels
ggplot(hotels, aes(x = pricecat)) + geom_bar() + facet_wrap("country", scales = "free")
ggplot(hotels, aes(x = country, fill = pricecat)) + geom_bar() + coord_flip()
ggplot(hotels, aes(x = country, fill = pricecat)) + geom_bar(position = "fill") + coord_flip()
# Geom bar automatically assings it, with geom_col, we do it manually
# Instead of list =, we use "."
ggplot(hotels[, .N, by = .(country, pricecat)], aes(x = country, y = N, fill = pricecat)) +
  geom_col(position = "fill") + coord_flip() +
  scale_fill_manual(values = c("yellow", "purple", "pink"))

# Different color scale
ggplot(hotels[, .N, by = .(country, pricecat)], aes(x = country, y = N, fill = pricecat)) +
  geom_col(position = "fill") + coord_flip() +
  scale_fill_brewer(palette = "YlGn")

## ML

# Repeat each twice
?rep
rep(1:10, 2, each = 2)
rep(1:10, each = 2)
## DT: x, y, color (100x100)
points <- data.table(x = rep(1:100, 100), y = rep(1:100, each = 100))
points[, col := "white"]
points[x == 50 & y == 50, col := "red"]
# points[x > 40 & x < 60, col := "red"]
points[(abs(50-x) + abs(50 - y)) < 10, col := "red"] # Diamond

# Getting there
points[, col := "white"]
points[((50 - x) ^ 2 + (50 - y) ^ 2) < 50, col := "red"] 
points[, .N, by = col]

# Geom point
ggplot(points, aes(x,y, color = col)) + geom_point() +
  theme_void()  + theme(legend.position = NULL) +
  labs(x = NULL, y = NULL) +
  scale_color_manual(values=c('red','white')) +
  theme_void() 

# Geom_tile

ggplot(points, aes(x,y, fill = col)) + geom_tile() +
  theme_void()  + theme(legend.position = NULL) +
  labs(x = NULL, y = NULL) +
  scale_fill_manual(values=c('red','white')) +
  theme_void() 

str(points)

## TODO Predicting the color of a pixel
# lm stands for the linear regression model
?lm
# col is the LHS
lm(col ~ x + y, data = points)

# generalized linear model and logistic regression
# Since logit is a probability model, we need to use factor(col) as they are colors
?glm
points$col <- factor(points$col)
fit <- glm(factor(col) ~ x + y, data = points, family = binomial(link = logit))
summary(fit)

# Prediction
?predict
predict(fit, newdata = points, type = "response")
hist(predict(fit, newdata = points, type = "response"))

points$pred <- round(predict(fit, newdata = points, type = "response"))

# Updated ggplot
ggplot(points, aes(x,y, fill = pred)) + geom_tile() +
  theme_void()  + theme(legend.position = NULL) +
  labs(x = NULL, y = NULL) +
  scale_fill_manual(values=c('red','white'))

# New - decision trees
library(rpart)
fit <- rpart(col ~ x + y, data = points)
plot(fit)
text(fit)
?rpart

# If-else repeated many times
library(partykit)
plot(as.party(fit))

points$pred <- predict(fit, newdata = points, type = "class")
ggplot(points, aes(x, y, fill = pred)) + geom_tile() +
  theme_void() + theme(legend.position = 'none') +
  scale_fill_manual(values = c("red", "white"))

# NEW
fit <- rpart(col ~ x + y, data = points, control = rpart.control(minsplit = 1))
plot(as.party(fit))
ggplot(points, aes(x, y, fill = pred)) + geom_tile() +
  theme_void() + theme(legend.position = 'none') +
  scale_fill_manual(values = c("red", "white"))

# NEW
fit <- rpart(col ~ x + y, data = points, control = rpart.control(cp = 0, minsplit = 1))
plot(as.party(fit))

# CORRECT:
fit <- rpart(col ~ x + y, data = points, control = rpart.control(cp = 0, minsplit = 1))
fit
plot(as.party(fit))
points$pred <- predict(fit, newdata = points, type = "class")
ggplot(points, aes(x, y, fill = pred)) + geom_tile() +
  theme_void() + theme(legend.position = 'none') +
  scale_fill_manual(values = c("red", "white"))

# FIRST FIT
# custom mapping for each geom separately
fit <- rpart(col ~ x + y, data = points)
points$pred <- predict(fit, newdata = points, type = "class")
ggplot(points, aes(x, y, fill = pred)) + 
  geom_tile(aes(fill = col), alpha = .5) + # actual color
  geom_tile(aes(fill = pred), alpha = .5) + # predicted
  theme_void() + theme(legend.position = 'none') +
  scale_fill_manual(values = c("red", "white"))

## confusion matrix
points[, .N, by = list(pred, col)]
# 28 pixels where the prediction was off
# this can be used to describe what we have missed in our model

## reshape: long <> wide (melting)
?dcast # long-to-wide
?melt # wide-to-long
# this is the actual confusion matrix
dcast(points[, .N, by = list(pred, col)], pred ~ col)

#  h2o
# we get a better fit with randomForest
install.packages("randomForest")
library(randomForest)

fit <- randomForest(col ~ x + y, data = points)
points$pred <- predict(fit, newdata = points, type = "class")
ggplot(points, aes(x, y, fill = pred)) + 
  geom_tile(aes(fill = col), alpha = .5) + # actual color
  geom_tile(aes(fill = pred), alpha = .5) + # predicted
  theme_void() + theme(legend.position = 'none') +
  scale_fill_manual(values = c("red", "white"))

fit

## feature engineering
points[, x2 := (50 -x) ^ 2]
points[, y2 := (50 -y) ^ 2]

fit <- glm(factor(col) ~ x2 + y2, data = points, family = binomial(link = logit))
fit
summary(fit)


points$pred <- factor(predict(fit, newdata = points, type = "response"))
ggplot(points, aes(x, y)) + 
  geom_tile(aes(fill = pred), alpha = .5)
