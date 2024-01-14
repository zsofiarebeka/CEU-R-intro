library(ggplot2)
library(data.table)
hotels <- readRDS(url('http://bit.ly/CEU-R-hotels-2018-merged'))

# Reading the df
hotels

# Making a data table of hotels
dt <- data.table(hotels)
str(dt)

# How many hotels are from Austria?
dt[country == "Austria"]

# What is the rating of the most expensive hotel (based on the price per night)?
dt[, avg_price_per_night, by = rating]

# How many bookings are in 4-star hotels?
dt$four_stars <- dt$stars == 4.0 & !is.na(dt$stars)
sum(dt$four_stars == "TRUE")

# Which country has the highest number of 5-star hotels? !!!!!

hotels[,sum(stars==5),by=country][order(by=-V1)][1,country]

# Plot the number of bookings per country!
bookings_per_country <- aggregate(bookings ~ country, data = dt, sum)

ggplot(bookings_per_country, aes(x = country, y = bookings)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Number of bookings by country", x = "Country", y = "Number of bookings") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

# Flip the coordinates and use the "classic dark-on-light theme"!
ggplot(bookings_per_country, aes(x = bookings, y = country)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Number of bookings by country", x = "Number of bookings", y = "Country") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Drop the Y axis title, and rename the X axis to "Number of hotels"!
ggplot(bookings_per_country, aes(x = country, y = bookings)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Number of hotels", y = NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

# Count the number of hotels per country!
dt[, .(hotel_count = uniqueN(hotel_id)), by = country]

# Order by alphabet!
dt[, .(hotel_count = uniqueN(hotel_id)), by = country][order(country)]

# Count the number of bookings per country, order by the number of bookings!
bookings_per_country[order(-bookings_per_country$bookings), ]

# Compute the average rating per number of stars!
# Use the weighted.mean function to account for the number of ratings of the hotels, and experiment with the na.rm argument.
# Eliminate NAs. Order by stars.

# Filtering the data.table to drop NA values
dt_filtered <- dt[complete.cases(dt[, c("rating", "rating_count", "stars")]), ]
str(dt_filtered)

# Computing the weighted average
weighted_mean_rating_per_stars <- dt_filtered[, .(weighted_mean_rating = weighted.mean(rating, w = rating_count, na.rm = TRUE)), by = stars]
weighted_mean_rating_per_stars

# Ordering by stars
weighted_mean_rating_per_stars <- weighted_mean_rating_per_stars[order(stars), ]
weighted_mean_rating_per_stars

# Plot this computed average rating per stars!
# Make sure that each star category is printed on the X axis!
ggplot(weighted_mean_rating_per_stars, aes(x = factor(stars), y = weighted_mean_rating)) +
  geom_bar(stat = "identity", fill = "cadetblue3", width = 0.4) +
  labs(title = "Weighted average of ratings by stars", x = "Stars", y = "Weighted mean of ratings") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Create a boxplot on ratings per stars! !!!
ggplot(weighted_mean_rating_per_stars, aes(weighted_mean_rating, group = stars)) + geom_boxplot()

# Create histograms on the nightly prices for each star category! !!!
# Check out the arguments and disable forcing the same Y axis range for the subplots.

# Drop NAs from 'nightly_prices' and 'stars'
dt_filtered_prices <- dt[complete.cases(dt[, c("avg_price_per_night", "stars")]), ]

ggplot(dt_filtered_prices, aes(x = avg_price_per_night)) +
  geom_histogram(binwidth = 10, fill = "cadetblue3", width = 2) +
  facet_wrap(~ stars) +
  labs(title = "Histogram on nightly prices by stars", x = "Nightly prices", y = NULL) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

