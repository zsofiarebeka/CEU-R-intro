library(ggplot2)
library(data.table)
hotels <- readRDS(url('http://bit.ly/CEU-R-hotels-2018-merged'))

# Reading the df
hotels

# How many hotels are from Austria?
hotels[country == "Austria", .N]

# What is the rating of the most expensive hotel (based on the price per night)?
hotels[avg_price_per_night == max(avg_price_per_night) & accommodation_type == "Hotel", rating]

# How many bookings are in 4-star hotels?
hotels$four_stars <- hotels$stars == 4
hotels[four_stars == "TRUE", sum(bookings)]

# Which country has the highest number of 5-star hotels?
hotels[!is.na(stars), .(five_star = sum(stars == 5)), by = country][order(-five_star)][1,country]

# Plot the number of bookings per country!
bookings_counted <- hotels[, .(booking_count = sum(!is.na(bookings))), by = country]
bookings_counted

ggplot(bookings_counted, aes(x = country, y = booking_count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Number of bookings by country", x = "Country") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

# Flip the coordinates and use the "classic dark-on-light theme"!
ggplot(bookings_counted, aes(x = booking_count, y = country)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Number of bookings by country", y = "Country") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Drop the Y axis title, and rename the X axis to "Number of hotels"!
ggplot(bookings_counted, aes(x = booking_count, y = country)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Number of hotels", y = NULL) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Count the number of hotels per country!
hotels[, .(hotel_count = uniqueN(hotel_id)), by = country]

# Order by alphabet!
hotels[, .(hotel_count = uniqueN(hotel_id)), by = country][order(country)]

# Count the number of bookings per country, order by the number of bookings!
bookings_per_country[order(bookings_per_country$bookings), ]

# Compute the average rating per number of stars!
# Use the weighted.mean function to account for the number of ratings of the hotels, and experiment with the na.rm argument.
# Eliminate NAs. Order by stars.

# Filtering the data.table to drop NA values
hotels_filtered <- hotels[complete.cases(hotels[, c("rating", "rating_count", "stars")]), ]

# Computing the weighted average and ordering by stars
weighted_mean_rating_per_stars <- hotels_filtered[, .(weighted_mean_rating = weighted.mean(rating, w = rating_count, na.rm = TRUE)), by = stars][order(stars), ]
weighted_mean_rating_per_stars


# Plot this computed average rating per stars!
ggplot(weighted_mean_rating_per_stars, aes(x = stars, y = weighted_mean_rating)) +
  geom_bar(stat = "identity", fill = "cadetblue3", width = 0.4) +
  labs(title = "Weighted average of ratings by stars", x = "Stars", y = "Weighted mean of ratings") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Make sure that each star category is printed on the X axis!
ggplot(weighted_mean_rating_per_stars, aes(x = factor(stars), y = weighted_mean_rating)) +
  geom_bar(stat = "identity", fill = "cadetblue3", width = 0.7) +
  labs(title = "Weighted average of ratings by stars", x = "Stars", y = "Weighted mean of ratings") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Create a boxplot on ratings per stars!
ggplot(hotels, aes(y = rating, x = factor(stars), group = stars)) +
  geom_boxplot(na.rm = TRUE) +
  labs( x = NULL, y = "rating")

# Create histograms on the nightly prices for each star category!
# Check out the arguments and disable forcing the same Y axis range for the subplots.

ggplot(hotels, aes(x = avg_price_per_night)) +
  geom_histogram() +
  facet_wrap(~ stars, scales = "free") +
  labs(title = "Histogram on nightly prices by stars") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

