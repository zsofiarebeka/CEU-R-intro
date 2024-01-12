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

# How many bookings are in 4-star hotels? !!!!!
dt$four_stars <- dt$stars == 4.0 & !is.na(dt$stars)
sum(dt$four_stars == "TRUE")

# Which country has the highest number of 5-star hotels? !!!!!
dt$five_stars <- dt$stars == 5.0 & !is.na(dt$stars)

five_stars_per_country <- aggregate(five_stars ~ country, data = dt, sum)


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

# Converting the ratings to numeric values
dt$ratings <- as.numeric(dt$ratings)


