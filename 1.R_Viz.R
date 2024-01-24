install.packages("remotes")
library(remotes)
install_github('daroczig/students', force = 'TRUE') #CRAN
# CRAN is safe
# never install from untrusted sources
library(students)

?students
str(students)

# Creating charts to visualize correlation
ggplot(students, aes(x = math, y = shoe)) + geom_point() +
  abline(fit)

# Linear model
# we can specify what we would like to predict based on what variable
fit <- lm(math ~ shoe, data = students)
# we can also add further variables
fit2 <- lm(math ~ shoe + x, data = students)
summary(fit)
summary(fit2)
plot(fit)
plot(fit2)

# Visualizations (Base R way)
plot(students$shoe, students$math)
abline(fit, col = "red")

## Visualization (ggplot way)
library(ggplot2)
ggplot(students, aes(x = x, y = math)) + geom_point() + geom_smooth(method = 'lm')

# Partial correlation (with a conditioning variable) and residuals
residuals(lm(math ~ x, data = students))
residuals(lm(shoe ~ x, data = students))

cor(residuals(lm(math ~ x, data = students)),
    residuals(lm(shoe ~ x, data = students)))


# Correlation calculation
install.packages("psych")
library(psych)

?partial.r
partial.r(students, 1:2, 3)

plot(students)

#GGally package
library(GGally)
ggpairs(students)

# gt is a shorthand for grammar tables
install.packages ("gtExtras")
library(gtExtras)

install.packages("svglite")
library(svglite)

# Visualizes the basic metrics of each variables
gt_plt_summary(students)

.secret # comes from the loaded students dataset

#### New dataset: distances in Germany ####


# wb is the binary way of donwloading it --> some machines require the mode
download.file("https://bit.ly/de-cities-distance", 'cities.xls', mode = 'wb')

# the fread has a limitation of being unable to handle excel files
# distance <- fread("https://bit.ly/de-cities-distance")

install.packages("readxl")
library(readxl)

cities <- read_excel("cities.xls")

str(cities)

# dropped the first column
cities <- cities[, -1]
str(cities)

## TODO dropped the last 3 rows
# creating a seq for the rows we would like to keep and extracting 3 rows from it
cities <- cities[1:(nrow(cities)-3), ]
str(cities)
plot(cities)

#### let's move the 15 dimension to 2 ####
## PCA
## or MDS (multidimensional scaling)

?cmdscale
# d is the distance matrix, which we have but R doesn't know
# so we have to use the as.dist matrix

mds <- cmdscale(as.dist(cities))
mds
plot(mds)
# we are referring to the column names of the cities
text(mds[, 1], mds[, 2], names(cities))

# Getting the map of Germany
# we have to run the plot() and the text() too every time
mds <- -mds
plot(mds)
text(mds[, 1], mds[, 2], names(cities))
mds[, 1] <- mds[, 1] * -1

#### let's plot it in ggplot ####
ggplot(mds, aes(x, y)) + geom_scatter()

# We have to transform it to a df
str(mds)
str(as.data.frame(mds))
mds <- as.data.frame(mds)
mds$city <- rownames(mds)
mds$city

ggplot(mds, aes(V1, V2)) + geom_point()
?geom_text

# Customizing and adding an extra layer
ggplot(mds, aes(V1, V2)) + geom_point() + geom_text(aes(label = ))

# We are trying to rename the dots to names and the city names are the indexes = rownames

rownames(mds)
ggplot(mds, aes(V1, V2)) + geom_point() + geom_text(aes(label = city))

# Removing the dots from the scatter plot
ggplot(mds, aes(V1, V2, label = city)) + geom_text() + theme_void()

## TODO Do the same with European cities
?eurodist
str(eurodist)

mds <- cmdscale(as.dist(eurodist))
plot(mds)

# Flipping it
mds[, 2] <- mds[, 2] * -1

mds <- as.data.frame(mds)
str(mds)
mds$city <- rownames(mds)
mds$city

ggplot(mds, aes(V1, V2, label = city)) + geom_text() + theme_void()

# even with ggplot, we can do flips (putting the '-' sign in front of V2)
# ggplot(mds, aes(V1, -V2, label = city)) + geom_text() + theme_void()


#### CREATING ACTUAL MAPS ####
install.packages("ggmap")
library (ggmap)
install.packages("tidygeocoder")
library(tidygeocoder)
geocode(mds, 'city')

library(data.table)
mds <- data.table(geocode(mds, 'city'))
ggplot(mds, aes(long ,lat, label = city)) + geom_text() + theme_void()

# Adding a background
install.packages("maps")
library("maps")

?maps::world
world <- map_data('world')

# We can specify the region and setting the coordinates so it doesn't transform
ggplot() +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  coord_fixed(1.3)

# Adding the points
ggplot() +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  geom_point(data = mds, aes(x = long,y = lat), color = 'orange') +
  coord_fixed(1.3)

# Coloring the regions
# grepl-> returning a boolean
# ^ is the beginning of a name
world$a <- grepl('^A', world$region)

ggplot() +
  geom_map(data = world, map = world, aes(long, lat, map_id = region, fill = a)) +
  geom_point(data = mds, aes(x = long,y = lat), color = 'orange') +
  coord_fixed(1.3)

# Coloring the dots black

ggplot() +
  geom_map(data = world, map = world, aes(long, lat, map_id = region, fill = a)) +
  geom_point(data = mds, aes(x = long,y = lat), color = 'black') +
  coord_fixed(1.3) + theme_void() + theme(legend.position = 'none')

library(ggmap)
register_stadiamaps('e42f09c3-95fb-4a74-a76e-7267cfd45f20')

# Creating the bounding box (vectors) so the coordinates are more visible
?get_stadiamap
map <- get_stadiamap(
  c(
    left = min(mds$long),
    right = max(mds$long),
    top = max(mds$lat),
    bottom = min(mds$lat)
  ),
  zoom = 4,
  maptype = 'stamen_toner'
)

str(map)

# Setting the coordinates of the map
ggmap(map) +
  geom_point(data = mds, aes(x = long,y = lat), color = 'orange')

# Spatial objects (sf = simple features)
install.packages("sf")
library(sf)
# the last two columns are standing for the geocodes
# original mds 
geomds <- st_as_sf(x = mds, coords = c("long", "lat"))
st_bbox(geomds)

# Names are not matching with the bounding box labels

map <- get_stadiamap(
  st_bbox(geomds),
  zoom = 4,
  maptype = "stamen_toner"
)
# The issue is usually due to missing values (NA)

unname(st_bbox(geomds))

map <- get_stadiamap(
  unname(st_bbox(geomds)),
  zoom = 4,
  maptype = "stamen_toner"
)

# GIS info on Austria
download.file(
  'https://stacks.stanford.edu/file/druid:rc343vz5889/data.zip',
  'Austria_boundary.zip')
download.file(
  'https://stacks.stanford.edu/file/druid:yv617vc9132/data.zip',
  'Austria_divisions.zip')
unzip('Austria_boundary.zip')
unzip('Austria_divisions.zip')

# calling the helper and calling from the current directory
library(sf)
st_layers('.')

adm0 <- st_read('.', layer = "AUT_adm0")
plot(adm0)

adm2 <- st_read('.', layer = "AUT_adm2")
plot(adm2)

cities <- fread('https://simplemaps.com/static/data/country-cities/at/at.csv')

# Mapping the cities with ggplot
ggplot() +
  geom_sf(data = adm0, color = "black", fill = "black") +
  geom_sf(data = adm2, color = "black", aes(fill = TYPE_2)) +
  geom_point(data = cities, aes(lng, lat, size = population)) +
  theme_void() + theme(legend.position = "top")


## geojson
download.file(
  'https://raw.githubusercontent.com/ginseng666/GeoJSON-TopoJSON-Austria/master/2021/simplified-95/bezirke_95_topo.json',
  'austria.geojson')

# JavaScript
install.packages("leaflet")
library(leaflet)

map <- st_read('austria.geojson')
plot(map)
leaflet(map)

# Specifying how to add polynoms
popup <- paste0('<strong>Name: </strong>', map$name)
leaflet(map) %>%
  addPolygons(
    weight = 1, smoothFactor = 0.5,
    opacity = 1.0, fillOpacity = 0.5,
    popup = popup,
    label = ~name,
    layerId = ~name,
    labelOptions = labelOptions(noHide = TRUE),
    highlightOptions = highlightOptions(
      color = 'white', weight = 2,
      bringToFront = TRUE))

ggplot() +
  geom_sf(data =map)


#### Back to the MDS object ####

mtcars
?cmdscale
# for cmdscal, it requires a distance matrix
?dist
# in the distance matrix, we compared the cars how similar they are to each other based on the variables
# independent of the nr of variables
dist(mtcars)
# 32 x 32 matrix as a data.frame
mds <- as.data.frame(cmdscale(dist(scale(mtcars))))
mds$car <- rownames(mtcars)
mds

ggplot(mds, aes(V1, V2, label = car)) + geom_text()

# Fixing the chart - solves the text overlap
install.packages("ggrepel")
library(ggrepel)
library(ggplot2)

ggplot(mds, aes(V1, V2, label = car)) + geom_text_repel()

# how mds came up with this plot
# we should scale our variables first --> as.data.frame(cmdscale(dist(scale(mtcars))))
# we should standardize all our variables before 


#### Simpson's paradox ####

str(UCBAdmissions)
plot(UCBAdmissions)
berkeley <- as.data.frame(UCBAdmissions)
berkeley

# Plotting the Berkeley, we must add the stats = "identity"
# bar counts the number of rows, and stats doesn't do any transformations but it will use the data we provide in the column
ggplot(berkeley, aes(x = Gender, y = Freq, fill = Admit)) +
  geom_col(stat = "identity")

# Position
ggplot(berkeley, aes(x = Gender, y = Freq, fill = Admit)) +
  geom_col(position = "fill")

# Changing the colors of this plot
ggplot(berkeley, aes(x = Gender, y = Freq, fill = Admit)) +
  geom_col(position = "fill") +
  scale_fill_manual(values=c('Admitted' = 'darkgreen',
                              'Rejected' = 'darkred'))

# Doing another split to see the departments
ggplot(berkeley, aes(x = Gender, y = Freq, fill = Admit)) +
  geom_col(position = "fill") +
  scale_fill_manual(values=c('Admitted' = 'darkgreen',
                             'Rejected' = 'darkred')) +
  facet_wrap(~Dept)

#### IRIS dataset ####

## TODO scateerplot on Sepal.Length ~ Sepal.Width + lm
# Instead of creating a fit --> geom_smooth(method = "lm")

ggplot(iris, aes(x = Sepal.Length,y = Sepal.Width, color = Species)) +
  geom_point() +
  geom_smooth(method = 'lm')

summary(lm(Sepal.Width ~ Sepal.Length, iris))
summary(lm(Sepal.Width ~ Sepal.Length + Species, iris))

# All 4 models rendered on the same plot (the 3 species and the one for the whole dataset)
ggplot(iris, aes(x = Sepal.Length,y = Sepal.Width)) +
  geom_point(aes(color = Species)) +
  geom_smooth(aes(color = Species), method = 'lm', se = FALSE) +
  geom_smooth(method = 'lm', color = "black", se = FALSE)

## MDS

mds <- as.data.frame(cmdscale(dist(iris[, 1:4])))
mds
mds$Species <- iris$Species
ggplot(mds, aes(x = V1,y = V2)) +
  geom_point(aes(color = Species)) +
  geom_smooth(aes(color = Species), method = 'lm', se = FALSE) +
  geom_smooth(method = 'lm', color = "black", se = FALSE)

#### Anscombe's quartet ####

# Data transformation
df <- data.frame(x = anscombe$x1, y = anscombe$y1)

# lapply will run a list for you

lapply(1:8, function(i) (anscombe[, c(i)]))

lapply(1:4, function(i) (anscombe[, c(i, i+4)]))
# Now we have 4 dataframes
lapply(1:4, function(i) data.frame(x = anscombe[, i], y = anscombe[, i+4]))

# We need one dataframe:
df <- rbindlist(lapply(1:4, function(i) data.frame(
  x = anscombe[, i], 
  y = anscombe[, i+4], 
  type = i)))


## Alternative:
library(data.table)
ggplot(df, aes(x, y)) + geom_point() + facet_wrap(~type, scales = "free") +
  geom_smooth(method = "lm")

#### DatasauRus ####
install.packages("datasauRus")
library(datasauRus)

datasaurus_dozen_wide

# we have to specify a flag: drop = TRUE so it drops the first name
?sub

df <- rbindlist(lapply(seq(1, 25, by = 2), function(i) data.frame(
  x = datasaurus_dozen_wide[, i, drop = TRUE], 
  y = datasaurus_dozen_wide[, i+1, drop = TRUE], 
  type = sub('_x$', '', names(datasaurus_dozen_wide)[i]))))

ggplot(df, aes(x, y)) + geom_point() + facet_wrap(~type) +
  geom_smooth(method = "lm")

# This is not a df, but a tibble which acts different --> takes up the name
# of the first two columns