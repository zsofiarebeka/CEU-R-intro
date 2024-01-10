x <- seq(from = 0, to = 20, by = 0.01)
plot(x, sin(x), type = "l")


curve(sin, from = 0, to = pi * 2)
curve(cos, from = 0, to = pi * 2, add = TRUE, col = 'red')

# TODO Brownian motion / random walk
x <- 0
## TODO 100 iterations
for (i in 1:100) {
  if (runif(1) < 0.5) {
  x <- x + 1
  } else {
    x <- x-1
  }
}
x

## vectorize
set.seed(42)
cumsum(round(runif(100)) * 2 -1)
?set.seed

h <- c(174, 170, 160)
w <- c(90, 80, 70)

min(w)
max(w)
range(w)
diff(range(w))
mean(w)
median(w)
sum(w)
summary(w)

str(summary(w))

cor(w, h)
lm(w ~ h)
?lm

## let's see predicted w for someone 165cm
-146.154 + 165 * 1.346
## we predicted that a 165 cm tall person will likely to have 76 kg

# putting this into a variable
fit <- lm(w ~ h)
str(fit)
summary(fit)
# summary is a better function cause it works like stargazer

predict(fit, newdata = list(h = 165))
## it will do the calculation for us what we typed it manually

predict(fit, newdata = list(h = 52))

# Plotting our model
# abline is adding a straight line to the plot
plot(h, w)
abline(fit, col = "red")


# Let's create out first Dataframe
# weight = w is column name = column values
df <- data.frame(weight = w, height = h)
df
str(df)
# we use $ to refer to the column of the data frames
df$weight

# accessing the first element of the column
# in R, we don't use [0] but [1] as referring to the first element
df$weight[1]

# [row, column]
df[1,1]
df [2, 1]

nrow(df)
ncol(df)
dim(df)

plot(df)
cor(df)

## compute BMI: weight/height^2 in meters
df$bmi <- df$weight / (df$height/100)^2
df

# reading the .csv file
df <- read.csv("http://bit.ly/CEU-R-heights")
df

# Computing BMI in the new df
df$bmi <- (df$weightLb*0.4536) / (df$heightIn*0.0254)^2
df

# Even if we use weight or w, R will use partial matching if we use [] for columns
# But it's better if we use $ and add the full name of the column

df$weight <- df$weightLb * 0.45
df$height <- df$heightIn * 2.54
df$weightLb <- df$heightIn <- NULL
df$bmi <- df$weight / (df$height/100) ^ 2
df
# plotting the fit df
plot(df)

install.packages("pairsD3")

library(pairsD3)  #inline comment
# double :: means calling the functions of the package
pairsD3::pairsD3(df)


install.packages("GGally")
library(GGally)
ggpairs(df)
# This plotting is useful, but it's quite slow

# Using ggplot2 in R
library(ggplot2)
ggplot(df, aes(x = height)) + geom_histogram()
ggplot(df, aes(x = height, y = weight)) + geom_point()
system.time(g <- ggplot(df, aes(x = height, y = weight, color = sex)) + geom_point())
system.time(print(g))

# Themes and Standard Errors
# If we take the lm out --> creates a lowess
g + theme_bw()
g + geom_smooth(method = 'lm', se = TRUE)

# We get a line for male, female and both
ggplot(df, aes(x = height, y = weight)) +
  geom_point(aes(color = sex)) +
  geom_smooth(method = 'lm', se = FALSE, color = "black") +
  geom_smooth(aes(color = sex), method = 'lm', se = FALSE)

# Scale definitions
g + scale_y_log10()
# not possible to add secondary y axis


# box plots
ggplot(df, aes(x = height)) + geom_boxplot()
# separating based on categories
ggplot(df, aes(sex,height)) + geom_boxplot()
# violin plot
ggplot(df, aes(sex,height)) + geom_violin()

