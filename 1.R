source('http://bit.ly/CEU-R-shoes')
ls()
str(students)
plot(students$shoe, students$math)
abline(lm(math ~ shoe, students), col = 'red')

plot(students)

library(GGally)
ggpairs(students)

rm(list=ls(all.names= TRUE))
?ls
ls(all.names = TRUE)
?source
readLines('http://bit.ly/CEU-R-shoes')
?detach

## 

download.file('https://bit.ly/hun-cities-distance', 'cities.xls')

library(readxl)
cities <- read_excel('cities.xls')

cities <- cities[, -1]
cities <- cities[-nrow(cities), ]

plot(cities)
library(GGally)
ggpairs(cities)

?cmdscale
mds <- cmdscale(as.dist(cities))
plot(mds)
text(mds[, 1], mds[, 2], names(cities))

mds[, 1] <- -1 * mds[, 1]
plot(mds)
text(mds[, 1], mds[, 2], names(cities))

## TODO redo with ggplot
library(ggplot2)
mds <- data.frame(mds)
str(mds)
mds$city <- names(cities)
str(mds)
ggplot(mds, aes(X1, X2, label = city)) + geom_point() +
  geom_text()

ggplot(mds, aes(X1, X2, label = city)) + geom_text()

library(ggrepel)
ggplot(mds, aes(X1, X2, label = city)) + geom_text_repel()

?eurodist
str(eurodist)


mds <- cmdscale(eurodist)
mds <- data.frame(mds)
mds$city <- row.names(mds)
ggplot(mds, aes(X1, -X2, label = city)) + geom_text_repel()

## TODO same with mtcars
?mtcars

library(ggplot2)
library(ggrepel)
mds <- cmdscale(dist(scale(mtcars)))
mds <- data.frame(mds)
mds$car <- rownames(mtcars)
ggplot(mds, aes(X1, X2, label = car)) + geom_text_repel()

scale(mtcars)


UCBAdmissions

berkeley <- as.data.frame(UCBAdmissions)
ggplot(berkeley, aes(Gender, Freq, fill = Admit)) + 
  geom_col(position = 'fill') + scale_fill_brewer(palette = 'Dark2')

ggplot(berkeley, aes(Gender, Freq, fill = Admit)) + 
  geom_col(position = 'fill') + scale_fill_manual(values = c("Admitted" = 'darkgreen',
                                                             'Rejected' = 'darkred'))

ggplot(berkeley, aes(Gender, Freq, fill = Admit)) + 
  facet_wrap(~Dept) + 
  geom_col() + scale_fill_manual(values = c("Admitted" = 'darkgreen',
                                                             'Rejected' = 'darkred'))

?iris

ggplot(iris, aes(Sepal.Length, Sepal.Width)) + geom_point() +
  geom_smooth(method = 'lm')


library(data.table)
bookings <- fread('http://bit.ly/CEU-R-hotels-2018-prices')
bookings[price < 100 & holiday == 1]

bookings[price < 100][holiday == 1][1:5]

bookings[price < 100 & holiday == 1, .N]
bookings[price < 100 & holiday == 1, mean(price)]
bookings[price < 100 & holiday == 1, summary(price)]
bookings[price < 100 & holiday == 1, hist(price)]

## TODO compute the avg price of bookings on weekends
## TODO compute the avg price of bookings on weekdays

bookings[weekend == 1, mean(price)]
bookings[weekend == 0, mean(price)]

bookings[, mean(price), by = weekend]

bookings$price_per_night <- bookings$price / bookings$nnights
bookings[, price_per_night := price / nnights]

bookings[, .(price = mean(price), min = min(price), max = max(price)), 
         by = .(weekend, nnights, holiday)]

features <- fread('http://bit.ly/CEU-R-hotels-2018-features')
merge(bookings, features, all.x = TRUE)
?merge

merge(bookings, features, all.x = TRUE)[is.na(city)]

features

## TODO country-level aggregated data on avg rating of hotels

countries <- features[, .(rating = mean(rating, na.rm = TRUE)), by = country][!is.na(country)]
setorder(countries, rating)
countries


countries[order(country)]
countries[order(rating)]

countries


library(ggmap)
?geocode
library(tidygeocoder)
?geocode

countries <- data.table(tidygeocoder::geocode(countries, 'country'))

library(maps)
map('world', fill = TRUE, col = 1:10)

world <- map_data('world')
str(world)

map <- ggplot() +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  theme_void() +
  coord_fixed(1.3)

map + geom_point(data = countries, aes(long, lat, size = rating), color = 'orange')

?get_stamenmap

bbox <- c(left = min(countries$long), bottom = min(countries$lat),
          right = max(countries$long), top = max(countries$lat))
get_stamenmap(bbox = bbox, zoom = 4) %>% ggmap() +
  geom_point(aes(x = long, y = lat, size = rating), data = countries, colour = "red") +
  theme_void() +
  coord_fixed(1.3)
str(features)

anscombe
plot(anscombe[, c(1, 5)])
plot(anscombe[, c(2, 6)])

cor(anscombe[, c(1, 5)])
cor(anscombe[, c(2, 6)])

mean(anscombe[, c(1)])
mean(anscombe[, c(2)])
mean(anscombe[, c(3)])

lapply(1:4, function(i) mean(anscombe[, c(i)]))

computemean <- function(i) {
  mean(anscombe[, c(i)])
}
lapply(1:4, computemean)

lapply(1:4, function(i) cor(anscombe[, c(i)], anscombe[, c(i+4)]))

lapply(1:4, function(i) data.frame(x = anscombe[, c(i)], y = anscombe[, c(i+4)]))
rbindlist(lapply(1:4, function(i) data.frame(x = anscombe[, c(i)], y = anscombe[, c(i+4)])))

anscombe_df <- rbindlist(lapply(1:4, function(i) {
  data.frame(
    x = anscombe[, c(i)],
    y = anscombe[, c(i+4)],
    dataset = i)
  }
))

## data.frame with 4x11 rows, 3 columns: x, y, dataset id
ggplot(anscombe_df, aes(x, y)) + geom_point() + 
  geom_smooth(method = 'lm', se = FALSE) + 
  facet_wrap(~dataset) +
  theme_bw()
