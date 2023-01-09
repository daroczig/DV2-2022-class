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



