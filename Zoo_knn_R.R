set.seed(123)
library(class)
zoo <- read.csv(file.choose())
View(zoo)
d <- data.frame(zoo)
View(d)
head(d)
names(d) <- c("animals", "hair", "feathers", "eggs", "milk", "airborne",
              "aquatic", "predator", "toothed", "backbone", "breathes", "venomous",
              "fins", "legs", "tail", "domestic", "size", "type")
type <- table(d$type)
d_target <- d[,18]
d_key <- d[,1]
d$animals <- NULL
names(type) <- c("mammal", "bird", "reptile", "fish", "amphibian", "insect", "crustacean")
type
summary(d)
str(d)

k = sqrt(17) + 1
k
m1 <- knn.cv(d, d_target, k, prob = TRUE)
prediction <- m1
cmat <- table(d_target,prediction)
acc <- (sum(diag(cmat)) / length(d_target)) * 100
print(acc)

data.frame(type)

