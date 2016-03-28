library(readr)
setwd("~/GitHub/KaggleBNP")
train <- read_csv("train.csv")

train <- train[, -1]

active <- as.data.frame(train[1, ])
passive <- as.data.frame(train[1, ])
for (i in 1:114321) {
      if (train[i, ]$target == 1) { # complete observation
            active <- rbind(active, train[i, ])
      } else { # incomplete observation
            passive <- rbind(passive, train[i, ])
      }     
}

