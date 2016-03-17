# this preprocessing pipeline is for imputating missing value to -1 
# however, this is a naive way

setwd("~/GitHub/KaggleBNP")

library(readr)
cat("reading data, including train and test\n")
train <- read_csv("train.csv")
test <- read_csv("test.csv")

train$target <- as.factor(train$target)
target <- train$target
target <- as.data.frame(target)

cat("remove ID and target variable ")
train <- train[, -c(1:2)]
test <- test[, -1]

cat("missing value count per observation as a predictor")
count.missing.train <- apply(train, 1, function(x) sum(is.na(x)))
count.missing.test <- apply(test, 1, function(x) sum(is.na(x)))
count.missing.train <- as.data.frame(count.missing.train)
count.missing.test <- as.data.frame(count.missing.test)

cat("check missing value percentile\n")
mean(is.na(train)) ## overall missing value
mean(is.na(test))
missing_train <- apply(train, 2, function(x) sum(is.na(x))/length(x))
missing_test <- apply(test, 2, function(x) sum(is.na(x))/length(x))

cat("remove constant\n")
col.const <- sapply(train, function(x) length(unique(x))==1)
table(col.const) # no constant columns 
rm(col.const)

cat("remove duplicated columns\n")
table(duplicated(as.list(train))) # no duplicated columns 

cat("separate numeric and non numeric columns\n")
train.num <- train[, sapply(train, is.numeric)]
train.char <- train[, sapply(train, is.character)]
test.num <- test[, sapply(test, is.numeric)]
test.char <- test[, sapply(test, is.character)]

cat("convert categorical to numeric")
for (f in names(train.char)) {
      if (class(train.char[[f]])=="character") { 
            levels <- unique(c(train.char[[f]], test.char[[f]]))
            train.char[[f]] <- as.integer(factor(train.char[[f]], levels=levels))
            test.char[[f]]  <- as.integer(factor(test.char[[f]],  levels=levels))
      }
}
rm(f, levels)

cat("find and remove redundant variables for numeric")
library(corrplot)
library(caret)
corr.Matrix <- cor(train.num, use="pairwise.complete.obs")

corr.80 <- findCorrelation(corr.Matrix, cutoff = 0.80)
train.num.80 <- train.num[, -corr.80]
corrplot(corr.Matrix, order = "hclust")

corr.90 <- findCorrelation(corr.Matrix, cutoff = 0.90)
train.num.90 <- train.num[, -corr.90]

cat("imputation on missing value")
# train.num.80[is.na(train.num.80)] <- -1
train1 <- cbind(train.num.80, train.char)
train1 <- cbind(train1, target)
train1[is.na(train1)] <- -1

# train.num.90[is.na(train.num.90)] <- -1
train2 <- cbind(train.num.90, train.char)
train2 <- cbind(train2, target)
train2[is.na(train2)] <- -1

cat("feature ranking, including correlation and entropy based")
library(FSelector)
weights <- symmetrical.uncertainty(target~., train1)
print(weights)
subset <- cutoff.k(weights, 50)
train.clean1 <- train1[, subset]

weights2 <- symmetrical.uncertainty(target~., train2)
print(weights2)
subset2 <- cutoff.k(weights2, 60)
train.clean2 <- train2[, subset2]

test[is.na(test)] <- -1 
test.clean.80 <- test[, colnames(train.clean1)]
test.clean.80 <- cbind(test.clean.80, count.missing.test)

test.clean.90 <- test[, colnames(train.clean2)]
test.clean.90 <- cbind(test.clean.90, count.missing.test)

train.clean1 <- cbind(train.clean, target)
train.clean1 <- cbind(train.clean1, count.missing.train)

train.clean2 <- cbind(train.clean2, target)
train.clean2 <- cbind(train.clean2, count.missing.train)


cat("write cleaning files to disk")
write.csv(train.clean1, "train_clean1.csv", row.names = F)
saveRDS(train.clean1, "train_clean1.rds")

write.csv(train.clean2, "train_clean2.csv", row.names = F)
saveRDS(train.clean2, "train_clean2.rds")

write.csv(test.clean.80, "test_clean1.csv", row.names = F)
saveRDS(test.clean.80, "test_clean1.rds")

write.csv(test.clean.90, "test_clean2.csv", row.names = F)
saveRDS(test.clean.90, "test_clean2.rds")








