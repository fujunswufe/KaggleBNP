setwd("~/GitHub/KaggleBNP")

library(readr)
cat("reading data, including train and test\n")
train <- read_csv("train.csv")
test <- read_csv("test.csv")

train$target <- as.factor(train$target)
target <- train$target
target <- as.data.frame(target)

train <- train[, -c(1:2)]
test <- test[, -1]

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
corr.75 <- findCorrelation(corr.Matrix, cutoff = 0.75)
train.num.75 <- train.num[, -corr.75]
corrplot(corr.Matrix, order = "hclust")

# corr.85 <- findCorrelation(corr.Matrix, cutoff = 0.85)
# train.num.85 <- train.num[, -corr.85]

# corr.90 <- findCorrelation(corr.Matrix, cutoff = 0.90)
# train.num.90 <- train.num[, -corr.90]

cat("imputation on missing value")
train.num.75[is.na(train.num.75)] <- -1
train1 <- cbind(train.num.75, train.char)
train1 <- cbind(train1, target)
train1[is.na(train1)] <- -1

library(FSelector)
weights <- symmetrical.uncertainty(target~., train1)
print(weights)
subset <- cutoff.k(weights, 50)
train.clean <- train1[, subset]
train.clean <- cbind(train.clean, target)



cat("write cleaning files to disk")
write.csv(train.clean, "train_clean.csv", row.names = F)
saveRDS(train.clean, "train_clean.rds")



# subset <- cutoff.biggest.diff(weights)
# f <- as.simple.formula(subset, "target")
# print(f)






