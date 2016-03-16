library(corrplot)
library(caret)
temp <- train.num[,-1:-2]
corr.Matrix <- cor(temp, use="pairwise.complete.obs")
corr.75 <- findCorrelation(corr.Matrix, cutoff = 0.75)
train.num.75 <- temp[, corr.75]
corrplot(corr.Matrix, order = "hclust")



# missing.train.num.75 <- apply(train.num.75, 2, function(x) sum(is.na(x))/length(x))
# corr.80 <- findCorrelation(corr.Matrix, cutoff = 0.80)
# corrplot(corr.Matrix, order = "hclust")

temp.scale<- scale(temp,center=TRUE,scale=TRUE)
tempCor <- cor(temp.scale, use="pairwise.complete.obs")
corrplot(tempCor, order = "hclust")

