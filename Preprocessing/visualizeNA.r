setwd("~/GitHub/KaggleBNP")

library(readr)

cat("reading data, including train and test\n")
train <- read_csv("train.csv")
test <- read_csv("test.csv")

for (f in names(train)) {
      if (class(train[[f]])=="character") { 
            levels <- unique(c(train[[f]], test[[f]]))
            train[[f]] <- factor(train[[f]], levels=levels)
            test[[f]]  <- factor(test[[f]],  levels=levels)
      }
}

# make a table of missing values
library(mice)
missers <- md.pattern(train[, -c(1:2)])
head(missers)
write_csv(as.data.frame(missers),"NAsTable.csv")


library(VIM)
png(filename="NAsPatternAdj.png",
    type="cairo",
    units="in",
    width=12,
    height=6.5,
    pointsize=10,
    res=300)

miceplot2 <- aggr(train[, -c(1:2)], col=c("dodgerblue","dimgray"),
                  numbers=TRUE, combined=TRUE, varheight=TRUE, border=NA,
                  sortVars=TRUE, sortCombs=FALSE, ylabs=c("Missing Data Pattern w/ Height Adjustment"),
                  labels=names(train[-c(1:2)]), cex.axis=.7)
dev.off()


png(filename="NAsPatternEq2.png",
    type="cairo",
    units="in",
    width=12,
    height=6.5,
    pointsize=10,
    res=300)

miceplot1 <- aggr(train[, -c(1:2)], col=c("dodgerblue","dimgray"),
                  numbers=TRUE, combined=TRUE, varheight=FALSE, border="gray50",
                  sortVars=TRUE, sortCombs=FALSE, ylabs=c("Missing Data Pattern"),
                  labels=names(train[-c(1:2)]), cex.axis=.7)
dev.off()

