getwd()
setwd("/Users/tktran/Documents/hackathon-packet/data")

xtrdata = read_csv('X_train.csv')
ytrdata = read_csv('y_train.csv')
xtedata = read_csv('X_test.csv')


install.packages('randomForest')
library(randomForest) 
xtrdata[is.na(xtrdata)] <- 0

install.packages('ranger')
library(ranger)
rf = ranger(ytrdata~., data = xtrdata, num.trees = 50)

xtedata[is.na(xtedata)] <- 0
p=predict(rf,xtedata)
install.packages("dplyr")
library(dplyr)

xtedata$source = "xtedata"
xtrdata$source = "xtrdata"
xall = bind_rows(xtedata, xtrdata) 
install.packages("readr")
library(readr)
df = mutate_if(xall, is.character, as.factor)
sapply(df, class)
xtedata.2 <- xall[xall$source=="xtedata",]
xtrdata.2 = xall[xall$source == "xtrdata",]

bound = floor((nrow(df)/5)*4)
df = df[sample(nrow(df)),]
df.train = df[1:bound,]
df.test = df[(bound+1):nrow(df),]

xtrdata <- xtrdata %>% mutate_if(is.numeric, function(x) ifelse(is.finite(x), x, 0))
xtedata = xtedata %>% mutate_if(is.numeric, function(x) ifelse(is.finite(x), x, 0))

#xtrdata[!is.finite(unlist(xtrdata))] <- 0
#xtedata[!is.finite(unlist(xtedata))] <- 0

#is.finite(df[])
#true - infinity, false otherwise

trdata = cbind(xtrdata, UNITS=ytrdata$UNITS)
tedata = cbind(xtedata, UNITS=as.numeric(NA))


rf = ranger(UNITS ~., data = trdata, num.trees = 50)

#p = predict(rf, trdata)
p = predict (rf, tedata)
#p = predict(rf, tedata) 
p$pred

df.p <- data.frame(idx=tedata$idx,
                   predictions=p$pred) %>% arrange(idx)
                   
#p1 = getTerminalNodeIDs(rf, tedata)



write_csv(df.p, "/Users/tktran/Documents/hack_2_2.csv")






