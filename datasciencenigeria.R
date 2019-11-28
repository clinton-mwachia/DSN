library(readr)
train_perf = read_csv('../input/trainperf.csv')
str(train_perf)
summary(train_perf)
train_perf = train_perf[,-c(1,4:5,9)]
anyNA(train_perf)
test_perf = read_csv('../input/testperf.csv')
anyNA(test_perf)
str(test_perf)
customer_id = test_perf$customerid
test_perf = test_perf[,-c(1,4:5,9)]

anyNA(train_perf)
anyNA(test_perf)
train_perf$good_bad_flag = as.factor(train_perf$good_bad_flag)
str(train_perf)
str(test_perf)

attach(train_perf)
library(ggplot2)
p = ggplot(data = train_perf, aes(x=good_bad_flag))
p = p + geom_histogram(stat = 'count', fill=c('green','blue'))
p

tb = table(good_bad_flag, loanamount)
tb

str(train_perf)
train_perf$good_bad_flag = ifelse(train_perf$good_bad_flag=='Good',1,0)
str(train_perf)
# random forest model
train_perf$good_bad_flag = as.factor(train_perf$good_bad_flag)
library(randomForest)

library(caret)
control = trainControl(method='cv',number=10)
model = train(good_bad_flag~., data=train_perf, trControl=control, method='rf', metric='Accuracy', preProcess='scale')
model
RF = randomForest(good_bad_flag~., data = train_perf,mtry=2, ntree=60, kappa=0.03153118)
RF

Good_Bad_flag = predict(RF, newdata = test_perf)
Good_Bad_flag

submit = cbind(customer_id,Good_Bad_flag)
write.csv(submit, 'submit.csv')


control = trainControl(method='cv',number=10)
model1 = train(good_bad_flag~., data=train_perf, trControl=control, metric='Accuracy', method='cforest')
model1

library(party)
CF = cforest(good_bad_flag~., data = train_perf, control=cforest_unbiased(mtry=5))
CF
Good_Bad_flag1 = predict(CF, newdata = test_perf)
Good_Bad_flag1

submit = cbind(customer_id,Good_Bad_flag1)
write.csv(submit, 'submit.csv')

