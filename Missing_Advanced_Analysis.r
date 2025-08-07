install.packages("VIM")

df = read.csv("E:\\Rushani\\RUSH RESEARCH\\poverty countries (Autosaved).csv", na="")

head(df)
sum(is.na(df$Environmental)==TRUE)

########### Aggregate plot ######
library(VIM)
aggr_plot <- aggr(pdf, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(pdf), 
                  cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

#### parallel coordinate plots #######
install.packages("missForest")
library(DMwR)
library(naniar)
library(ggplot2)
library(missForest)
pdf = read.csv("E:\\Rushani\\RUSH RESEARCH\\dataset .csv", na ="")
head(pdf)

parcoordMiss(pdf,highlight=("Environmental"), alpha=1,cex.axis =0.7,ylab=c('Environmental'))
parcoordMiss(pdf,highlight=("Literacy"), alpha=1,cex.axis =0.7,ylab=c('Literacy'))
parcoordMiss(pdf,highlight=("Cultural"), alpha=1,cex.axis =0.7,ylab=c('Cultural'))

######## Imputation of data ############
library('mice')
df1 = read.csv("E:\\Rushani\\RUSH RESEARCH\\dataset without country.csv", na="")
head(df1)

imputed_Data <- mice(df1, m=5, maxit = 750, method = 'cart', seed = 500)

completeData <- complete(imputed_Data,2)
cd = data.frame(completeData)
str(cd)

######## WRITING COMPLETE DATA INTO A CSV FILE ####
write.csv(cd,'imputeddata.csv')

df2 = read.csv("E:\\Rushani\\RUSH RESEARCH\\imputeddata.csv", na="")

densityplot(imputed_Data)
sum(is.na(cd$percentage)==TRUE)

############# RANDOM FOREST MODEL  #############

######## train test splitting #########

set.seed(1234)
datasetsize = floor(nrow(df2)*0.80)
index <- sample(1:nrow(df2),size=datasetsize)
traindata <- df2[index,]
testdata <- df2[-index,]


###### model fitiing #####
library(randomForest)
library(caret)
library(e1071)

####### cross validation #########
# Define the control
trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")


# Run the model with default parameters
set.seed(123)
rf_default <- randomForest(percentage~.,
                    data = traindata,
                    metric = "rmse",
                    trControl = trControl,
                    importance = TRUE )
# Print the results
print(rf_default)
plot(rf_default)

# number of trees with lowest MSE
which.min(rf_default$mse)

# RMSE of this optimal random forest
sqrt(rf_default$mse[which.min(rf_default$mse)])

####### prediction on test data #######
preddflt <- predict(rf_default, testdata, type = "response")
preddflt
#accuracy of model11
postResample(preddflt,testdata$percentage)

####### tuning the model ########

# names of features
features <- setdiff(names(traindata), "percentage")

set.seed(123)

m2 <- tuneRF(
  x          = traindata[features],
  y          = traindata$percentage,
  ntreeTry   = 500,
  mtryStart  = 3,
  stepFactor = 0.5,
  improve    = 0.001,
  trace      = TRUE      # to not show real-time progress 
)

####### model with best mtry ###########
set.seed(123)
rf_bstmtry <-randomForest(percentage~.,data=traindata, mtry=3, importance=TRUE,ntree=500)
print(rf_bstmtry)
plot(rf_bstmtry)

# RMSE of this best mtry random forest
sqrt(rf_bstmtry$mse[which.min(rf_bstmtry$mse)])
which.min(rf_bstmtry$mse)

varImpPlot(rf_bstmtry)

####### prediction on test data #######
predmtry <- predict(rf_bstmtry, testdata, type = "response")
predmtry
#accuracy of model11
postResample(predmtry,testdata$percentage)


##### finding best no.of trees #######
store_maxtrees <- list()
tuneGrid <- expand.grid(.mtry = 3)
for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
  set.seed(5678)
  rf_maxtrees <- train(percentage~.,
                       data = traindata,
                       metric = "RMSE",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                        method = 'rf',
                       importance = TRUE,
                       nodesize = 10,
                       maxnodes = 13,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
ntree

####### model with best ntree ########

set.seed(123)
rf_bst <-randomForest(percentage~.,data=traindata, trControl = trControl,mtry=3, importance=TRUE,
                      ntree=2000)
print(rf_bst)
plot(rf_bst)

# RMSE of this best mtry random forest
sqrt(rf_bst$mse[which.min(rf_bst$mse)])

varImpPlot(rf_bst)

####### prediction on test data #######
predbst <- predict(rf_bst, testdata, type = "response")
predbst
pred_bst=data.frame(predbst)
pred_bst
#accuracy of model11
aa = postResample(predbst,testdata$percentage)
ab = data.frame(aa)
ab
testdata= data.frame(testdata)


# Plot the bar chart.
plot(testdata$percentage,type = "l",col = "red", xlab = "Index", ylab = "Percentage", 
     main = "Variation of predicted and actual value plot",legend("topleft",c("Actual values","Predicted values"),
                                                                  fill=c("red","blue")
     ))
lines(pred_bst, type = "l", col = "blue")


######### model with important variables except Unemployment and GDP ##########
library(dplyr)

df3 <-dplyr::select(df2, -c('GDP','Unemployment'))
str(df3)

set.seed(1234)
datasetsize1 = floor(nrow(df3)*0.80)
index1 <- sample(1:nrow(df3),size=datasetsize1)
traindata11 <- df3[index1,]
testdata11 <- df3[-index1,]

######### RF model with important variables ######
set.seed(123)
rf_impt <-randomForest(percentage~.,data=traindata11, trControl = trControl,mtry=3, importance=TRUE,
                       ntree=2000)
print(rf_impt)
plot(rf_impt)


predbstimpt <- predict(rf_impt, testdata11, type = "response")
predbstimpt
pred_bstimpt=data.frame(predbstimpt)
pred_bstimpt
#accuracy of model with important variables
aa = postResample(predbstimpt,testdata11$percentage)
aa

# Plot the bar chart of model with important variables.
plot(testdata11$percentage,type = "l",col = "red", xlab = "Index", ylab = "Percentage", 
     main = "Variation of predicted and actual value plot")

lines(pred_bstimpt, type = "l", col = "blue")

######### model with important variables except Unemployment and GDP ##########
library(dplyr)

df4 <-dplyr::select(df2, -c('GDP','Unemployment'))
str(df3)
df4 = select(df2,c('percentage','Environmental','Literacy','Health','Agriculture'))
head(df4)

set.seed(1234)
datasetsize2 = floor(nrow(df4)*0.80)
index1 <- sample(1:nrow(df4),size=datasetsize2)
traindata12 <- df4[index1,]
testdata12 <- df4[-index1,]

######### RF model with important variables ######
set.seed(123)
rf_imp2 <-randomForest(percentage~.,data=traindata12, trControl = trControl,mtry=3, importance=TRUE,
                       ntree=500)
print(rf_imp2)
plot(rf_impt)


predbstimpt2 <- predict(rf_imp2, testdata12, type = "response")
predbstimpt2
pred_bstimpt2=data.frame(predbstimpt2)
pred_bstimpt
#accuracy of model with important variables
aa = postResample(pred_bstimpt2,testdata12$percentage)
aa

# Plot the bar chart of model with important variables.
plot(testdata11$percentage,type = "l",col = "red", xlab = "Index", ylab = "Percentage", 
     main = "Variation of predicted and actual value plot")

lines(pred_bstimpt, type = "l", col = "blue")


################### ############# XG BOOST REGRESSION ###############################

install.packages('xgboost')
library(xgboost)
library(caret)

train_x = data.matrix(traindata[,-1])
train_y = traindata[,1]

test_x = data.matrix(testdata[, -1])
test_y = testdata[, 1]


xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

########## DEFAULT MODEL #########
xgbc = xgboost(data = xgb_train, max.depth = 8, nrounds = 200)
print(xgbc)

pred_y = predict(xgbc, xgb_test)
plot(xgbc)

postResample(test_y,pred_y)
mse = mean((test_y - pred_y)^2)
mae = caret::MAE(test_y, pred_y)
rmse = caret::RMSE(test_y, pred_y)

cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)

x = 1:length(test_y)
plot(x, test_y, col = "red", type = "l")
lines(x, pred_y, col = "blue", type = "l")
legend(x = 1, y = 38,  legend = c("original test_y", "predicted test_y"), 
       col = c("red", "blue"), box.lty = 1, cex = 0.8, lty = c(1, 1))

# Compute feature importance matrix
mat <- xgb.importance (feature_names = colnames(train_x),model = xgbc)
xgb.plot.importance (importance_matrix = mat[1:12]) 



########### PARAMETER TUNING ##############

library(ggplot2)
library(reshape2)
library(Ecdat)
set.seed(1)
N = 1000
k = 10
x = matrix(rnorm(N*k),N,k)
b = (-1)^(1:k)
yaux=(x%*%b)^2
e = rnorm(N)
y=yaux+e
# = select train and test indexes = #
train=sample(1:N,800)
test=setdiff(1:N,train)
# = parameters = #
# = eta candidates = #
eta=c(0.05,0.1,0.2,0.5,1)
# = colsample_bylevel candidates = #
cs=c(1/3,2/3,1)
# = max_depth candidates = #
md=c(2,4,6,10)
# = sub_sample candidates = #
ss=c(0.25,0.5,0.75,1)
# = standard model is the second value  of each vector above = #
standard=c(2,2,3,2)

########### tuning max depth ##########

set.seed(1)
conv_md=matrix(NA,500,length(md))
pred_md=matrix(NA,length(test_x),length(md))
colnames(conv_md)=colnames(pred_md)=md
for(i in 1:length(md)){
  params=list(eta=eta[standard[1]],colsample_bylevel=cs[standard[2]],
              subsample=ss[standard[4]],max_depth=md[i],
              min_child_weigth=1)
  xgb=xgboost(train_x, label = train_y,nrounds = 500,params=params)
  conv_md[,i] = xgb$evaluation_log$train_rmse
  pred_md[,i] = predict(xgb,test_x)
}
conv_md=data.frame(iter=1:500,conv_md)
conv_md=melt(conv_md,id.vars = "iter")
ggplot(data=conv_md)+geom_line(aes(x=iter,y=value,color=variable))


########## tuning eta ###########
set.seed(1)
conv_eta = matrix(NA,500,length(eta))
pred_eta = matrix(NA,length(test_x), length(eta))
colnames(conv_eta) = colnames(pred_eta) = eta
for(i in 1:length(eta)){
  params=list(eta = eta[i], colsample_bylevel=cs[standard[2]],
              subsample = ss[standard[4]], max_depth = md[standard[3]],
              min_child_weigth = 1)
  xgb=xgboost(train_x, label = train_y, nrounds = 500, params = params)
  conv_eta[,i] = xgb$evaluation_log$train_rmse
  pred_eta[,i] = predict(xgb, test_x)
}
conv_eta = data.frame(iter=1:500, conv_eta)
conv_eta = melt(conv_eta, id.vars = "iter")
ggplot(data = conv_eta) + geom_line(aes(x = iter, y = value, color = variable))



############# tuning colsample_bylevel #########
set.seed(1)
conv_cs = matrix(NA,500,length(cs))
pred_cs = matrix(NA,length(test_x), length(cs))
colnames(conv_cs) = colnames(pred_cs) = cs
for(i in 1:length(cs)){
  params = list(eta = eta[standard[1]], colsample_bylevel = cs[i],
                subsample = ss[standard[4]], max_depth = md[standard[3]],
                min_child_weigth = 1)
  xgb=xgboost(train_x, label = train_y,nrounds = 500, params = params)
  conv_cs[,i] = xgb$evaluation_log$train_rmse
  pred_cs[,i] = predict(xgb,test_x)
}
conv_cs = data.frame(iter=1:500, conv_cs)
conv_cs = melt(conv_cs, id.vars = "iter")
ggplot(data = conv_cs) + geom_line(aes(x = iter, y = value, color = variable))

########## MODEL WITH PARAMETERS TUNED ########

xgb1 = xgboost(data = xgb_train, max.depth = 4, nrounds = 200,eta = 0.05)
print(xgb1)

pred_y1 = predict(xgb1, xgb_test)

postResample(test_y,pred_y1)
mse1 = mean((test_y - pred_y1)^2)
mae1 = caret::MAE(test_y, pred_y1)
rmse1 = caret::RMSE(test_y, pred_y1)
cat("MSE: ", mse1, "MAE: ", mae1, " RMSE: ", rmse1)

x = 1:length(test_y)
plot(x, test_y, col = "red", type = "l")
lines(x, pred_y1, col = "blue", type = "l")
legend(x = 1, y = 38,  legend = c("original test_y", "predicted test_y"), 
       col = c("red", "blue"), box.lty = 1, cex = 0.8, lty = c(1, 1))

# Compute feature importance matrix
mat <- xgb.importance (feature_names = colnames(train_x),model = xgb1)
xgb.plot.importance (importance_matrix = mat[1:12]) 

# Plot the bar chart.
plot(testdata$percentage,type = "l",col = "red", xlab = "Index", ylab = "Percentage", 
     main = "Variation of predicted and actual value plot")

lines(pred_y1, type = "l", col = "blue")





