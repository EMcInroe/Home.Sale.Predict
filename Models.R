library(caret)
library(stringr)
library(reshape2)
library(tree)
library(dplyr)
library(readr)
library(randomForest)

train_clean <- read_csv("C:/Users/eudoc/Documents/Statistics/STAT580/Project2/train_clean.csv")
train_dummy <- read_csv("C:/Users/eudoc/Documents/Statistics/STAT580/Project2/train_dummy.csv")
train_tran <- read_csv("C:/Users/eudoc/Documents/Statistics/STAT580/Project2/train_tran.csv")
test_dummy <- read_csv("C:/Users/eudoc/Documents/Statistics/STAT580/Project2/test_dummy.csv")

train_tran <- train_tran[ , ! names(train_tran) %in% c("ID","uniqueID")]
train_dummy <- train_dummy[ , ! names(train_dummy) %in% c("ID","uniqueID")]
train_clean <- train_clean[ , ! names(train_clean) %in% c("ID","uniqueID")]
test_dummy <- test_dummy[ , ! names(test_dummy) %in% c("ID","House","SalePrice")]

#Remove house due to errors in data
train_dummy <- subset(train_dummy, SalePrice != 475000)
train_clean <- subset(train_clean, SalePrice != 475000)
train_tran <- subset(train_tran, SalePrice != 475000)

train_dummy<- subset(train_dummy, House!="Edwards65")
train_clean<-subset(train_clean, House!="Edwards65")
train_tran<-subset(train_tran, House!="Edwards65")

p.cor<-cor(train_dummy[sapply(train_dummy, is.numeric)])
corrplot.mixed(p.cor)

#Set Training and Tests Data
set.seed(36)
sale_sample_vector <- createDataPartition(train_dummy$SalePrice, p = 0.8, list = FALSE)
sale_test<-train_dummy[-sale_sample_vector,]
sale_train<-train_dummy[sale_sample_vector,]

set.seed(567)
sale_sample_vector <- createDataPartition(train_clean$SalePrice, p = 0.8, list = FALSE)
sale_test2<-train_clean[-sale_sample_vector,]
sale_train2<-train_clean[sale_sample_vector,]

set.seed(5)
sale_sample_vector3 <- createDataPartition(train_tran$SalePrice, p = 0.8, list = FALSE)
sale_test3<-train_tran[-sale_sample_vector3,]
sale_train3<-train_tran[sale_sample_vector3,]

#Dumbest Models

model1<-lm(SalePrice~OverallQual, data=sale_train)
pred_mod1<-predict(model1, newdata=sale_test)
mean((pred_mod1-sale_test$SalePrice)^2)

model2<-lm(SalePrice~YrSold, data=sale_train)
pred_mod2<-predict(model2, newdata=sale_test)
mean((pred_mod2-sale_test$SalePrice)^2)

#Regression Tree
tree_sale<-tree(formula=SalePrice~.,sale_train, na.action = "na.pass")
summary(tree_sale)
plot(tree_sale)
text(tree_sale, pretty=0)
cv.sale<-cv.tree(tree_sale)
plot(cv.sale$size, cv.sale$dev, type='b')

yhat<-predict(tree_sale,newdata=sale_test)
plot(yhat,sale_test$SalePrice)
abline(0,1)
mean((yhat-sale_test$SalePrice)^2)

tree_sale_dum<-tree(formula=SalePrice~.,train_dummy, na.action = "na.pass")
summary(tree_sale_dum)
plot(tree_sale_dum)
text(tree_sale_dum, pretty=0)


#Random Forest
set.seed(2)
rf.sale<-randomForest(SalePrice~., data=sale_train, mtry = 11, importance=TRUE)
yhat.rf<-predict(rf.sale,newdata=sale_test)
plot(yhat.rf,sale_test$SalePrice)
abline(0,1)
mean((yhat.rf-sale_test$SalePrice)^2)
importance(rf.sale)

#Boosting
library(gbm)
set.seed(3859)
boost.sale<-gbm(SalePrice~.-House, data=sale_train, distribution="gaussian",
                n.trees=5000,interaction.dept=4)
summary(boost.sale)
yhat.boost<-predict(boost.sale,newdata=sale_test,n.trees=5000)
mean((yhat.boost-sale_test$SalePrice)^2)
plot(yhat.boost,sale_test$SalePrice, main="Boost Predicted vs. Actual", 
     xlab = "Predicted Sales Price", ylab="True Sales Price", 
     pch=19, col="sienna1")
abline(0,1, col="tan4")

set.seed(3859)
boost.mod<-gbm(SalePrice~.-House, data=train_dummy, distribution="gaussian",n.trees=5000,interaction.dept=4)
summary(boost.mod)
relative.influence(boost.mod)
rel.infl<-summary(boost.mod)
write.csv(rel.infl,"C:/Users/eudoc/Documents/Statistics/STAT580/Project2/rel_infl.csv", row.names = FALSE)


#XGBoost
library(xgboost)
library(DiagrammeR)

train_x = data.matrix(x[train,])
train_y = y[train]

test_x = data.matrix(x[-train,])
test_y = y[test]

xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

xgbc = xgboost(data = xgb_train, max.depth = 2, nrounds = 50)
print(xgbc)

pred_y = predict(xgbc, xgb_test)

mean((test_y - pred_y)^2)
xgb.model.dt.tree(model=xgbc, trees=3, use_int_id = FALSE)
xgb.plot.tree(model = xgbc, trees = 3)

#Ridge and Lasso Regression
library(glmnet)
#Ridge 
x<-model.matrix(SalePrice~.-House, train_dummy)
y<-train_dummy$SalePrice
set.seed(48)
train<-sample(1:nrow(x), nrow(x)*.8)
test<-(-train)
y.test<-y[test]

set.seed(2)
cv.out<-cv.glmnet(x[train,],y[train],alpha =0)
plot(cv.out)
bestlam<-cv.out$lambda.min
bestlam

grid<-10^seq(10, -2, length=100)
ridge.mod<-glmnet(x[train,], y[train], alpha=0, lambda=grid)
ridge.pred<-predict(ridge.mod, s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)
plot(ridge.pred, y.test)
abline(0,1)

#Lasso
set.seed(3859)
lasso.mod<-glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)
cv.out2<-cv.glmnet(x[train,], y[train], alpha=1)
plot(cv.out2)
bestlam2<-cv.out2$lambda.min
bestlam2

lasso.pred<-predict(lasso.mod, s=bestlam2, newx=x[test,])
mean((lasso.pred-y.test)^2)
plot(lasso.pred,y.test, main="Predicted vs. Actual", 
     xlab="Predicted Sales Price", ylab="True Sales Price",
     col="tan4", pch=19)
abline(0,1, col="sienna1")

set.seed(3859)
lasso.modf<-glmnet(x,y,alpha=1)  #use data to train model
plot(lasso.modf)
cv.outf<-cv.glmnet(x, y, alpha=1) #Cv used to find best lambda
plot(cv.outf)
bestlamf<-cv.outf$lambda.min     #lamda.min from CV
bestlamf

#Predicted Sale Price (Lasso)
x2<-data.matrix(test_dummy)       #create matrix of new data to predict
lasso.predf2<-predict(lasso.modf, s=bestlamf, newx=x2)
lasso.predf2                      #Prints predictions for Sale Price
lasso.sum<-summary(lasso.predf2)
hist(lasso.predf2, main="Predicted Sale Price Distribution (Lasso)", 
     xlab="Predicted Sales Price", col="slategray")

out<-glmnet(x,y, alpha=1, lambda=grid)
lasso.coef<-predict(lasso.modf,type="coefficients",s=bestlamf)
lasso.coef
coefs<-as.data.frame(lasso.coef)

#Elastic Net
#Find best alpha
list.of.fits<-list()
for (i in 0:10) {
  fit.name<-paste0("alpha", i/10)

list.of.fits[[fit.name]]<-cv.glmnet(x[train,],y[train], type.measure="mse",
                                   alpha= i/10, family="gaussian")
}

results.alph<-data.frame()

for (i in 0:10){
  fit.name<-paste0("alpha",i/10)
  
predicted<-predict(list.of.fits[[fit.name]], s=list.of.fits[[fit.name]]$lambda.1se,
                     newx=x[test,])
mse<-mean((y.test-predicted)^2)
temp<-data.frame(alpha=i/10, mse=mse, fit.name=fit.name)
results.alph<-rbind(results.alph,temp)
}
results.alph

#Another method
grid2<-expand.grid(.alpha=seq(0,1,by=.5),.lambda=seq(0,0.2,by=.1))
control<-trainControl(method = "LOOCV")
enet.train<-train(SalePrice~.-House,train_dummy,method="glmnet",trControl=control,tuneGrid=grid2)
enet.train

enet<-glmnet(x4[train,],y[train],family = "gaussian",alpha = .2 ,lambda = .2)
enet.coef<-coef(enet,lambda=.2,alpha=1,exact=T)
enet.coef

enet.pred<-predict(enet, newx=x4[test,],type = "response", lambda=.2,alpha=.2)
mean((enet.pred-y.test)^2)
plot(enet.pred,y.test)
abline(0,1)

set.seed(317)
enet.cv<-cv.glmnet(x4[train,],y[train],alpha=.2)
plot(enet.cv)
enet.cv$lambda.min
enet.cv$lambda.1se
coef(enet.cv,s="lambda.min")

enet.y.cv<-predict(enet.cv,newx = x4[test,],type='response',lambda="lambda.min", alpha = .2)
mean((enet.y.cv-y.test)^2)
plot(enet.y.cv,y.test)
abline(0,1)

#Predict sales price of the test houses (Boosting)
test.pred<-predict(boost.mod, newdata=test_dummy, n.trees=5000)
test.pred
boost.sum<-summary(test.pred)
hist(test.pred, main="Predicted Sale Price Distribution (Boosting)", 
     xlab="Predicted Sales Price", col="slategray")

#Create data from of predicted sales prices

final<-data.frame(test_dummy$uniqueID)
df_pred<- data.frame(test.pred, lasso.predf2)
final$SalePrice<-df_pred$test.pred
final$LassoPrice<-df_pred$s1
final$LassoPrice2<-df_pred$s1
colnames(final)[colnames(final)=="test_dummy.uniqueID"] <- "ID"

#rearrange rows in order
final$ID<-gsub("[a-zA-Z ]", "", final$ID)
final$ID<-str_remove_all(final$ID, "\\.")
final$ID<-as.numeric(final$ID)
final<-final[order(final$ID, decreasing = FALSE), ] 
final$House<-c("House.")
final$uniqueID<-do.call(paste,c(final[c("House","ID")],sep=""))
final <- final[ , ! names(final) %in% c("House","ID","SalePrice")]
final$SalePrice<-final$LassoPrice
final <- final[ , ! names(final) %in% c("LassoPrice")]
final <- final %>% relocate(uniqueID, .before = SalePrice)

View(final)
#Export Prediction Data
write.csv(final,"Statistics/STAT580/Project2/saleprice_predictions.csv", row.names = FALSE)

#Visualizations
options(scipen=999)
hist(train_dummy$SalePrice, col="lightblue4", 
     main="Training Data Sales Price Distribution", 
     xlab="Sales Price in Dollars")
lines(density(train_dummy$SalePrice), col="lightcoral", lwd=2)

p2 <- ggplot(train_dummy, aes(x=OverallQual, group=OverallQual, y=SalePrice)) + 
  geom_boxplot(fill="#EE7600")+ggtitle("Sales Price by Overall Quality")+
  theme(plot.title = element_text(hjust = 0.5))
p2

ggplot(train_dummy, aes(x = GrLivArea, y = SalePrice)) +
  geom_point(color="plum4")+ggtitle("Sale Price by Ground Living Area")+xlab("Ground Living Area") +
  theme(plot.title = element_text(hjust=0.5))

ggplot(train_dummy, aes(x = YearBuilt, y = SalePrice)) +
  geom_point(color="steelblue4")+ggtitle("Sale Price by Year Built")+xlab("Year Built") +
  theme(plot.title = element_text(hjust=0.5))

ggplot(train_dummy, aes(x = LotArea, y = SalePrice)) +
  geom_point(color="lightcyan4")+ggtitle("Sale Price by Lot Area")+xlab("Lot Area") +
  theme(plot.title = element_text(hjust=0.5))

ggplot(train_dummy, aes(x=YrSold, group=YrSold, y=SalePrice)) + 
  geom_boxplot(fill="Olivedrab4")+ggtitle("Sales Price by Year Sold")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(train_clean, aes(x=BsmtFinType1, group=BsmtFinType1, y=SalePrice)) + 
  geom_boxplot(fill="cadetblue4")+ggtitle("Sales Price by Basement Finish Type")+
  theme(plot.title = element_text(hjust = 0.5))

sd(lasso.predf2)

