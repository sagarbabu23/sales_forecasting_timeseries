library(readr)

plastic<-read.csv(file.choose()) # read the Amtrack data
View(plastic) # Seasonality 12 months 
windows()
plot(plastic$Sales,type="o")
# So creating 12 dummy variables 

X<- data.frame(outer(rep(month.abb,length = 60), month.abb,"==") + 0 )# Creating dummies for 12 months
View(X)

colnames(X)<-month.abb # Assigning month names 
View(X)
plasticdata<-cbind(plastic,X)
View(plasticdata)
colnames(plasticdata)[2]<-"Sales"
colnames(plasticdata)
plasticdata["t"]<- 1:60
View(plasticdata)
plasticdata["log_Sales"]<-log(plasticdata["Sales"])
plasticdata["t_square"]<-plasticdata["t"]*plasticdata["t"]
attach(plasticdata)

train<-plasticdata[1:40,]

test<-plasticdata[41:60,]

########################### LINEAR MODEL #############################

linear_model<-lm(Sales~t,data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear 



######################### Exponential #################################

expo_model<-lm(log_Sales~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo 

######################### Quadratic ####################################

Quad_model<-lm(Sales~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad 

######################### Additive Seasonality #########################

sea_add_model<-lm(Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add 

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear 

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Sales~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad 

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea 

######################## Multiplicative Seasonality Linear trend ##########################

multi_add_sea_model<-lm(log_Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea 

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Additive seasonality with Quadratic has least RMSE value

new_model <- lm(Sales~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=plasticdata)


resid <- residuals(new_model)
resid[1:10]
windows()
acf(resid,lag.max = 10)
# By principal of parcimony we will consider lag - 1  as we have so 
# many significant lags 
# Building Autoregressive model on residuals consider lag-1 

k <- arima(resid, order=c(1,0,0))
str(k)

View(data.frame(res=resid,newresid=k$residuals))
windows()
acf(k$residuals,lag.max = 15)
pred_res<- predict(arima(k$residuals,order=c(1,0,0)),n.ahead = 12)
str(pred_res)
pred_res$pred
acf(k$residuals)
write.csv(plasticdata,file="plasticdata.csv",col.names = F,row.names = F)

####################### Predicting new data #############################
library(readxl)
test_data<-read.csv(file.choose(),1)
View(test)
pred_new<-data.frame(predict(new_model,newdata=test,interval = 'predict'))
View(pred_new)
pred_new$fit <- pred_new$fit+pred_res$pred
View(pred_new)
