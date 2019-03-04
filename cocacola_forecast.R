install.packages("readr")
library(readr)
library(xlsx)
install.packages("fpp")
install.packages("forecast")
install.packages("TSA")
install.packages("tseries")
install.packages("caret")
install.packages("ggplot2")
install.packages("ggthemes")
library(fpp)
library(forecast)
library(TSA)
library(tseries)
library('caret')
library('ggplot2') # visualization
library('ggthemes') # visualization

cocacola<-read.xlsx(file.choose(),sheetIndex = 1) 
View(cocacola) 
windows()
plot(cocacola$Sales,type="o")
 
coca <-  ts(cocacola,frequency = 4)
coca
View(coca)
summary(coca)
plot.ts(coca, col = "blue", main = "cocacola sales")


View(coca)
cycle(coca)

anuual= aggregate(coca)

decompose_coca <- decompose(coca,type = "additive")

seasonal_coca <- as.ts(decompose_coca$seasonal)
trend_coca <- as.ts(decompose_coca$trend)
random_cocca <- as.ts(decompose_coca$random)
plot.ts(seasonal_coca, main = "Seasonal Component")
plot.ts(trend_coca, main = "Trend Component")
plot.ts(random_cocca, main = "Randon Component")
coca <- data.frame(coca)



train<-coca[1:34,]

test<-coca[35:42,]

########################### LINEAR MODEL #############################

linear_model<-lm(Sales~ Quarter,data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear # 209.9256



######################### Exponential #################################

expo_model<-lm(log(Sales)~Quarter,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo # 217.0526

######################### Quadratic ####################################

Quad_model<-lm(Sales ~ poly(Quarter,3),data =train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad # 137.15

######################### Additive Seasonality #########################

  sea_add_model<-lm(Sales ~ Quarter+I(Quarter^2),data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add 

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Ridership~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Ridership-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear # 168.6316

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Ridership~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Ridership-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad # 50.60725

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_rider~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Ridership-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea # 268.197

######################## Multiplicative Seasonality Linear trend ##########################

multi_add_sea_model<-lm(log_rider~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Ridership-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea # 172.76

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Additive seasonality with Quadratic has least RMSE value

new_model <- lm(Ridership~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=trakdata)


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
write.csv(trakdata,file="trakdata.csv",col.names = F,row.names = F)

####################### Predicting new data #############################
library(readxl)
test_data<-read_excel(file.choose(),1)
View(test_data)
pred_new<-data.frame(predict(new_model,newdata=test_data,interval = 'predict'))
View(pred_new)
pred_new$fit <- pred_new$fit+pred_res$pred
View(pred_new)
