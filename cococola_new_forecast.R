library(readr)
install.packages("xlsx")
install.packages("dummies")
library(xlsx)
cocacola<-read.xlsx(file.choose(),sheetIndex = 1) 
View(cocacola) # Seasonality 3 months 
windows()
plot(cocacola$Sales,type="o")
# So creating 12 dummy variables 


Quarter_dummy  <- c("Q1","Q2","Q3","Q4")
Quarter_dummy <- rep(Quarter_dummy,length = nrow(cocacola))
cocacola <- cbind(cocacola,Quarter_dummy)
str(cocacola)
View(cocacola)
cocacola$Quater <- as.character(cocacola$Quater)
library(dummies)
temp <- dummy(cocacola[,3])
View(temp)
cocacola_new <- cbind(cocacola,temp)
View(cocacola_new)
cocacola_new["t"] <- c(1:nrow(cocacola_new))
View(cocacola_new)
cocacola_new["log_Sales"]<-log(cocacola_new["Sales"])
cocacola_new["t_square"]<-cocacola_new["t"]*cocacola_new["t"]


attach(cocacola_new)

train<-cocacola_new[1:30,]

test<-cocacola_new[31:42,]

########################### LINEAR MODEL #############################

linear_model<-lm(Sales~t,data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear # 209.9256



######################### Exponential #################################

expo_model<-lm(log_Sales~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo # 217.0526

######################### Quadratic ####################################

Quad_model<-lm(Sales~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad 

######################### Additive Seasonality #########################

sea_add_model<-lm(Sales~cocacolaQ1+cocacolaQ2+cocacolaQ3+cocacolaQ4,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add 

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Sales~t+cocacolaQ1+cocacolaQ2+cocacolaQ3+cocacolaQ4,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear 

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Sales~t+t_square+cocacolaQ1+cocacolaQ2+cocacolaQ3+cocacolaQ4,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_Sales~cocacolaQ1+cocacolaQ2+cocacolaQ3+cocacolaQ4,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea 

######################## Multiplicative Seasonality Linear trend ##########################

multi_add_sea_model<-lm(log_Sales~t+cocacolaQ1+cocacolaQ2+cocacolaQ3+cocacolaQ4,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea # 172.76

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Additive seasonality with Quadratic has least RMSE value

new_model <- lm(Sales~t+t_square+cocacolaQ1+cocacolaQ2+cocacolaQ3+cocacolaQ4,data=cocacola_new)


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
write.csv(cocacola_new,file="cocacola_new.csv",col.names = F,row.names = F)

####################### Predicting new data #############################
library(readxl)
test_data<-read_excel(file.choose(),1)
View(test)
pred_new<-data.frame(predict(new_model,newdata=test,interval = 'predict'))
View(pred_new)
pred_new$fit <- pred_new$fit+pred_res$pred
View(pred_new)

