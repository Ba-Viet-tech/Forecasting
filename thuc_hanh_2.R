
# Bài thực hành 2
## Sử dụng dữ liệu giai đoạn 01/01/2015 – 31/12/2018 làm training dataset để xây dựng mô hình ARIMA hoặc SARIMA dự báo nhu cầu điện.
## Thể hiện time series data và predicted values (dựa trên mô hình xây dựng được) trên cùng một time plot
## Sử dụng mô hình đã xây dựng để dự báo nhu cầu điện giai đoạn 01/01/2019 – 31/12/2019.
## Nhận xét xem các giá trị dự báo mà mô hình đưa ra có phù hợp với dữ liệu hay không.
##  So sánh giá trị dự báo với dữ liệu thực tế (testing dataset) giai đoạn 01/01/2019 - 31/12/2019 tính sai số dự báo, đồng thời thể hiện testing data so với predicted values trên cùng time plot.

### 1 Mô tả dữ liệu
#### Dữ liệu bao gồm biến chính là nhu cầu điện hằng ngày (demand, MWh) cùng các biến liên quan đến thời tiết (nhiệt độ, bức xạ mặt trời, lượng mưa) và yếu tố lịch sinh hoạt (ngày học, ngày lễ), được sử dụng để phân tích và dự báo nhu cầu điện.

### 2 Dữ liệu
#### File: electric-demand-australia.csv
#### Tổng số quan sát: 2106 ngày

### 2.1 Các biến trong dataset
#### demand: tổng nhu cầu sử dụng điện mỗi ngày (MWh)
#### min_temperature: nhiệt độ thấp nhất trong ngày (°C)
#### max_temperature: nhiệt độ cao nhất trong ngày (°C)
#### solar_exposure: tổng năng lượng mặt trời trong ngày (MJ/m²)
#### rainfall: lượng mưa trung bình ngày (mm)
#### school_day: biến giả (1 nếu là ngày học, 0 nếu không)
#### holiday: biến giả (1 nếu là ngày lễ, 0 nếu không)

### 2.2 Chia tệp dữ liệu 
#### Training set 01/01/2015 - 31/12/2018
#### testing set 01/01/2019 - 31/12/2019



### 3 Phân tích mã nguồn

#### Chuẩn bị dữ liệu

edata <- read.csv("D:/dowload/electric-demand-australia.csv")
edata$school_day <- as.numeric(ifelse(edata$school_day == "Y", 1, 0))
edata$holiday    <- as.numeric(ifelse(edata$holiday == "Y", 1, 0))
edata$min_temperature <- as.numeric(as.character(edata$min_temperature))
edata$max_temperature <- as.numeric(as.character(edata$max_temperature))
edata$solar_exposure  <- as.numeric(as.character(edata$solar_exposure))
edata$rainfall        <- as.numeric(as.character(edata$rainfall))
edata$date <- as.Date(edata$date)
edata <- edata[order(edata$date), ]
edata <- na.omit(edata)
train <- edata[edata$date >= as.Date("2015-01-01") & edata$date <= as.Date("2018-12-31"), ]
test  <- edata[edata$date >= as.Date("2019-01-01") & edata$date <= as.Date("2019-12-31"), ]



#### Trực quan hóa dữ liệu 

demand_ts_7<-ts(train$demand, frequency = 7)
plot.ts (demand_ts_7, xlab= "Số tuần")







#### Kiểm tra tính mùa vụ 
#### 
library(forecast)
library(ggplot2)
demand_ts <- ts(train$demand, frequency = 365, start = c(2015, 1))
demand_ts_fixed <- window(demand_ts, end = c(2018, 365))
ggseasonplot(demand_ts_fixed, 
             year.labels = TRUE,
             main = "Biểu đồ so sánh giá trị của các ngày theo từng năm",
             xlab = "Ngày trong năm",
             ylab = "Nhu cầu (Demand)") +
  theme_minimal() +
  theme(legend.position = "right") +          
  guides(colour = guide_legend(title = "Năm"))
demand_30days <- subset(demand_ts, start = 1, end = 30)
####
data_sub <- train[1:30, ]
plot(data_sub$date, data_sub$demand,
     type = "b",        
     main = "Nhu cầu điện 30 ngày đầu",
     xlab = "Ngày",
     ylab = "Demand (MWh)")


#### Đồ thị cho thấy hình dạng biến động nhu cầu điện tương tự qua các năm, với các đỉnh và đáy nhỏ xuất hiện mỗi 7 ngày xác nhận sự tồn tại của yếu tố mùa vụ ổn định.

#### Kiểm tra tính xu hướng

library(trend)
mk_test <- mk.test(demand_ts)
print(mk_test)
#### Nhận thấy vì p_value < 0.05 nên chuỗi demand có tính xu hướng

### Ta loại bỏ yếu tố mùa vụ và yếu tố xu hướng cứ mỗi 7 ngày
demand_ts_7 <- ts(train$demand, frequency = 7)
lx  <- log(demand_ts_7)
dlx <- diff(lx)
ddlx <- diff(dlx, 7)

par(
  mfrow = c(1, 2),
  mar = c(4, 4.5, 3, 1)  
)

plot.ts(dlx,
        main = "Loại bỏ xu hướng\n(Sai phân bậc 1)",
        xlab = "Thời gian (Tuần)",
        ylab = expression(Delta*log(Demand)))

plot.ts(ddlx,
        main = "Loại bỏ xu hướng và mùa vụ\n(Sai phân bậc 1 + chu kỳ 7)",
        xlab = "Thời gian (tuần)",
        ylab = expression(Delta[7]*Delta*log(Demand)))

par(mfrow = c(1, 1))

#### Kiểm tra tính dừng của chuỗi sau khi loại bỏ tính mùa vụ và tính xu hướng



library(tseries)
adf.test(ddlx)
#### Nhận thấy p_value < 0.01 nên chuỗi đã dừng.


#### Tiếp tục xây dựng acf và pacf cho chuỗi demand
library(astsa)
acf2(ddlx)
### -> Chọn mô hình SARIMA(0,1,3)(0,1,1)7




### So Sánh các mô hình
m1 <- sarima(demand_ts_7, 0, 1, 3, 0, 1, 1, 7)
m2 <- sarima(demand_ts_7, 0, 1, 2, 0, 1, 1, 7)
m3 <- sarima(demand_ts_7, 1, 1, 3, 0, 1, 1, 7)
m4 <- sarima(demand_ts_7, 1, 1, 2, 0, 1, 1, 7)




#### Sử dụng hồi quy tuyến tính bội để chọn biến ngoại sinh có ý nghĩa

full_model <- lm(
  demand ~ min_temperature + max_temperature +
    solar_exposure + rainfall +
    school_day + holiday,
  data = train
)

summary(full_model)


#### Nhận thấy biến school_day có p_value > 0,05 nên loại
#### Các biến còn lại có p_value < 0,05 nên giữ lại


#### Chuẩn bị ma trận ngoại sinh 
xreg_vars <- c(
  "min_temperature",
  "max_temperature",
  "solar_exposure",
  "rainfall",
  "holiday"
)

xreg_train <- as.matrix(train[, xreg_vars])
storage.mode(xreg_train) <- "numeric"
xreg_train <- scale(xreg_train)

xreg_test <- as.matrix(test[, xreg_vars])
storage.mode(xreg_test) <- "numeric"
xreg_test <- scale(
  xreg_test,
  center = attr(xreg_train, "scaled:center"),
  scale  = attr(xreg_train, "scaled:scale")
)



#### Mô hình SARIMAX


library(forecast)
model_sarimax <- Arima(demand_ts_7, 
                       order = c(0,1,3), 
                       seasonal = c(0,1,1), 
                       xreg = xreg_train)





#### Thể hiện time series data và predicted values (dựa trên mô hình xây dựng được) trên cùng 1 time plot



train_fitted <- fitted(model_sarimax)
plot(demand_ts_7, col = "black",
     main = "Dữ liệu quá khứ và giá trị fitted (training)",
     ylab = "Demand (MWh)")
lines(train_fitted, col = "red")
legend("topright",
       legend = c("Actual", "Fitted"),
       col = c("black", "red"),
       lty = 1)








#### Dự báo cho nhu cầu sử dụng điện giai đoạn 01/01/2019 - 31/12/2019


options(scipen = 999)

forecast_2019 <- forecast(model_sarimax, xreg = xreg_test, level = 95)

n_train_display <- length(train$demand)

y_limit_min <- min(c(as.vector(train$demand), as.vector(forecast_2019$lower)))
y_limit_max <- max(c(as.vector(train$demand), as.vector(forecast_2019$upper)))

if(y_limit_min < 0) y_limit_min <- 0

plot(forecast_2019, 
     include = n_train_display,
     ylim = c(y_limit_min, y_limit_max),
     main = " Biểu đồ dự báo năm 2019",
     xlab = "Tuần", 
     ylab = "Demand (MWh)",
     col = "blue",
     fcol = "red",
     flwd = 2)

legend("topleft", 
       legend = c("Du lieu lich su (2015-2018)", "Du bao (Mean 2019)", "Khoang tin cay 95%"),
       col = c("blue", "red", "grey"), 
       lty = c(1, 1, NA), 
       fill = c(NA, NA, "grey"),
       border = "white", 
       cex = 0.7)

### 3/ Nhận thấy mô hình dự báo chưa phù hợp với giá trị thực tế và giá trị bị lệch rất nhiều

###4 / 

accuracy_metrics <- accuracy(forecast_2019, test$demand)
print(accuracy_metrics)



predicted_demand <- as.numeric(forecast_2019$mean)

plot(test$date, test$demand, 
     type = "l", 
     col = "black", 
     lwd = 1.5,
     main = "So sanh Nhu cau dien Thuc te vs Du bao (2019)",
     xlab = "Tuan", 
     ylab = "Nhu cau dien (MWh)",
     ylim = range(c(test$demand, predicted_demand)))

lines(test$date, predicted_demand, col = "red", lwd = 1.5)

legend("topright", 
       legend = c("Thuc te (Testing Data)", "Du bao (Predicted Values)"), 
       col = c("black", "red"), 
       lty = 1, 
       lwd = 2,
       cex = 0.8)



### Thiếu khả năng tổng quát hóa: Sai số tập Test cao gấp gần 5 lần tập Training.
### Không phản ánh đúng mức tăng trưởng hoặc các biến động thực tế của nhu cầu điện năm 2019.
### Không vượt qua được phương pháp dự báo cơ bản (MASE > 1), dẫn đến rủi ro thiếu hụt điện năng nếu áp dụng vào thực tế.


### Cả mô hình SARIMA và ARIMA không phù hợp


library(FinTS)
res <- residuals(model_sarimax)

arch_result <- ArchTest(res, lags = 12)

print(arch_result)

