data <- read.csv("F:/Dự báo/Đồ án cuối kì/data gas consumption.csv")

#Chuyển dữ liệu thành chuỗi thời gian 
ts_gas <- ts(data$Value, start = c(2019, 1), frequency = 12)
ts_gas

#Vẽ lại bằng R
plot.ts(ts_gas,col = "dodgerblue", lwd = 2.5,
        main = "Monthly Consumption 2019 - 2025",
        xlab = "Year", ylab = "Value")
grid(15, 15, col = "gray")
#Các biểu hiện nổi bật
#Chu kỳ lặp lại theo chu kỳ 12 tháng:
#Đỉnh: Thường rơi vào tháng 12 và tháng 1 hàng năm
#Đáy: Rơi vào khoảng tháng 5 và tháng 6
#Xu hướng: Có sự tăng trưởng đều đặn

# Monthplot
monthplot(ts_gas, col = "dodgerblue", lwd = 2.5, col.base = "indianred",
          xlab = "Month", ylab = "Value")
grid(15, 15, col = "gray")

#Mô tả các biểu hiện nổi bật:
#Mức tiêu thụ có xu hướng tăng nhẹ qua các năm (đặc biệt các đỉnh sau cao hơn đỉnh trước).
#Đồ thị lặp lại hình dạng sau mỗi chu kỳ 12 tháng (1 năm) ---> Tính mùa vụ mạnh.
#Trong mỗi năm, giá trị cao nhất vào Tháng 1 và thấp nhất vào khoảng Tháng 5 - Tháng 6.

#Xây dựng SARIMA
# 
x = ts(data$Value, start = c(2019, 1), frequency = 12)

# Thực hiện các phép biến đổi
lx = log(x)              # Logarit
dlx = diff(lx)           # loai bo xu huong
ddlx = diff(dlx, 12)     # loai bo mua vu

library(tseries)
adf_test <- adf.test(ddlx)
print(adf_test) 
# Với p-value = 0.01 < 0.05, xác nhận chuỗi ddlx đã dừng

# Vẽ biểu đồ so sánh các bước biến đổi
plot.ts(cbind(x, lx, dlx, ddlx), main="Cac buoc bien doi du lieu gas")

par(mfrow=c(2, 1), mar=c(3, 3, 2, 1))
monthplot(dlx, main="Monthplot loai bo xu huong")
monthplot(ddlx, main="Monthplot loai bo ca xu huong va mua vu")

#Biểu đồ hàm acf và pacf
library(astsa)
acf2(ddlx, 50)

# Ước lượng mô hình: sarima(dữ liệu_log, p, d, q, P, D, Q, S)
m1 <- sarima(lx, 1, 1, 1, 0, 1, 1, 12)
m2 <- sarima(lx, 0, 1, 1, 0, 1, 1, 12)
m3 <- sarima(lx, 1, 1, 0, 0, 1, 1, 12)

#
best_model <- m2

# Dự báo 12 tháng tiếp theo cho chuỗi đã lấy Log (lx)
# n.ahead = 12: số tháng cần dự báo
fore = sarima.for(lx, n.ahead = 12, 0, 1, 1, 0, 1, 1, 12)

# Chuyển kết quả từ Log về giá trị thực tế (nguyên gốc)
# Vì ta dùng log nên phải dùng hàm exp() để đưa về đơn vị sản lượng gas
predicted_values = exp(fore$pred)
lower_bound = exp(fore$pred - 1.96 * fore$se) # Khoảng tin cậy dưới 95%
upper_bound = exp(fore$pred + 1.96 * fore$se) # Khoảng tin cậy trên 95%

# In kết quả dự báo ra màn hình
print(predicted_values)

# 1. Trích xuất phần dư (residuals) từ mô hình sarima
res <- best_model$fit$residuals


# 2. Tính giá trị khớp (Fitted values) đưa về đơn vị gốc bằng exp()
# Do mô hình chạy trên lx (log), ta lấy lx trừ phần dư rồi mới exp()
fitted_values <- exp(lx - res)

# 3. Vẽ đồ thị so sánh
# Giá trị khớp = Giá trị thực tế - Phần dư
plot(ts_gas, col = "dodgerblue", lwd = 2, 
     main = "Observed values and Predicted values",
     xlab = "Time", ylab = "Gas Consumption")
#
lines(fitted_values, col = "red", lwd = 1.5)
#
grid(10, 10, col = "gray")