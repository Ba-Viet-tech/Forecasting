

# Đọc dữ liệu
data <- read.csv("D:/Insurance-company-expenses.csv")
#1.1.1. Tiền xử lý dữ liệu
# Loại bỏ các dòng có giá trị Missing (NA)
data <- na.omit(data)
# Xử lý các giá trị quan trắc < 0 cho các biến không cho phép âm
data <- data <- data[data$EXPENSES >= 0 & data$ASSETS >= 0 & data$LONGLOSS >= 0 & data$GPWPERSONAL
                     >= 0 & data$GPWCOMM >= 0, ]
#1.1.2. Summary statistics cho các biến non-binary
non_binary_vars <- c("EXPENSES", "STAFFWAGE", "AGENTWAGE", "LONGLOSS",
                     "SHORTLOSS", "GPWPERSONAL", "GPWCOMM", "ASSETS", "CASH")
summary_stats <- summary(data[, non_binary_vars])
print(summary_stats)



#1.2
#
#1.2 histogram and Q-Q normal plot
hist(data$EXPENSES, main="Histogram of EXPENSES", col="skyblue", xlab="Expenses")
qqnorm(data$EXPENSES, main="Q-Q Plot of EXPENSES")
qqline(data$EXPENSES)



# 1.2 Biến đổi logarithm ln(Y) , sau đó đánh giá
data$LNEXPENSES_raw <- log(data$EXPENSES)
hist(data$LNEXPENSES_raw, main="Histogram of ln(EXPENSES)", col="lightgreen", xlab="ln(Expenses)")
qqnorm(data$LNEXPENSES_raw, main="Q-Q Plot of ln(EXPENSES)")
qqline(data$LNEXPENSES_raw)



#1.3 Biến đổi ln(1+u) cho tất cả các biến non-binary theo yêu cầu
data$LNEXPENSES <- log(1 + data$EXPENSES)
data$LNSTAFFWAGE <- log(1 + data$STAFFWAGE)
data$LNAGENTWAGE <- log(1 + data$AGENTWAGE)
data$LNLONGLOSS <- log(1 + data$LONGLOSS)
data$LNSHORTLOSS <- log(1 + data$SHORTLOSS)
data$LNGPWPERSONAL <- log(1 + data$GPWPERSONAL)
data$LNGPWCOMM <- log(1 + data$GPWCOMM)
data$LNASSETS <- log(1 + data$ASSETS)
data$LNCASH <- log(1 + data$CASH)
##
# Tạo bảng tương quan cho các biến đã biến đổi
Log_vars <- c("LNEXPENSES", "LNSTAFFWAGE", "LNAGENTWAGE", "LNLONGLOSS", "LNSHORTLOSS", "
LNGPWPERSONAL", "LNGPWCOMM", "LNASSETS", "LNCASH")
cor_matrix <- cor(data[, Log_vars])
print(cor_matrix["LNEXPENSES", ]) # Xem tương quan với LNEXPENSES



# Boxplot cho LNEXPENSES theo GROUP
boxplot(LNEXPENSES ~ GROUP, data = data, 
        main="LNEXPENSES by GROUP", 
        xlab="Group (0: No, 1: Affiliated)", 
        ylab="LNEXPENSES", col=c("orange", "cyan"))



#1.4
# Xây dựng mô hình hồi quy tuyến tính (M1.1) cho LNEXPENSES
m1.1 <- lm(LNEXPENSES ~ GROUP + MUTUAL + STOCK + LNSTAFFWAGE + LNAGENTWAGE + LNLONGLOSS +
             LNSHORTLOSS + LNGPWPERSONAL + LNGPWCOMM + LNASSETS + LNCASH,data = data)
summary(m1.1)




m1.2 <- lm(LNEXPENSES ~ GROUP + LNSTAFFWAGE + LNAGENTWAGE +
             LNLONGLOSS + LNSHORTLOSS + LNGPWPERSONAL + LNGPWCOMM + LNASSETS,
           data = data)
summary(m1.2)

# Lấy giá trị trung vị của biến gốc (không phải log)
median_expenses <- median(data$EXPENSES)
median_gpwcomm <- median(data$GPWCOMM)
# Tính toán mức tăng tuyệt đối khi GPWCOMM tăng 1 đôla
delta_expenses <- 0.0977806 * (median_expenses / median_gpwcomm)
cat("Khi GPWCOMM tang 1$, EXPENSES tang khoang:", delta_expenses, "$")





#Diễn giải hệ số LNGPWCOMM
# Lấy giá trị trung vị của biến gốc
median_expenses <- median(data$EXPENSES)
median_gpwcomm <- median(data$GPWCOMM)
# Tính toán mức tăng tuyệt đối khi GPWCOMM tăng 1 đôla
delta_expenses <- 0.0977806 * (median_expenses / median_gpwcomm)
cat("Khi GPWCOMM tang 1$, EXPENSES tang khoang:", delta_expenses, "$")




#1.5
## 1. So sánh R-squared hiệu chỉnh (Adjusted R-squared)
summary(m1.1)$adj.r.squared
summary(m1.2)$adj.r.squared



# 2. So sánh chỉ số AIC (Akaike Information Criterion) - Càng thấp càng tốt
AIC(m1.1)
AIC(m1.2)



# 3. Kiểm định ANOVA để xem việc loại bỏ 3 biến có làm mô hình tệ đi đáng kể không
anova(m1.2, m1.1)




mex1 <- m1.2
new_data <- data.frame(
  GROUP = 1,
  LNSTAFFWAGE = log(1 + median(data$STAFFWAGE)),
  LNAGENTWAGE = log(1 + median(data$AGENTWAGE)),
  LNLONGLOSS = log(1 + 0.025),
  LNSHORTLOSS = log(1 + 0.040),
  LNGPWPERSONAL = log(1 + 0.050),
  LNGPWCOMM = log(1 + 0.120),
  LNASSETS = log(1 + 0.400)
)
# Dự báo giá trị log
conf_log <- predict(mex1, newdata = new_data, interval = "confidence", level = 0.95)
pred_log <- predict(mex1, newdata = new_data, interval = "prediction", level = 0.95)
print(conf_log)

print(pred_log)



# Chuyển đổi ngược về đơn vị triệu USD (EXPENSES = exp(LNEXPENSES) - 1)
cat("\nKhoảng tin cậy 95% cho giá trị dự báo:\n")
conf_real <- exp(conf_log) - 1
print(conf_real)
cat("Giá trị dự báo EXPENSES:\n")
pred_real <- exp(pred_log) - 1
print(pred_real)

