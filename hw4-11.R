# 加载必要的库
library(ISLR)  # 提供Auto数据集
library(MASS)   # 用于LDA和QDA
library(class)  # 用于KNN

# (a) 建立二元变量mpg01
data(Auto)
median_mpg <- median(Auto$mpg, na.rm = TRUE)
mpg01 <- ifelse(Auto$mpg > median_mpg, 1, 0)
Auto_df <- data.frame(mpg01, Auto)

# (b) 探索mpg01与其他特征的关系
par(mfrow = c(2, 3))
plot(Auto_df$mpg01, Auto_df$displacement, main = "mpg01 vs displacement")
plot(Auto_df$mpg01, Auto_df$horsepower, main = "mpg01 vs horsepower")
plot(Auto_df$mpg01, Auto_df$weight, main = "mpg01 vs weight")
plot(Auto_df$mpg01, Auto_df$acceleration, main = "mpg01 vs acceleration")
boxplot(displacement ~ mpg01, data = Auto_df, main = "Boxplot: displacement by mpg01")
boxplot(horsepower ~ mpg01, data = Auto_df, main = "Boxplot: horsepower by mpg01")

# (c) 分割训练集和测试集
set.seed(1)
train_index <- sample(1:nrow(Auto_df), 0.7 * nrow(Auto_df))
train_data <- Auto_df[train_index, ]
test_data <- Auto_df[-train_index, ]

# (d) LDA模型
lda_model <- lda(mpg01 ~ displacement + horsepower + weight + acceleration, data = train_data)
lda_pred <- predict(lda_model, test_data)
lda_error <- mean(lda_pred$class != test_data$mpg01)
cat("LDA测试误差:", lda_error, "\n")

# (e) QDA模型
qda_model <- qda(mpg01 ~ displacement + horsepower + weight + acceleration, data = train_data)
qda_pred <- predict(qda_model, test_data)
qda_error <- mean(qda_pred$class != test_data$mpg01)
cat("QDA测试误差:", qda_error, "\n")

# (f) 逻辑斯谛回归
logit_model <- glm(mpg01 ~ displacement + horsepower + weight + acceleration, data = train_data, family = binomial)
logit_pred <- predict(logit_model, test_data, type = "response")
logit_pred_class <- ifelse(logit_pred > 0.5, 1, 0)
logit_error <- mean(logit_pred_class != test_data$mpg01)
cat("逻辑斯谛回归测试误差:", logit_error, "\n")

# (g) KNN模型
k_values <- seq(1, 10, by = 2)
knn_errors <- numeric(length(k_values))
for(i in 1:length(k_values)) {
  knn_pred <- knn(train = train_data[, c("displacement", "horsepower", "weight", "acceleration")], 
                  test = test_data[, c("displacement", "horsepower", "weight", "acceleration")],
                  cl = train_data$mpg01, k = k_values[i])
  knn_errors[i] <- mean(knn_pred != test_data$mpg01)
}
best_k <- k_values[which.min(knn_errors)]
cat("最佳K值:", best_k, "\n")
cat("KNN最小测试误差:", min(knn_errors), "\n")