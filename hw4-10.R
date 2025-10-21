# 加载必要的库
library(ISLR)  # 提供Weekly数据集
library(MASS)   # 用于LDA和QDA
library(class)  # 用于KNN


# 加载Weekly数据集
data(Weekly)

# 查看数据集结构，确保数据加载正确
str(Weekly)
summary(Weekly)

# (a) 对Weekly数据进行数值和图像描述统计，检查是否存在模式
print("描述统计结果：")
print(summary(Weekly))
# 图像描述统计：绘制相关图表
par(mfrow = c(2, 3)) # 设置多图布局
plot(Weekly$Year, Weekly$Today, main = "Today vs Year", xlab = "Year", ylab = "Today") # 时间序列图
hist(Weekly$Volume, main = "Histogram of Volume", xlab = "Volume") # Volume直方图
boxplot(Weekly$Lag1, main = "Boxplot of Lag1") # Lag1箱线图
plot(Weekly$Lag2, Weekly$Direction, main = "Lag2 vs Direction") # 散点图
barplot(table(Weekly$Direction), main = "Barplot of Direction")

# 重置图形参数
par(mfrow = c(1, 1))

# (b) 用整个数据集建立逻辑斯谛回归模型，使用Lag1-Lag5和Volume作为预测变量，Direction作为响应变量
model_b <- glm(Direction ~Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, family = binomial) # 逻辑斯谛回归模型
print(summary(model_b))


# (c) 计算混淆矩阵和整体预测准确率，解释错误类型
pred_b <- predict(model_b, type = "response")  # 获取预测概率
pred_class_b <- ifelse(pred_b > 0.5, "Up", "Down")  # 以0.5为阈值分类
conf_matrix_b <- table(Predicted = pred_class_b, Actual = Weekly$Direction)  # 混淆矩阵
accuracy_b <- sum(diag(conf_matrix_b)) / sum(conf_matrix_b)  # 准确率
cat("混淆矩阵:\n")
print(conf_matrix_b)
cat("整体预测准确率:", accuracy_b, "\n")

# (d) 使用1990-2008年数据训练逻辑回归模型，仅用Lag2预测，测试2009-2010年数据
train <- subset(Weekly, Year >= 1990 & Year <= 2008)  # 训练集：1990-2008
test <- subset(Weekly, Year >= 2009 & Year <= 2010)   # 测试集：2009-2010
model_d <- glm(Direction ~ Lag2, data = train, family = binomial)  # 仅用Lag2拟合
pred_d <- predict(model_d, newdata = test, type = "response")
pred_class_d <- ifelse(pred_d > 0.5, "Up", "Down")
conf_matrix_d <- table(Predicted = pred_class_d, Actual = test$Direction)
accuracy_d <- sum(diag(conf_matrix_d)) / sum(conf_matrix_d)
cat("(d) 逻辑回归测试准确率:", accuracy_d, "\n")
print(conf_matrix_d)

# (e) 应用LDA重复(d)的过程
model_e <- lda(Direction ~ Lag2, data = train)
pred_e <- predict(model_e, newdata = test)
conf_matrix_e <- table(Predicted = pred_e$class, Actual = test$Direction)
accuracy_e <- sum(diag(conf_matrix_e)) / sum(conf_matrix_e)
cat("(e) LDA测试准确率:", accuracy_e, "\n")
print(conf_matrix_e)

# (f) 应用QDA重复(d)的过程
model_f <- qda(Direction ~ Lag2, data = train)
pred_f <- predict(model_f, newdata = test)
conf_matrix_f <- table(Predicted = pred_f$class, Actual = test$Direction)
accuracy_f <- sum(diag(conf_matrix_f)) / sum(conf_matrix_f)
cat("(f) QDA测试准确率:", accuracy_f, "\n")
print(conf_matrix_f)

# (g) 应用K=1的KNN重复(d)的过程
train_X <- as.matrix(train$Lag2)  # 预测变量矩阵
test_X <- as.matrix(test$Lag2)
train_Y <- train$Direction
set.seed(123)  # 设置随机种子以确保可重复性
pred_g <- knn(train_X, test_X, train_Y, k = 1)
conf_matrix_g <- table(Predicted = pred_g, Actual = test$Direction)
accuracy_g <- sum(diag(conf_matrix_g)) / sum(conf_matrix_g)
cat("(g) KNN (K=1) 测试准确率:", accuracy_g, "\n")
print(conf_matrix_g)

# (i) 探索不同预测变量组合、变换和交互作用，找出最佳组合

# 示例1: 逻辑回归带交互项
model_i1 <- glm(Direction ~ Lag2 * Volume, data = train, family = binomial)
pred_i1 <- predict(model_i1, newdata = test, type = "response")
pred_class_i1 <- ifelse(pred_i1 > 0.5, "Up", "Down")
acc_i1 <- mean(pred_class_i1 == test$Direction)

# 示例2: 使用多个变量
model_i2 <- glm(Direction ~ Lag1 + Lag2, data = train, family = binomial)
pred_i2 <- predict(model_i2, newdata = test, type = "response")
pred_class_i2 <- ifelse(pred_i2 > 0.5, "Up", "Down")
acc_i2 <- mean(pred_class_i2 == test$Direction)

# 示例3: 尝试KNN with K=5（优化K值）
pred_i3 <- knn(train_X, test_X, train_Y, k = 5)
acc_i3 <- mean(pred_i3 == test$Direction)

# 比较准确率
acc_i <- c(Interaction = acc_i1, MultiVar = acc_i2, KNN5 = acc_i3)
best_i <- names(which.max(acc_i))
cat("(i) 探索性结果准确率:", acc_i, "\n")
cat("最佳组合:", best_i, "with accuracy", max(acc_i), "\n")
# 分析：最佳组合可能涉及Lag2和Volume的交互，或优化K值；需通过交叉验证确认。

# 输出最佳混淆矩阵（以示例model_i1为例）
conf_matrix_i <- table(Predicted = pred_class_i1, Actual = test$Direction)
cat("最佳混淆矩阵（示例）:\n")
print(conf_matrix_i)













