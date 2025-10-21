# (a) 使用 rnorm() 函数生成长度为 n=100 的预测变量 X 和长度为 n=100 的噪声向量 ε
set.seed(1) # 为了结果可重现性设置随机种子
n <- 100
X <- rnorm(n)
epsilon <- rnorm(n)

# (b) 依据以下模型产生长度为 n=100 的响应变量 Y：
# 定义beta常数
beta0 <- 1
beta1 <- 2
beta2 <- 0.5
beta3 <- -1

# 生成响应变量 Y
Y <- beta0 + beta1 * X + beta2 * X^2 + beta3 * X^3 + epsilon

# (c) 利用 regsubsets() 函数对数据集使用最优子集选择法，从包含预测变量X,X2,...,X10X,X2,...,X10的模型中选出最优的模型
# 创建X的多项式项
X_poly <- model.matrix(Y ~ poly(X, 10, raw = TRUE))[,-1] # 移除截距项
colnames(X_poly) <- paste0("X", 1:10) # 给列命名

# 将X的多项式项和Y组合成数据框
data_df <- data.frame(Y, X_poly)

# 使用regsubsets进行最优子集选择
library(leaps)
regfit.full <- regsubsets(Y ~ ., data = data_df, nvmax = 10) # nvmax指定最大变量数

# 查看选择结果的统计量
summary_regfit <- summary(regfit.full)

# 根据Cp、BIC和调整R^2选择最优模型

# 1. Cp准则 (Mallows' Cp)
# Cp越小越好，通常接近p的模型是好的
which.min(summary_regfit$cp)
# 绘制Cp图
plot(summary_regfit$cp, type = "l", xlab = "Number of Variables", ylab = "Cp", main = "Cp Criterion for Model Selection")
points(which.min(summary_regfit$cp), summary_regfit$cp[which.min(summary_regfit$cp)], col = "red", cex = 2, pch = 20)
abline(h = min(summary_regfit$cp), col = "red", lty = 2)
mtext(paste("Optimal model based on Cp has", which.min(summary_regfit$cp), "variables"), side = 3, line = 0.5)

# 2. BIC准则 (Bayesian Information Criterion)
# BIC越小越好，惩罚更复杂的模型
which.min(summary_regfit$bic)
# 绘制BIC图
plot(summary_regfit$bic, type = "l", xlab = "Number of Variables", ylab = "BIC", main = "BIC Criterion for Model Selection")
points(which.min(summary_regfit$bic), summary_regfit$bic[which.min(summary_regfit$bic)], col = "blue", cex = 2, pch = 20)
abline(h = min(summary_regfit$bic), col = "blue", lty = 2)
mtext(paste("Optimal model based on BIC has", which.min(summary_regfit$bic), "variables"), side = 3, line = 0.5)

# 3. 调整R^2 (Adjusted R-squared)
# 调整R^2越大越好
which.max(summary_regfit$adjr2)
# 绘制调整R^2图
plot(summary_regfit$adjr2, type = "l", xlab = "Number of Variables", ylab = "Adjusted R^2", main = "Adjusted R^2 Criterion for Model Selection")
points(which.max(summary_regfit$adjr2), summary_regfit$adjr2[which.max(summary_regfit$adjr2)], col = "green", cex = 2, pch = 20)
abline(h = max(summary_regfit$adjr2), col = "green", lty = 2)
mtext(paste("Optimal model based on Adjusted R^2 has", which.max(summary_regfit$adjr2), "variables"), side = 3, line = 0.5)

# 获取基于Cp、BIC和Adjusted R^2选择的最优模型（假设都是3个变量）
# 例如，选择3个变量的模型
print(coef(regfit.full, 3))


# (d) 向前和向后逐步选择重复c的步骤
# (d) 向前和向后逐步选择 - 重复(c)的步骤

# 向前逐步选择 (Forward Selection)
regfit.forward <- regsubsets(Y ~ ., data = data_df, nvmax = 10, method = "forward")
summary_forward <- summary(regfit.forward)

# 基于Cp、BIC和调整R^2选择最优模型
# Cp准则
which.min(summary_forward$cp)
plot(summary_forward$cp, type = "l", xlab = "Number of Variables", ylab = "Cp", 
    main = "Forward Selection - Cp Criterion")
points(which.min(summary_forward$cp), summary_forward$cp[which.min(summary_forward$cp)], 
    col = "red", cex = 2, pch = 20)
abline(h = min(summary_forward$cp), col = "red", lty = 2)
mtext(paste("Optimal model based on Cp has", which.min(summary_forward$cp), "variables"), 
    side = 3, line = 0.5)

# BIC准则
which.min(summary_forward$bic)
plot(summary_forward$bic, type = "l", xlab = "Number of Variables", ylab = "BIC", 
    main = "Forward Selection - BIC Criterion")
points(which.min(summary_forward$bic), summary_forward$bic[which.min(summary_forward$bic)], 
    col = "blue", cex = 2, pch = 20)
abline(h = min(summary_forward$bic), col = "blue", lty = 2)
mtext(paste("Optimal model based on BIC has", which.min(summary_forward$bic), "variables"), 
    side = 3, line = 0.5)

# 调整R^2
which.max(summary_forward$adjr2)
plot(summary_forward$adjr2, type = "l", xlab = "Number of Variables", ylab = "Adjusted R^2", 
    main = "Forward Selection - Adjusted R^2 Criterion")
points(which.max(summary_forward$adjr2), summary_forward$adjr2[which.max(summary_forward$adjr2)], 
    col = "green", cex = 2, pch = 20)
abline(h = max(summary_forward$adjr2), col = "green", lty = 2)
mtext(paste("Optimal model based on Adjusted R^2 has", which.max(summary_forward$adjr2), "variables"), 
    side = 3, line = 0.5)

# 输出最优模型系数（假设选择3个变量）
print(coef(regfit.forward, 3))

# 向后逐步选择 (Backward Selection)
regfit.backward <- regsubsets(Y ~ ., data = data_df, nvmax = 10, method = "backward")
summary_backward <- summary(regfit.backward)

# 基于Cp、BIC和调整R^2选择最优模型
# Cp准则
which.min(summary_backward$cp)
plot(summary_backward$cp, type = "l", xlab = "Number of Variables", ylab = "Cp", 
    main = "Backward Selection - Cp Criterion")
points(which.min(summary_backward$cp), summary_backward$cp[which.min(summary_backward$cp)], 
    col = "red", cex = 2, pch = 20)
abline(h = min(summary_backward$cp), col = "red", lty = 2)
mtext(paste("Optimal model based on Cp has", which.min(summary_backward$cp), "variables"), 
    side = 3, line = 0.5)

# BIC准则
which.min(summary_backward$bic)
plot(summary_backward$bic, type = "l", xlab = "Number of Variables", ylab = "BIC", 
    main = "Backward Selection - BIC Criterion")
points(which.min(summary_backward$bic), summary_backward$bic[which.min(summary_backward$bic)], 
    col = "blue", cex = 2, pch = 20)
abline(h = min(summary_backward$bic), col = "blue", lty = 2)
mtext(paste("Optimal model based on BIC has", which.min(summary_backward$bic), "variables"), 
    side = 3, line = 0.5)

# 调整R^2
which.max(summary_backward$adjr2)
plot(summary_backward$adjr2, type = "l", xlab = "Number of Variables", ylab = "Adjusted R^2", 
    main = "Backward Selection - Adjusted R^2 Criterion")
points(which.max(summary_backward$adjr2), summary_backward$adjr2[which.max(summary_backward$adjr2)], 
    col = "green", cex = 2, pch = 20)
abline(h = max(summary_backward$adjr2), col = "green", lty = 2)
mtext(paste("Optimal model based on Adjusted R^2 has", which.max(summary_backward$adjr2), "variables"), 
    side = 3, line = 0.5)

# 输出最优模型系数（假设选择3个变量）
print(coef(regfit.backward, 3))

# (e) lasso 拟合数据集
library(glmnet)
# 创建X的多项式项（包含X^1到X^10）
X_poly <- model.matrix(Y ~ poly(X, 10, raw = TRUE))[,-1]  # 移除截距项

# 将X的多项式项转换为矩阵形式
X_matrix <- as.matrix(X_poly)

# 确保Y是向量
Y_vector <- as.vector(Y)

# 执行交叉验证以选择最优的 lambda
cv_lasso <- cv.glmnet(X_matrix, Y_vector, alpha = 1, nfolds = 10)

# 查看交叉验证结果
print(cv_lasso)

# 绘制 CV 误差曲线
plot(cv_lasso, main = "Cross-Validation Error vs Lambda for Lasso", xlab = "log(Lambda)", ylab = "CV Error")
abline(v = log(cv_lasso$lambda.min), col = "red", lty = 2)
abline(v = log(cv_lasso$lambda.1se), col = "blue", lty = 2)
mtext(paste("Optimal lambda (min):", round(cv_lasso$lambda.min, 4)), side = 3, line = 0.5, col = "red")
mtext(paste("Lambda with 1SE:", round(cv_lasso$lambda.1se, 4)), side = 3, line = 1.5, col = "blue")

# 使用最优 lambda 得到系数估计
lasso_coef_min <- coef(cv_lasso, s = "lambda.min")
lasso_coef_1se <- coef(cv_lasso, s = "lambda.1se")

# 输出结果
print("Lasso Coefficients (lambda.min):")
print(lasso_coef_min)

print("\nLasso Coefficients (lambda.1se):")
print(lasso_coef_1se)

# (f) 依据新模型 Y = β0 + β1*X^2 + ε 生成响应变量并进行模型选择

# 重新设置随机种子以确保结果可重现
set.seed(1)
n <- 100
X <- rnorm(n)
epsilon <- rnorm(n)

# 定义新的参数（仅含 X^2 项）
beta0 <- 1
beta1 <- 2

# 生成新的响应变量 Y
Y <- beta0 + beta1 * X^2 + epsilon

# 创建X的多项式项（包含X^1到X^10）
X_poly <- model.matrix(Y ~ poly(X, 10, raw = TRUE))[,-1]  # 移除截距项
colnames(X_poly) <- paste0("X", 1:10)

# 将X的多项式项和Y组合成数据框
data_df <- data.frame(Y, X_poly)

# 使用最优子集选择法（regsubsets）
library(leaps)
regfit.full <- regsubsets(Y ~ ., data = data_df, nvmax = 10)

# 查看选择结果的统计量
summary_regfit <- summary(regfit.full)

# 根据Cp、BIC和调整R^2选择最优模型
# Cp准则
which.min(summary_regfit$cp)
plot(summary_regfit$cp, type = "l", xlab = "Number of Variables", ylab = "Cp", 
     main = "Cp Criterion for Optimal Subset Selection")
points(which.min(summary_regfit$cp), summary_regfit$cp[which.min(summary_regfit$cp)], col = "red", cex = 2, pch = 20)
abline(h = min(summary_regfit$cp), col = "red", lty = 2)
mtext(paste("Optimal model based on Cp has", which.min(summary_regfit$cp), "variables"), side = 3, line = 0.5)

# BIC准则
which.min(summary_regfit$bic)
plot(summary_regfit$bic, type = "l", xlab = "Number of Variables", ylab = "BIC", 
     main = "BIC Criterion for Optimal Subset Selection")
points(which.min(summary_regfit$bic), summary_regfit$bic[which.min(summary_regfit$bic)], col = "blue", cex = 2, pch = 20)
abline(h = min(summary_regfit$bic), col = "blue", lty = 2)
mtext(paste("Optimal model based on BIC has", which.min(summary_regfit$bic), "variables"), side = 3, line = 0.5)

# 调整R^2
which.max(summary_regfit$adjr2)
plot(summary_regfit$adjr2, type = "l", xlab = "Number of Variables", ylab = "Adjusted R^2", 
     main = "Adjusted R^2 Criterion for Optimal Subset Selection")
points(which.max(summary_regfit$adjr2), summary_regfit$adjr2[which.max(summary_regfit$adjr2)], col = "green", cex = 2, pch = 20)
abline(h = max(summary_regfit$adjr2), col = "green", lty = 2)
mtext(paste("Optimal model based on Adjusted R^2 has", which.max(summary_regfit$adjr2), "variables"), side = 3, line = 0.5)

# 输出最优模型系数（假设为2个变量：截距和X^2）
print("Optimal Subset Selection - Coefficients:")
print(coef(regfit.full, 2))

# 使用Lasso方法
library(glmnet)
X_matrix <- as.matrix(X_poly)
Y_vector <- as.vector(Y)

# 执行交叉验证以选择最优的 lambda
cv_lasso <- cv.glmnet(X_matrix, Y_vector, alpha = 1, nfolds = 10)

# 绘制CV误差曲线
plot(cv_lasso, main = "Cross-Validation Error vs Lambda for Lasso", xlab = "log(Lambda)", ylab = "CV Error")
abline(v = log(cv_lasso$lambda.min), col = "red", lty = 2)
abline(v = log(cv_lasso$lambda.1se), col = "blue", lty = 2)
mtext(paste("Optimal lambda (min):", round(cv_lasso$lambda.min, 4)), side = 3, line = 0.5, col = "red")
mtext(paste("Lambda with 1SE:", round(cv_lasso$lambda.1se, 4)), side = 3, line = 1.5, col = "blue")

# 获取最优模型系数
lasso_coef_min <- coef(cv_lasso, s = "lambda.min")
lasso_coef_1se <- coef(cv_lasso, s = "lambda.1se")

print("\nLasso Coefficients (lambda.min):")
print(lasso_coef_min)

print("\nLasso Coefficients (lambda.1se):")
print(lasso_coef_1se)