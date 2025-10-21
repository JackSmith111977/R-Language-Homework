# (a) 数据集分割为训练集和测试集 
library(ISLR)
data(College)

set.seed(123)  # 固定随机种子
train_index <- sample(nrow(College), 0.7 * nrow(College))  # 70%训练集
train <- College[train_index, ]
test <- College[-train_index, ]

# (b) 最小二乘（OLS）模型与测试误差 
# 训练OLS模型
lm_model <- lm(Apps ~ ., data = train)  

# 测试集预测与误差
pred_lm <- predict(lm_model, newdata = test)  
test_error_lm <- mean((test$Apps - pred_lm)^2)  # MSE
print("最小二乘测试误差：")
print(test_error_lm)  # 输出测试误差

# (c) 全面改进的岭回归模型
library(glmnet)

# 准备矩阵并标准化特征
x_train <- model.matrix(Apps ~ ., train)[, -1]  
y_train <- train$Apps
x_test <- model.matrix(Apps ~ ., test)[, -1]  

# 标准化特征矩阵
x_train_scaled <- scale(x_train)
x_test_scaled <- scale(x_test, 
                    center = attr(x_train_scaled, "scaled:center"), 
                    scale = attr(x_train_scaled, "scaled:scale"))

# 交叉验证选λ - 使用更合理的lambda范围
lambda_seq <- exp(seq(log(0.01), log(1000), length.out = 100))
ridge_cv <- cv.glmnet(x_train_scaled, y_train, alpha = 0, lambda = lambda_seq)  

# 比较lambda.min和lambda.1se
best_lambda_ridge_min <- ridge_cv$lambda.min
best_lambda_ridge_1se <- ridge_cv$lambda.1se

# 测试两种lambda的性能
pred_ridge_min <- predict(ridge_cv, newx = x_test_scaled, s = best_lambda_ridge_min)  
test_error_ridge_min <- mean((test$Apps - pred_ridge_min)^2)

pred_ridge_1se <- predict(ridge_cv, newx = x_test_scaled, s = best_lambda_ridge_1se)  
test_error_ridge_1se <- mean((test$Apps - pred_ridge_1se)^2)

print("岭回归最优lambda (min)：")
print(best_lambda_ridge_min)
print("岭回归测试误差 (min)：")
print(test_error_ridge_min)

print("岭回归最优lambda (1se)：")
print(best_lambda_ridge_1se)
print("岭回归测试误差 (1se)：")
print(test_error_ridge_1se)


# (d) Lasso模型（含交叉验证选λ）与测试误差
# 交叉验证选λ（alpha=1为Lasso）
lasso_cv <- cv.glmnet(x_train, y_train, alpha = 1)  
best_lambda_lasso <- lasso_cv$lambda.min  # 最优λ

# 测试集预测与误差
pred_lasso <- predict(lasso_cv, newx = x_test, s = best_lambda_lasso)  
test_error_lasso <- mean((test$Apps - pred_lasso)^2)

# 统计非零系数个数（含截距）
coef_lasso <- predict(lasso_cv, type = "coefficients", s = best_lambda_lasso)  
non_zero_coef <- sum(coef_lasso != 0)  # 输出非零系数数量（如：8）

print("Lasso测试误差：")
print(test_error_lasso)  # 输出测试误差（通常比OLS小，且因特征选择更简洁）

# (e) 主成分回归（PCR）模型（含交叉验证选主成分数M）
library(pls)

# 训练PCR模型（scale=TRUE标准化预测变量）
pcr_model <- pcr(Apps ~ ., data = train, scale = TRUE, validation = "CV")

# 交叉验证选最优ncomp（使用MSEP函数获取最佳主成分数）
best_ncomp_pcr <- which.min(MSEP(pcr_model)$val[1, 1, ])  # 最优ncomp

# 测试集预测与误差
pred_pcr <- predict(pcr_model, newdata = test, ncomp = best_ncomp_pcr)
test_error_pcr <- mean((test$Apps - pred_pcr)^2)

print("最优主成分数：")
print(best_ncomp_pcr)  # 输出最优主成分数
print("主成分回归测试误差：")
print(test_error_pcr)    # 输出测试误差（降维后误差可能降低）

# 添加PLS模型训练
pls_model <- plsr(Apps ~ ., data = train, scale = TRUE, validation = "CV")  # 新增这一行

# 交叉验证选最优ncomp（正确访问MSEP的方法）
mse_values <- MSEP(pls_model)$val[1, 1, ]  # 现在可以正常工作
best_ncomp_pls <- which.min(mse_values)  # 最优ncomp

# 确保获得有效的主成分数
if(is.null(best_ncomp_pls) || length(best_ncomp_pls) == 0 || is.na(best_ncomp_pls)) {
    best_ncomp_pls <- 1
} else {
    best_ncomp_pls <- min(best_ncomp_pls, ncol(pls_model$loadings))  # 防止超出范围
}

# 测试集预测与误差
pred_pls <- predict(pls_model, newdata = test, ncomp = best_ncomp_pls)
test_error_pls <- mean((test$Apps - pred_pls)^2, na.rm = TRUE)

print("最优主成分数：")
print(best_ncomp_pls)  # 输出最优成分数（如：4）
print("偏最小二乘测试误差：")
print(test_error_pls)    # 输出测试误差（与PCR类似，降维后误差降低）

# 各模型详细分析
# 1. Lasso回归（略微最佳）
# 测试误差: 1,730,981（比OLS略好0.2%）
# 优势: 通过特征选择实现了轻微的性能提升
# 特点: 具有稀疏性，自动进行特征选择
# 2. 普通最小二乘法(OLS)和主成分回归(PCR)
# 测试误差: 1,734,841（完全相同）
# PCR最优主成分数: 17（几乎使用了所有成分）
# 分析: PCR使用了接近全部主成分，降维效果有限
# 3. 偏最小二乘法(PLS)
# 测试误差: 1,763,662（比OLS差约1.7%）
# 最优主成分数: 9
# 特点: 考虑了响应变量的信息进行降维
# 4. 岭回归
# lambda.min测试误差: 1,837,222（比OLS差约5.9%）
# lambda.1se测试误差: 3,154,965（明显较差）
# 分析:
# 使用 lambda.min 的岭回归表现尚可，但不如其他方法
# 使用 lambda.1se 的岭回归过度正则化，性能显著下降
# 岭回归中lambda.min和lambda.1se的区别
# lambda.min (15.2): 相对较小的正则化强度，测试误差为1,837,222
# lambda.1se (394.4): 更强的正则化强度，测试误差显著增加至3,154,965
# 结论: 在这个数据集中，较弱的正则化效果更好
# 模型准确性总体评估
# 1. 预测准确性
# 从实际应用角度看，这些模型的预测准确性相当不错：

# 所有模型的均方误差都在同一数量级
# 最佳模型间的差异非常小（<0.2%）
# 2. 模型间差异分析
# 最佳组: OLS、Lasso、PCR三者性能几乎相同
# 中等组: PLS略微逊色（差异约1.7%）
# 较差组: 岭回归明显落后（差异5.9%-82%）
# 3. 实用性建议
# 首选模型: Lasso回归

# 略优于其他方法
# 具有特征选择功能，模型更易解释
# 备选模型: OLS或PCR

# 性能与Lasso相当
# 实现简单，解释性强
# 需要改进: 岭回归

# 当前参数选择不佳
# 可能需要更细致的调参
# 结论
# 总体而言，这五种方法都能较好地预测大学申请人数，模型间差异不大。Lasso回归以微弱优势胜出，同时提供了特征选择的优势。在实际应用中，推荐使用Lasso回归，因为它在保持良好预测性能的同时，还能提供模型的可解释性。
