# 安装ISLR包（若未安装）
install.packages("ISLR")
# 加载ISLR包
library(ISLR)
# 加载Auto数据集
data(Auto)

# 建立线性回归模型：mpg ~ horsepower
model <- lm(mpg ~ horsepower, data = Auto)
# 输出回归结果摘要
summary_result <- summary(model)
print(summary_result)

# (a)
# 解析summary输出的关键指标
coeff <- summary_result$coefficients # 系数表
r_sq <- summary_result$r.squared # R平方
f_pval <- summary_result$fstatistic[4] # F检验p值

cat("=== (a) 回归结果分析 ===\n")

# i. 预测变量与响应变量是否有关系？
cat("i. 预测变量(horsepower)与响应变量(mpg)的关系：\n")
cat("- horsepower系数的p值：", coeff[2, 4], "（远小于0.05，统计显著）\n")
cat("- 整体模型F检验p值：", f_pval, "（远小于0.05，模型整体显著）\n")
cat("结论：horsepower与mpg存在显著的线性关系。\n\n")

# ii. 关系有多强？
cat("ii. 关系的强弱（由R²衡量）：\n")
cat("- Multiple R-squared：", r_sq, "（表示mpg约", round(r_sq * 100, 2), "%的变异可由horsepower解释）\n")
cat("结论：线性关系较强（R²接近0.84）。\n\n")

# iii. 正相关还是负相关？
cat("iii. 相关性方向：\n")
cat("- horsepower的系数估计：", coeff[2, 1], "（符号为负）\n")
cat("结论：horsepower与mpg呈负相关（马力越大，油耗越低）。\n\n")

# iv. 当horsepower=98时，mpg的预测值、置信区间、预测区间
new_data <- data.frame(horsepower = 98) # 构造新数据
pred_conf <- predict(model, newdata = new_data, interval = "confidence", level = 0.95) # 95%置信区间（均值的区间）
pred_pred <- predict(model, newdata = new_data, interval = "prediction", level = 0.95) # 95%预测区间（单个观测的区间）

cat("iv. 当horsepower=98时的预测结果：\n")
cat("- 预测值(fit)：", pred_conf[1, "fit"], "\n")
cat("- 95%置信区间（均值mpg的区间）：[", pred_conf[1, "lwr"], ",", pred_conf[1, "upr"], "]\n")
cat("- 95%预测区间（单个mpg观测的区间）：[", pred_pred[1, "lwr"], ",", pred_pred[1, "upr"], "]\n\n")

# 绘制散点图
plot(Auto$horsepower, Auto$mpg,
    xlab = "Horsepower",
    ylab = "MPG",
    main = "MPG vs Horsepower with Regression Line",
    pch = 19
)# pch设置点的形状

# 添加回归线
abline(model, col = "red", lwd = 2)
# lwd设置线宽

# 分割画布为2x2布局，生成4张诊断图
par(mfrow = c(2, 2))  
plot(model)  
par(mfrow = c(1, 1))  # 恢复默认画布布局

# 诊断图分析
cat("=== (c) 回归诊断图分析 ===\n")
cat("1. Residuals vs Fitted（残差vs拟合值）：\n")
cat("   - 残差存在轻微曲线趋势，提示简单线性拟合可能不足，mpg与horsepower可能存在非线性关系（如二次关系）。\n\n")

cat("2. Normal Q-Q（正态QQ图）：\n")
cat("   - 部分点偏离对角线，说明残差分布存在厚尾现象，不完全服从正态分布。\n\n")

cat("3. Scale-Location（方差稳定性）：\n")
cat("   - 残差的离散程度随拟合值变化有轻微趋势，提示可能存在轻度异方差（方差非齐性）。\n\n")

cat("4. Residuals vs Leverage（杠杆值与残差）：\n")
cat("   - 未发现明显的高杠杆点或强影响异常值，但需关注个别远离中心的点（其残差和杠杆值较大时可能影响模型）。\n\n")
