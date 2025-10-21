# (a) 拟合多元回归模型
library(ISLR)   # 加载包含Carseats数据集的包
data(Carseats)  # 加载数据集
fit_a <- lm(Sales ~ Price + Urban + US, data = Carseats)  # 拟合模型
print("多元回归模型系数:")
print(coef(fit_a))
print(summary(fit_a))
# (e) 拟合更小模型
fit_e <- lm(Sales ~ Price + US, data = Carseats)  # 仅保留显著变量
print("更小模型系数:")
print(coef(fit_e))
print(summary(fit_e))

# (g) 计算95%的置信区间
print(confint(fit_e))

# (h) 离群点和高杠杆点检测
rstudent_fit_e <- rstudent(fit_e)
outliers <- abs(rstudent_fit_e) > 3
print(sum(outliers))  # 统计离群点数量

hatvals <- hatvalues(fit_e)
high_leverage <- hatvals > (2 * 2)/400  # 2k/n = 0.01 # nolint: infix_spaces_linter, line_length_linter.
print(sum(high_leverage))  # 统计高杠杆点数量

cookd <- cooks.distance(fit_e)
influential <- cookd > 1
print(sum(influential))  # 统计强影响点数量