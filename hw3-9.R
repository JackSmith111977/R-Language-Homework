# (a) 加载ISLR包
library(ISLR)
# 绘制所有变量散点图
pairs(Auto)

# (b) 计算相关系数矩阵
## 提取除name列外的变量
x <- Auto[, -which(names(Auto) == "name")]
## 计算相关系数矩阵
cor_matrix <- cor(x)
## 打印相关系数矩阵
print(cor_matrix)

# (c) 多元线性回归建模并分析结果
model_all <- lm(mpg ~ ., data = Auto)
print("多元线性回归建模结果:")
print(summary(model_all))

# (d) 回归诊断图与异常点分析
## 生成诊断图
par(mfrow = c(2, 2))
plot(model_all)

# (e) 交互作用模型与显著性检验
model_interact <- lm(mpg ~ displacement * horsepower, data = Auto)
print("交互作用模型结果:")
print(summary(model_interact))

# (f) 预测变量的变换与结果分析
## 1. 对数变换
model_log <- lm(mpg ~ log(weight), data = Auto)
print("对数变换模型结果:")
print(summary(model_log))
## 2. 平方根变换
model_sqrt <- lm(mpg ~ sqrt(weight), data = Auto)
print("平方根变换模型结果:")
print(summary(model_sqrt))
## 3. 平方项（二次多项式）
model_poly <- lm(mpg ~ poly(weight, 2), data = Auto)
print("平方项（二次多项式）模型结果:")
print(summary(model_poly))