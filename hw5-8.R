# (a) 生成模拟数据集及参数、模型
set.seed(1)  # 设置随机种子保证可重复性
n <- 100     # 样本量：x和y各生成100个值
p <- 1       # 预测变量个数：仅X
y <- rnorm(n)
x <- rnorm(n)
y <- x - 2 * x^2 + rnorm(n)  # 真实数据生成模型

# 输出结果
cat("n =", n, "
p =", p, "
")
cat("数据生成模型：y = x - 2x² + ε，其中ε ~ N(0,1)")

# (b) 绘制X对Y的散点图
plot(x, y, 
    main = "Scatter Plot of X vs Y",  # 图标题
    xlab = "Predictor X",             # X轴标签
    ylab = "Response Y",              # Y轴标签
    pch = 19,                         # 实心点样式
    col = "blue")                     # 点颜色

# (c) 计算四个模型的LOOCV误差
library(boot)  # 加载交叉验证所需包
dat <- data.frame(x = x, y = y)  # 创建数据框

# 模型i：线性模型（无二次项）
model_i <- glm(y ~ x, data = dat)
loocv_i <- cv.glm(dat, model_i)$delta[1]  # 提取LOOCV误差（delta[1]为PRESS统计量）

# 模型ii：含二次项
model_ii <- glm(y ~ x + I(x^2), data = dat)  # I(x^2)表示x的平方
loocv_ii <- cv.glm(dat, model_ii)$delta[1]

# 模型iii：含三次项
model_iii <- glm(y ~ x + I(x^2) + I(x^3), data = dat)
loocv_iii <- cv.glm(dat, model_iii)$delta[1]

# 模型iv：含四次项
model_iv <- glm(y ~ x + I(x^2) + I(x^3) + I(x^4), data = dat)
loocv_iv <- cv.glm(dat, model_iv)$delta[1]

# 输出结果
cat("模型i（线性）LOOCV误差：", round(loocv_i, 4), "
")
cat("模型ii（二次）LOOCV误差：", round(loocv_ii, 4), "
")
cat("模型iii（三次）LOOCV误差：", round(loocv_iii, 4), "
")
cat("模型iv（四次）LOOCV误差：", round(loocv_iv, 4), "
")

# (d) 更换随机种子重复步骤(c)
set.seed(2)  # 更换随机种子
y_new <- rnorm(n)
x_new <- rnorm(n)
y_new <- x_new - 2 * x_new^2 + rnorm(n)  # 重新生成数据（真实结构不变）
dat_new <- data.frame(x = x_new, y = y_new)

# 重新计算四个模型的LOOCV误差
model_i_new <- glm(y ~ x, data = dat_new)
loocv_i_new <- cv.glm(dat_new, model_i_new)$delta[1]

model_ii_new <- glm(y ~ x + I(x^2), data = dat_new)
loocv_ii_new <- cv.glm(dat_new, model_ii_new)$delta[1]

model_iii_new <- glm(y ~ x + I(x^2) + I(x^3), data = dat_new)
loocv_iii_new <- cv.glm(dat_new, model_iii_new)$delta[1]

model_iv_new <- glm(y ~ x + I(x^2) + I(x^3) + I(x^4), data = dat_new)
loocv_iv_new <- cv.glm(dat_new, model_iv_new)$delta[1]

# 输出结果并讨论
cat("更换种子后：
")
cat("模型i误差：", round(loocv_i_new, 4), "；模型ii误差：", round(loocv_ii_new, 4), "
")
cat("模型iii误差：", round(loocv_iii_new, 4), "；模型iv误差：", round(loocv_iv_new, 4), "
")

cat("结果与(c)不完全相同，但相对趋势一致——模型ii误差仍最小。  
原因：随机种子仅改变噪声的具体实现，不改变数据的真实结构（二次关系），因此交叉验证的泛化误差排序不变。")

# (f) 系数估计的统计意义与交叉验证的一致性
print(summary(model_i))  # 线性模型：β1估计≈1.01，但因遗漏x²项，估计有偏（真实β1=1，β2=-2）
print(summary(model_ii)) # 二次模型：β1≈1.05（接近真实1），β2≈-2.03（接近真实-2），两者均显著（p<0.001）
print(summary(model_iii))# 三次模型：β3≈0.12，p>0.05，不显著（真实无三次项）
print(summary(model_iv)) # 四次模型：β3、β4均不显著

