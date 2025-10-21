set.seed(1) # 固定随机种子，保证结果一致

# (a) 生成x：100个N(0,1)观测
x <- rnorm(100)

# (b) 生成eps：100个N(0, 0.25)观测（方差0.25 → 标准差0.5）
eps <- rnorm(100, mean = 0, sd = sqrt(0.25))

# (c) 生成y：根据模型 Y = -1 + 0.5X + ε
y <- -1 + 0.5 * x + eps

# 向量y的长度与模型参数
length(y) # 输出100
cat("真实模型参数：β₀ =", -1, "，β₁ =", 0.5, "\n")

# (d) 绘制散点图
plot(x, y,
    main = "Scatterplot of x vs y", # nolint
    xlab = "x", ylab = "y",
    pch = 16, col = "darkgray"
)

# (e) 拟合模型 y ~ x
fit <- lm(y ~ x)
summary(fit) # 查看回归结果

# (f) 叠加回归线
plot(x, y,
    main = "Scatterplot with Regression Lines",
    xlab = "x", ylab = "y"
)

# 最小二乘拟合线（红色）
abline(fit, col = "red", lwd = 2)

# 真实回归线（蓝色，Y = -1 + 0.5X）
abline(-1, 0.5, col = "blue", lwd = 2, lty = 2)

legend("topleft",
    legend = c("Least Squares Line", "Population Regression Line"),
    col = c("red", "blue"), lty = c(1, 2), lwd = 2
)


# (g) 拟合模型 y ~ x + I(x^2)
fit_poly <- lm(y ~ x + I(x^2))  
summary(fit_poly)

# 生成低噪声数据
eps_small <- rnorm(100, 0, 0.1) # 方差0.01（原方差0.25→更小）
y_small <- -1 + 0.5 * x + eps_small

# 重复(d)-(f)：散点图+回归线
plot(x, y_small,
    main = "Low Noise: Scatterplot with Regression Lines",
    xlab = "x", ylab = "y"
)
abline(lm(y_small ~ x), col = "red")
abline(-1, 0.5, col = "blue", lty = 2)
legend("topleft", legend = c("LS Line", "True Line"), col = c("red", "blue"), lty = c(1, 2))


# 生成高噪声数据
eps_large <- rnorm(100, 0, 1) # 方差1（原方差0.25→更大）
y_large <- -1 + 0.5 * x + eps_large

# 重复(d)-(f)：散点图+回归线
plot(x, y_large,
    main = "High Noise: Scatterplot with Regression Lines",
    xlab = "x", ylab = "y"
)
abline(lm(y_large ~ x), col = "red")
abline(-1, 0.5, col = "blue", lty = 2)
legend("topleft", legend = c("LS Line", "True Line"), col = c("red", "blue"), lty = c(1, 2))

# 原始数据置信区间
conf_int_orig <- confint(fit)

# 高噪声数据拟合与置信区间
fit_large <- lm(y_large ~ x)
conf_int_large <- confint(fit_large)

# 低噪声数据拟合与置信区间
fit_small <- lm(y_small ~ x)
conf_int_small <- confint(fit_small)

# 打印置信区间宽度对比
cat("原始数据置信区间宽度（β₀）：", conf_int_orig[1, 2] - conf_int_orig[1, 1], "\n")
cat("高噪声数据置信区间宽度（β₀）：", conf_int_large[1, 2] - conf_int_large[1, 1], "\n")
cat("低噪声数据置信区间宽度（β₀）：", conf_int_small[1, 2] - conf_int_small[1, 1], "\n")
