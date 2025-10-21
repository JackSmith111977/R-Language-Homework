# (a)
# 设置随机种子保证可重复性
set.seed(1)

# 生成自变量x1（均匀分布）
x1 <- runif(100)

# 生成自变量x2（x1的线性变换 + 小噪声）
x2 <- 0.5 * x1 + rnorm(100) / 10

# 生成因变量y（线性模型 + 噪声）
y <- 2 + 2 * x1 + 0.3 * x2 + rnorm(100) # nolint


# 线性模型的函数形式：
# $$ y = \beta_0 + \beta_1 x_1 + \beta_2 x_2 + \varepsilon $$
# 其中，真实回归系数为：$\beta_0=2$，$\beta_1=2$，$\beta_2=0.3$（由代码中y的构造逻辑直接确定）

# (b)
# 计算相关系数
cor_x1x2 <- cor(x1, x2)
cat("x1与x2的相关系数：", cor_x1x2, "\n")
cat("y的均值是", mean(y), "\n")

# 绘制散点图（辅助观察线性趋势）
plot(x1, x2,
    main = "Scatter Plot of x1 vs x2",
    xlab = "x1",
    ylab = "x2",
    pch = 16,
    col = "blue"
)
# 添加回归线（辅助观察线性趋势）
abline(lm(x2 ~ x1), col = "red", lwd = 2)

# (c)
# 拟合模型：y ~ x1 + x2
fit_c <- lm(y ~ x1 + x2)

# 输出回归结果摘要（含系数、显著性检验等）
print(summary(fit_c))


# 提取并打印系数（也可通过coef(fit_c)直接获取）
coef_c <- coef(fit_c)
cat("多元回归(c)估计系数：\n")
print(coef_c)

# (d)
# 拟合模型：y ~ x1
fit_d <- lm(y ~ x1)

# 输出回归结果摘要
print(summary(fit_d))

# (e)
# 拟合模型：y ~ x2
fit_e <- lm(y ~ x2)  

# 输出回归结果摘要
print(summary(fit_e))

# (g)
# 构造新观测（错误测量后的数据）
x1_new <- c(x1, 0.1)   # 原x1 + 错误测量值0.1
x2_new <- c(x2, 0.8)   # 原x2 + 错误测量值0.8
y_new <- c(y, 6)       # 原y + 错误测量值6  


# 重新拟合三个模型
fit_c_new <- lm(y_new ~ x1_new + x2_new)  # 多元回归
fit_d_new <- lm(y_new ~ x1_new)           # 仅x1
fit_e_new <- lm(y_new ~ x2_new)           # 仅x2  

# (g)
# --------------- 分析新观测的“离群点”与“高杠杆点”属性 ---------------
## 1. 离群点：学生化残差（rstudent）绝对值大（通常>2或3）  
rstudent_c_new <- rstudent(fit_c_new)  # 多元回归的学生化残差
rstudent_d_new <- rstudent(fit_d_new)  # 单变量x1的学生化残差
rstudent_e_new <- rstudent(fit_e_new)  # 单变量x2的学生化残差

## 2. 高杠杆点：杠杆值（hatvalues）远大于阈值（2p/n，p=2, n=101 → 2 * 2/101≈0.0396）  
hatvalues_c_new <- hatvalues(fit_c_new)  # 多元回归的杠杆值  


# 输出新观测（第101个数据点）的诊断结果
cat("\n===== 新观测对模型的影响分析 =====\n")
cat("(g)-1 多元回归(c) 新观测学生化残差：", rstudent_c_new[101], "\n")
cat("(g)-2 单变量x1(d) 新观测学生化残差：", rstudent_d_new[101], "\n")
cat("(g)-3 单变量x2(e) 新观测学生化残差：", rstudent_e_new[101], "\n")

cat("(g)-4 多元回归(c) 新观测杠杆值：", hatvalues_c_new[101], "\n")
cat("   高杠杆点阈值（2p/n）：", 2 * 2/101, "\n")


# 结合诊断结果解释：
# - 新观测y=6远高于原数据均值（原y均值≈3.15），学生化残差绝对值极大 → **离群点**；  
# - 新观测(x1=0.1, x2=0.8)在自变量空间中远离原数据分布 → 杠杆值远>阈值 → **高杠杆点**；  
# 因此，该观测**既是离群点，又是高杠杆点**，会严重扭曲回归系数估计。
