# (a) 总体均值μ^​的估计
library(MASS)
data(Boston)
mu_hat <- mean(Boston$medv)
print(mu_hat)

# (b) μ^​的标准误差估计
n <- nrow(Boston)
se_sample <- sd(Boston$medv) / sqrt(n)
print(se_sample)

# (c) 自助法估计μ^​的标准误差
set.seed(123)  # 固定随机种子（可选，保证可重复性）
B <- 1000
bootstrap_means <- numeric(B)
for (i in 1:B) {
    sample_idx <- sample(n, n, replace = TRUE)  # 有放回抽样
    bootstrap_means[i] <- mean(Boston$medv[sample_idx])
}
se_bootstrap <- sd(bootstrap_means)
print(se_bootstrap)

# (d) 自助法95%置信区间与t.test结果对比
print("自助法95%置信区间:")
ci_bootstrap <- mu_hat + c(-2, 2) * se_bootstrap
print(ci_bootstrap)

print("t.test结果:")
t_test <- t.test(Boston$medv)
t_test_ci <- t_test$conf.int
print(t_test_ci)

# (e) 总体中位数μ^​med​的估计
median_medv <- median(Boston$medv)
print(median_medv)

# (f) 自助法估计中位数μ^​med​的标准误差及讨论
set.seed(123)
B <- 1000
bootstrap_medians <- numeric(B)
for (i in 1:B) {
    sample_idx <- sample(n, n, replace = TRUE)
    bootstrap_medians[i] <- median(Boston$medv[sample_idx])
}
se_median_bootstrap <- sd(bootstrap_medians)
print(se_median_bootstrap)

# (g) 10%分位数μ^​0.1​的估计
quantile_01 <- quantile(Boston$medv, probs = 0.1)
print(quantile_01)

# (h) 自助法估计10%分位数μ^​0.1​的标准误差及讨论
set.seed(123)
B <- 1000
bootstrap_quantiles <- numeric(B)
for (i in 1:B) {
    sample_idx <- sample(n, n, replace = TRUE)
    bootstrap_quantiles[i] <- quantile(Boston$medv[sample_idx], probs = 0.1)
}
se_quantile_bootstrap <- sd(bootstrap_quantiles)
print(se_quantile_bootstrap)