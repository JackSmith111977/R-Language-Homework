set.seed(1)
x <- rnorm(100)
y <- 2 * x + rnorm(100) # 真实β=2，无截距
fit <- lm(y ~ x + 0)
summary(fit)

fit2 <- lm(x ~ y + 0)
summary(fit2)

# 正确的手动t统计量计算
beta_hat <- sum(x * y) / sum(x^2)
residuals <- y - beta_hat * x
sigma_squared <- sum(residuals^2) / (length(x) - 1)
se_beta <- sqrt(sigma_squared / sum(x^2))
t_manual <- beta_hat / se_beta

# 使用all.equal进行比较
all.equal(t_manual, summary(fit)$coefficients[1, 3])

fit_yx_int <- lm(y ~ x)
fit_xy_int <- lm(x ~ y)
t_yx_int <- coef(summary(fit_yx_int))[2, 3]
t_xy_int <- coef(summary(fit_xy_int))[2, 3]
t_yx_int == t_xy_int  # 结果为TRUE