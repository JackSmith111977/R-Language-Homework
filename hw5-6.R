# (a)计算标准差
library(ISLR)
model <- glm(default ~ income + balance, data = Default, family = binomial)
print(summary(model)$coefficients[, "Std. Error"])

# ​(b) 编写boot.fn()函数​​
boot.fn <- function(data, index) {
    subset_data <- data[index, ]  # 根据序号筛选子集
    fit <- glm(default ~ income + balance, data = subset_data, family = binomial)
    return(coef(fit)[c("income", "balance")])  # 返回目标变量的系数
}

# ​(c) 用boot()计算自助法标准误差​​
set.seed(123)  # 设置随机种子
library(boot)
boot_result <- boot(Default, boot.fn, R = 1000)  # R为自助样本量
boot_result$std.error  # 提取自助法估计的标准误差
print(boot_result)