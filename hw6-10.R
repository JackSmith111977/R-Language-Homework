# 加载必要的包
library(leaps)  # 用于最优子集选择
library(ggplot2) # 用于绘图（可选，但增强可视化）

# 设置随机种子，确保结果可重复
set.seed(122)

# (a) 生成模拟数据集：p=20个特征，n=1000个观测，响应Y = Xβ + ε，其中β有些元素为0
n <- 1000  # 观测数
p <- 20    # 特征数
# 生成X矩阵：特征从标准正态分布中随机生成
X <- matrix(rnorm(n * p), nrow = n, ncol = p)
# 设置β向量：前5个系数为1（非零），后15个为0，确保真实模型稀疏（大小5）
beta <- c(rep(1, 5), rep(0, p - 5))
# 生成误差项ε，从标准正态分布中生成
epsilon <- rnorm(n)
# 计算响应Y
Y <- X %*% beta + epsilon
# 将X和Y组合为数据框
data <- data.frame(Y = Y, X = X)
# 分析：真实模型只包含前5个特征，其余特征与Y无关，用于模拟特征选择场景

# (b) 分割数据集：100个观测的训练集，900个观测的测试集
train_index <- sample(n, 100)  # 随机选择100个索引作为训练集
train <- data[train_index, ]
test <- data[-train_index, ]
# 分析：分割比例较小（10%训练），旨在突出过拟合问题，测试集大以确保误差估计稳定

# (c) 基于训练集使用最优子集选择法，计算不同模型大小下最优模型的训练集MSE
# 使用regsubsets函数进行最优子集选择，最大模型大小设为p=20
subset_fit <- regsubsets(Y ~ ., data = train, nvmax = p)
# 提取模型选择摘要
subset_summary <- summary(subset_fit)
# 计算每个模型大小（r从1到20）下最优模型的训练集MSE
train_mse <- subset_summary$rss / nrow(train)  # RSS除以训练集样本数得MSE
# 分析：训练MSE随模型大小增加而单调递减，因为更复杂模型总是更好地拟合训练数据
cat("训练集MSE：\n")
print(train_mse)

# (d) 计算不同模型大小下最优模型的测试集MSE
# 初始化向量存储测试MSE
test_mse <- numeric(p)
for (r in 1:p) {
    # 获取第r个特征数的最优模型系数
    coef_r <- coef(subset_fit, id = r)
    # 提取模型对应的特征名（包括截距）
    features <- names(coef_r)
    # 在测试集上预测：仅使用最优模型中的特征
    # 构建模型公式：如果特征包括截距，需处理
    if ("(Intercept)" %in% features) {
        # 有截距项的情况
        model_formula <- as.formula(paste("Y ~", paste(features[-1], collapse = " + ")))  # 移除截距名
    } else {
        # 无截距项的情况（但通常有截距）
        model_formula <- as.formula(paste("Y ~", paste(features, collapse = " + ")))
    }
    # 在训练集上拟合线性模型（仅用于获取系数，但regsubsets已提供系数，可直接预测）
    # 更简单方法：直接使用系数进行矩阵乘法预测
    # 准备测试集X矩阵（包括截距列）
    X_test <- model.matrix(Y ~ ., test)  # 包括所有特征，但后续只取相关列
    # 匹配系数对应的列
    coef_vec <- numeric(ncol(X_test))  # 初始化系数向量，长度等于特征数+1（含截距）
    coef_vec[] <- 0  # 初始为0
    # 将coef_r中的值赋到对应位置
    for (feat in features) {
        idx <- which(colnames(X_test) == feat)
        if (length(idx) > 0) coef_vec[idx] <- coef_r[feat]
    }
    # 测试集预测
    pred_test <- X_test %*% coef_vec
    # 计算测试MSE
    test_mse[r] <- mean((test$Y - pred_test)^2)
    cat("模型大小为", r, "的测试集MSE：", test_mse[r], "\n")
}
# 分析：测试MSE通常先减小后增大，呈现U形曲线，表明过拟合（模型过大时误差增加）

# (e) 找出测试集MSE最小时的模型特征数量，并解释
min_test_mse_index <- which.min(test_mse)  # 最小测试MSE对应的模型大小r
min_test_mse <- test_mse[min_test_mse_index]
cat("测试集MSE最小的模型大小为：", min_test_mse_index, "\n")

# 检查最小MSE模型是否为截距模型（r=0）或全模型（r=20），如果是则调整数据生成（见后）
# 当前设置下，min_test_mse_index应介于1和20之间（5），因为真实模型大小为5
# 解释：测试MSE最小时对应的模型大小（r=5）是偏差-方差权衡的最优点，过大或过小模型都会增加误差

# 如果最小测试MSE模型是截距（r=1时可能，但r从1开始）或全模型（r=20），需重新生成数据
# 但当前β设置（前5个非零）通常能避免此问题，无需调整。如需强制调整，可循环生成数据直到满足条件
# 例如：如果min_test_mse_index为1或20，则重新执行(a)部分，调整β（如增加非零系数数量）

# (f) 比较测试集MSE最小的模型与真实模型
# 获取最小测试MSE模型的系数估计
best_coef <- coef(subset_fit, id = min_test_mse_index)
# 真实系数β（注意：真实β中，前5个为1，后15个为0）
true_beta <- c(0, beta)  # 添加截距项（真实截距为0，因为生成数据时未设截距，但模型通常包括截距）
# 由于数据生成时Y = Xβ + ε，无额外截距，但线性模型会估计截距，因此比较时需对齐
# 解释：最小测试MSE模型应能近似真实模型（前5个系数非零），但可能包含一些无关变量（系数估计较小），或遗漏相关变量（估计偏差）
cat("最小测试MSE模型系数：\n")
print(best_coef)
cat("真实模型系数：\n")
print(true_beta)

# (g) 计算并绘制系数估计误差的平方根：sqrt(sum(beta_j - beta_hat_j)^2)
# 初始化向量存储系数误差
coef_error <- numeric(p)

for (r in 1:p) {
    coef_r <- coef(subset_fit, id = r)  # 获取第 r 个最优模型的系数
    # 构造长度为 p+1 的系数向量（含截距），其余为0
    coef_vec <- rep(0, p + 1)
    names(coef_vec) <- c("(Intercept)", paste("X", 1:p, sep=""))
    
    # 将当前模型的系数赋值到对应位置
    for (name in names(coef_r)) {
        idx <- which(names(coef_vec) == name)
        if (length(idx) > 0) {
            coef_vec[idx] <- coef_r[name]
        }
    }
    
    # 真实系数（包含截距项）
    true_beta_full <- c(0, beta)  # 截距为0，真实β前5个为1，其余为0
    
    # 计算系数误差的L2范数
    coef_error[r] <- sqrt(sum((true_beta_full - coef_vec)^2))
}

# 绘制图像
plot(1:p, coef_error, type = "l", col = "blue", lwd = 2,
     xlab = "模型大小 r", ylab = "系数估计误差 L2 范数",
     main = "系数估计误差 vs 模型大小")
grid()

# 绘制测试集 MSE 图像（补充）
plot(1:p, test_mse, type = "l", col = "red", lwd = 2,
     xlab = "模型大小 r", ylab = "测试集 MSE",
     main = "测试集 MSE vs 模型大小")
grid()