import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import Circle

# 设置中文字体和图像大小
plt.rcParams['font.sans-serif'] = ['SimHei']  # 用来正常显示中文标签
plt.rcParams['axes.unicode_minus'] = False  # 用来正常显示负号
plt.rcParams['figure.figsize'] = (12, 5)

# 创建图形和子图
fig, (ax1, ax2) = plt.subplots(1, 2)

# ================================
# (a) 岭回归目标函数 Q(β₁) = (y₁ - β₁)² + λβ₁²
# ================================

# 选择参数
y1_ridge = 2
lambda_ridge = 1

# 计算解析解（式6.14）
beta1_hat_ridge = y1_ridge / (1 + lambda_ridge)

# 生成β₁的值
beta1_values = np.linspace(-1, 3, 500)

# 计算岭回归目标函数值
Q_ridge = (y1_ridge - beta1_values)**2 + lambda_ridge * beta1_values**2

# 绘制岭回归目标函数
ax1.plot(beta1_values, Q_ridge, 'b-', linewidth=2, label='岭回归目标函数')

# 标记最小值点
min_index_ridge = np.argmin(Q_ridge)
ax1.plot(beta1_values[min_index_ridge], Q_ridge[min_index_ridge], 'ro', markersize=8, 
         label=f'最小值点: β₁ = {beta1_hat_ridge:.2f}')

# 添加垂直参考线
ax1.axvline(x=beta1_hat_ridge, color='r', linestyle='--', alpha=0.7)

# 设置图表属性
ax1.set_xlabel('β₁')
ax1.set_ylabel('Q(β₁)')
ax1.set_title('(a) 岭回归目标函数验证\nQ(β₁) = (y₁ - β₁)² + λβ₁²')
ax1.legend()
ax1.grid(True, alpha=0.3)

# 添加文本框显示参数和解析解
text_ridge = f'参数:\ny₁ = {y1_ridge}, λ = {lambda_ridge}\n\n解析解 (式6.14):\nβ̂₁ = y₁/(1+λ)\n= {beta1_hat_ridge:.2f}'
ax1.text(0.02, 0.98, text_ridge, transform=ax1.transAxes, verticalalignment='top',
         bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.8))

# ================================
# (b) Lasso目标函数 Q(β₁) = (y₁ - β₁)² + λ|β₁|
# ================================

# 选择参数（展示两种情况）
y1_lasso1 = 1.5  # 情况1：|y₁| > λ/2
y1_lasso2 = 0.5  # 情况2：|y₁| ≤ λ/2
lambda_lasso = 2

# 计算解析解（式6.15）
if y1_lasso1 > lambda_lasso/2:
    beta1_hat_lasso1 = y1_lasso1 - lambda_lasso/2
else:
    beta1_hat_lasso1 = 0

if abs(y1_lasso2) <= lambda_lasso/2:
    beta1_hat_lasso2 = 0
else:
    beta1_hat_lasso2 = y1_lasso2 + lambda_lasso/2 if y1_lasso2 < 0 else y1_lasso2 - lambda_lasso/2

# 生成β₁的值
beta1_values_lasso = np.linspace(-2, 2, 1000)

# 计算Lasso目标函数值（分两段）
Q_lasso1_pos = (y1_lasso1 - beta1_values_lasso[beta1_values_lasso >= 0])**2 + lambda_lasso * beta1_values_lasso[beta1_values_lasso >= 0]
Q_lasso1_neg = (y1_lasso1 - beta1_values_lasso[beta1_values_lasso < 0])**2 - lambda_lasso * beta1_values_lasso[beta1_values_lasso < 0]

Q_lasso2_pos = (y1_lasso2 - beta1_values_lasso[beta1_values_lasso >= 0])**2 + lambda_lasso * beta1_values_lasso[beta1_values_lasso >= 0]
Q_lasso2_neg = (y1_lasso2 - beta1_values_lasso[beta1_values_lasso < 0])**2 - lambda_lasso * beta1_values_lasso[beta1_values_lasso < 0]

# 绘制Lasso目标函数
# 情况1：y₁ = 1.5
ax2.plot(beta1_values_lasso[beta1_values_lasso >= 0], Q_lasso1_pos, 'g-', linewidth=2, label='Lasso (y₁=1.5), β₁≥0')
ax2.plot(beta1_values_lasso[beta1_values_lasso < 0], Q_lasso1_neg, 'g-', linewidth=2, label='Lasso (y₁=1.5), β₁<0')

# 情况2：y₁ = 0.5
ax2.plot(beta1_values_lasso[beta1_values_lasso >= 0], Q_lasso2_pos, 'purple', linewidth=2, linestyle='--', label='Lasso (y₁=0.5), β₁≥0')
ax2.plot(beta1_values_lasso[beta1_values_lasso < 0], Q_lasso2_neg, 'purple', linewidth=2, linestyle='--', label='Lasso (y₁=0.5), β₁<0')

# 标记最小值点
# 情况1的最小值点
min_val1 = min(np.min(Q_lasso1_pos), np.min(Q_lasso1_neg))
ax2.plot(beta1_hat_lasso1, (y1_lasso1 - beta1_hat_lasso1)**2 + lambda_lasso * abs(beta1_hat_lasso1), 'ro', markersize=8, 
         label=f'最小值点1: β₁ = {beta1_hat_lasso1:.2f}')

# 情况2的最小值点
min_val2 = min(np.min(Q_lasso2_pos), np.min(Q_lasso2_neg))
ax2.plot(beta1_hat_lasso2, (y1_lasso2 - beta1_hat_lasso2)**2 + lambda_lasso * abs(beta1_hat_lasso2), 'mo', markersize=8, 
         label=f'最小值点2: β₁ = {beta1_hat_lasso2:.2f}')

# 添加垂直参考线
ax2.axvline(x=beta1_hat_lasso1, color='r', linestyle='--', alpha=0.7)
ax2.axvline(x=beta1_hat_lasso2, color='m', linestyle='--', alpha=0.7)

# 标记λ/2边界
ax2.axvline(x=lambda_lasso/2, color='gray', linestyle=':', alpha=0.5, label='λ/2边界')
ax2.axvline(x=-lambda_lasso/2, color='gray', linestyle=':', alpha=0.5)

# 设置图表属性
ax2.set_xlabel('β₁')
ax2.set_ylabel('Q(β₁)')
ax2.set_title('(b) Lasso目标函数验证\nQ(β₁) = (y₁ - β₁)² + λ|β₁|')
ax2.legend()
ax2.grid(True, alpha=0.3)

# 添加文本框显示参数和解析解
text_lasso = f'参数:\nλ = {lambda_lasso}\n\n情况1: y₁ = {y1_lasso1}\n|y₁| > λ/2 = {lambda_lasso/2}\nβ̂₁ = y₁ - λ/2 = {beta1_hat_lasso1:.2f}\n\n情况2: y₁ = {y1_lasso2}\n|y₁| ≤ λ/2 = {lambda_lasso/2}\nβ̂₁ = 0'
ax2.text(0.02, 0.98, text_lasso, transform=ax2.transAxes, verticalalignment='top',
         bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.8))

# 调整布局并显示
plt.tight_layout()
plt.show()

# 打印验证结果
print("="*50)
print("验证结果:")
print("="*50)
print("(a) 岭回归验证:")
print(f"目标函数最小值点: β₁ = {beta1_values[min_index_ridge]:.4f}")
print(f"解析解 (式6.14): β̂₁ = {beta1_hat_ridge:.4f}")
print(f"验证结果: {'通过' if abs(beta1_values[min_index_ridge] - beta1_hat_ridge) < 0.01 else '未通过'}")

print("\n(b) Lasso验证:")
print(f"情况1 (y₁={y1_lasso1}):")
print(f"目标函数最小值点: β₁ = {beta1_hat_lasso1:.4f}")
print(f"解析解 (式6.15): β̂₁ = {beta1_hat_lasso1:.4f}")
print(f"验证结果: {'通过' if abs(beta1_hat_lasso1 - (y1_lasso1 - lambda_lasso/2)) < 0.01 else '未通过'}")

print(f"\n情况2 (y₁={y1_lasso2}):")
print(f"目标函数最小值点: β₁ = {beta1_hat_lasso2:.4f}")
print(f"解析解 (式6.15): β̂₁ = 0")
print(f"验证结果: {'通过' if abs(beta1_hat_lasso2) < 0.01 else '未通过'}")