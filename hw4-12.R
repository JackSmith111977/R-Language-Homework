# (a) 编写Power()函数，计算2的3次方
Power <- function() {
  result <- 2^3
  print(result)
}
Power()

# (b) 创建Power2()函数，计算x的a次方
Power2 <- function(x, a) {
  result <- x^a
  print(result)
}

# (c) 使用Power2()计算10^3, 8^7, 131^3
Power2(10, 3)
Power2(8, 7)
Power2(131, 3)

# (d) 创建Power3()函数，返回计算结果
Power3 <- function(x, a) {
  result <- x^a
  return(result)
}

# (e) 使用Power3()绘制f(x)=x^2的图像
x_values <- 1:10
y_values <- Power3(x_values, 2)
plot(x_values, y_values, type = "b", main = "f(x) = x^2", xlab = "x", ylab = "x^2")

# (f) 创建PlotPower()函数，绘制x与x^a的关系图像
PlotPower <- function(x_range, a) {
  x_values <- x_range
  y_values <- Power3(x_values, a)
  plot(x_values, y_values, type = "b", main = paste("x vs x^", a), xlab = "x", ylab = paste("x^", a))
}

# 调用PlotPower函数
PlotPower(1:10, 3)