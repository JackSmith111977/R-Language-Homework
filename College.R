library(ggplot2)
#(a)
college <- read.csv("F:/学习资料/高级统计方法/作业/College.csv")
#(b)
rownames(college) <- college[,1]
college <- college[,-1]
fix(college)

#(c)
#1
summary_college <- summary(college)
summary_college

#2
pairs(college[,2:10])

#3
ggplot(college, aes(x=college$Private, y=college$Outstate)) + 
geom_boxplot() + labs(x = "学校类型", y = "外州学生学费", title = "公立与私立学校的外州学费比较")

#4
college = read.csv("F:/学习资料/高级统计方法/作业/College.csv")
Elite = rep("No",nrow(college))
Elite[college$Top10perc > 50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college,Elite)

summary_Elite = summary(college$Elite)
summary_Elite
plot(college$Elite, college$Outstate)

#5
par(mfrow = c(2, 2))
hist(college$Expend, breaks = 20,  col = 2)
hist(college$Grad.Rate, breaks = 20, col = 3)
hist(college$Room.Board, breaks = 20, col = 4)
hist(college$Books, breaks = 20, col = 5 )
par(mfrow = c(1, 1))

#6
par(mfrow = c(1,2))
plot(college$Accept,college$Enroll)
plot(college$Top25perc,college$Top10perc)
par(mfrow = c(1,2))
#一般来说，录取的学生越多，入学的学生也越多，所以应该存在正相关关系。但是，入学率（Enroll/Accept）因学校而异。一些学校可能录取很多学生但只有少数入学，而另一些学校可能录取较少但入学率较高。
#点应该沿着一条从左下到右上的趋势线分布。如果点分散，表明入学率变化很大。如果点聚集在一条线附近，表明入学率相对稳定。

#第二张图探索新生中来自前25%高中毕业生的比例与前10%高中毕业生
#的比例之间的关系。由于前10%的学生是前25%的子集，我们预期看到一个强烈的正相关关系。即，拥有更多前25%学生的学校，通常也拥有更多前10%的学生。
