library(tidyverse)
library(rio) # import data
library(Rmisc) # multiplot

# First, import your data
setwd("E:/R/plot") # 设置工作空间
data2 <- import("data2R2.csv", setclass = "data.table")


# Second, plot data2
source("theme_1.R") # 加载自定义主题函数
x <- data2$FC8_corr1
y <- data2$Regulatory_effects
df <- data.frame(x = x, y = y)

p1 <- ggplot(df, aes(x = x, y = y)) + # aes中的x,y值分别表示在x,y轴的变量；
      geom_point(size=2,color="black") + # geom_point表示增加点图图层，其中的size控制点的大小，shape控制形状，一共25个，为0-25。
      geom_smooth(method=lm) +   # 添加线性回归线
      xlab("FC") +  
      ylab("Emotion regulatory effects") +
      theme_1() # 加载自定义主题，也可以使用ggplot2默认的主题theme_bw()
# p2 <- ggplot(df, aes(x = x, y = y))
# multiplot(p1, p2, p3, p4, cols=2)


# Third, save plot
ggsave("Figure_9.jpg", 
       plot = last_plot(), # or give ggplot object name as in myPlot,
       width = 5, height = 5, 
       units = "in", # other options c("in", "cm", "mm"), 
       dpi = 300)  