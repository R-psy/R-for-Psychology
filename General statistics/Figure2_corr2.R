library(tidyverse)
library(rio) # import data
library(ggpubr)

# First, import your data
setwd("D:/R/plot") # 设置工作空间
data_raw <- import("data2R4.csv", setclass = "data.table")
source("theme_1.R")


# Second, plot picture1

x1 <- data_raw$FC1_corr2
y <- data_raw$Difficulty
df1 <- data.frame(x = x1, y = y)

p1 <- ggplot(df1, aes(x = x, y = y)) + # aes中的x,y值分别表示在x,y轴的变量；
      geom_count(size=2,color="black",stat = "sum") + # geom_point表示增加点图图层，其中的size控制点的大小，shape控制形状，一共25个，为0-25
      scale_size_area() +
      geom_smooth(method=lm) +   # 添加线性回归线
      xlab("R Putamen - L Rolandic Operculum") +  
      ylab("Regulatory difficulty") +
      annotate("text", x=0.5, y=2.5, label="r=0.54, p=0.002") +
      theme_1() # 加载自定义主题，也可以使用ggplot2默认的主题theme_bw()

#  plot picture2


x2 <- data_raw$FC4_corr2
y <- data_raw$Difficulty
df2 <- data.frame(x = x2, y = y)

p2 <- ggplot(df2, aes(x = x, y = y)) + # aes中的x,y值分别表示在x,y轴的变量；
  geom_point(size=2,color="black") + # geom_point表示增加点图图层，其中的size控制点的大小，shape控制形状，一共25个，为0-25。
  geom_smooth(method=lm) +   # 添加线性回归线
  xlab("R Lingual Gyri - R Putamen") +  
  ylab("Regulatory difficulty") +
  annotate("text", x=0.5, y=2.5, label="r=0.35, p=0.04") +
  theme_1() # 加载自定义主题，也可以使用ggplot2默认的主题theme_bw()

# plot picture3

x3 <- data_raw$FC10_corr2
y <- data_raw$Difficulty
df3 <- data.frame(x = x3, y = y)

p3 <- ggplot(df3, aes(x = x, y = y)) + # aes中的x,y值分别表示在x,y轴的变量；
  geom_point(size=2,color="black") + # geom_point表示增加点图图层，其中的size控制点的大小，shape控制形状，一共25个，为0-25。
  geom_smooth(method=lm) +   # 添加线性回归线
  xlab("R Paracentral Lobule - R STG") +  
  ylab("Regulatory difficulty") +
  annotate("text", x=0.5, y=2.5, label="r=0.41, p=0.02") +
  theme_1() # 加载自定义主题，也可以使用ggplot2默认的主题theme_bw()

#  use ggpubr
p_all <- ggarrange(p1, p2, p3, ncol=3, nrow = 1,
         labels = c("A", "B", "C"))
p_all <- annotate_figure(p_all,
                bottom = text_grob("FC intensity", color = "black",
                                   face = "bold", size = 12, family="serif")
                )

# Third, save plot
ggsave("Fig 8.tiff", plot = p_all, width=13, height=5, 
       dpi=600, compression = "lzw") 

#ggsave("Fig 8.pdf", plot = p_all,width=13, height=5) 