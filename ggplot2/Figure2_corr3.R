library(tidyverse)
library(rio) # import data
library(ggpubr)

# First, import your data
setwd("D:/R/plot") # 设置工作空间
data_raw <- import("data2R4.csv", setclass = "data.table") %>%
  # rename注意：修改的参数要放到一个向量中，格式为c(oldname="newname", oldname2="newname2")
  # 指定使用plyr中的rename函数
plyr::rename(c(FC1_corr2="R Putamen - L Rolandic Operculum",
               FC4_corr2="R Lingual Gyri - R Putamen",
               FC10_corr2="R Paracentral Lobule - R STG")) -> data_raw
source("theme_1.R")


# Second, reshape data

data_res <- data_raw %>% gather("FC_type", "FC_intensity",-Difficulty) 


#  plot pictures
p <- ggplot(data_res, aes(x = FC_intensity, y = Difficulty))  # aes中的x,y值分别表示在x,y轴的变量；
p <- p + facet_grid(. ~ FC_type) # 根据FC的类别形成多个绘图框
p <- p +  geom_count(size=2,color="black",stat = "sum")  # geom_point表示增加点图图层，其中的size控制点的大小，shape控制形状，一共25个，为0-25
p <- p +  scale_size_area() 
p <- p +  geom_smooth(method=lm)   # 添加线性回归线
p <- p +  xlab("FC intensity")  
p <- p +  ylab("Regulatory difficulty") 
#p <- p +  annotate("text", x=0.5, y=2.5, label="r=0.54, p=0.002") 
p <- p +  theme_1() # 加载自定义主题，也可以使用ggplot2默认的主题theme_bw()


# Third, save plot
ggsave("Fig 8.tiff", plot = p, width=13, height=5, 
       dpi=600, compression = "lzw") 

#ggsave("Fig 8.pdf", plot = p_all,width=13, height=5) 