library(extrafont)
library(tidyverse)
library(rio) # for importing
library(ggsci) # for scale_color (散点图) and scale_fill(条形图)

# First, import your data
# Attention do not use Chinese character in path or data
font_import("C:/Windows/Fonts/", pattern = "RobotoCondensed", prompt = F)
setwd("E:/R/plot")
data1 <- import("data2R1.csv", setclass = "data.table")


# Second, 先对数据进行整理

S1 <- data1$S1neg1 - data1$S1neu1
S2 <- data1$S2neg1 - data1$S2neu1
S3 <- data1$S3neg1 - data1$S1neu1

n <- 40
data11<- tibble(
  session = c(rep(c("First"), n),rep(c("Second"), n),rep(c("Three"), n)),
  G = rep(c(rep(c("watching"), each = 20),rep(c("GI"), each = 20)),3))
data11$dv <-  c(S1,S2,S3) 
data_raw = data11

# Third, 从data11原始数据中求出每个bar所需的平均值和SD ，然后再传递给data_stat
data11 %>%  
  group_by(G, session) %>%
  summarise(
    means = mean(dv),
    se = sd(dv)/sqrt(n())
  ) %>%
  mutate(a = means -se, b = means + se) -> data_stat

# Use ggolot2 to plot ViolinBox Plot based on raw data
p <-  ggplot(data = data_raw, aes(G, dv, fill=session)) 
p <- p +  geom_hline(yintercept=0, color="black", size=1)
p <- p + geom_violin(color="black", trim=FALSE, alpha = 0.5)
p <- p + geom_boxplot(color = "black", width = 0.25, 
                      position = position_dodge(width=0.9))
p <- p + labs(x = "Groups", y = "Emotional ratings", fill='Times') 
p <- p + ylim(-1, 9)
source("theme_1.R")
p <-  p + theme_1()
p <-  p+ theme(axis.line.x = element_blank(), axis.ticks.x = element_blank()) # 去掉X轴和刻度线
p_aaas = p + scale_fill_aaas()
p_aaas

# Save plot
ggsave("f1.pdf", 
       plot = last_plot(), # or give ggplot object name as in myPlot,
       width = 10, height = 5)  
# ggsave("Fig 4a.tiff", plot = p4, width=600, height=450, units="mm", dpi=300, compression = "lzw") 
# other units options c("in", "cm", "mm"), 