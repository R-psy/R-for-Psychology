library(tidyverse)
library(rio) # for importing
library(ggsci) # for scale_color (散点图) and scale_fill(条形图)

# First, import your data
# Attention do not use Chinese character in path or data

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
 
# Third, 从data11原始数据中求出每个bar所需的平均值和SD ，然后再传递给data11中。
  data11 %<>%  # %<>% 复合赋值操作符
    group_by(G, session) %>%
    summarise(
      means = mean(dv),
      se = sd(dv)/sqrt(n())
    ) %>%
    mutate(a = means -se, b = means + se)
  
# Use ggolot2 to plot barplots based on summaries of data11
  p <-  ggplot(data = data11, aes(G,means,fill=session)) 
  p <-  p + geom_hline(yintercept=0, color="black", size=1) 
  p <-  p + geom_col(color = "white", position="dodge", alpha = 0.5) 
  p <-  p + geom_errorbar(aes(ymin = a, ymax= b), 
                          width=0.1, 
                          position=position_dodge(0.9), 
                          color="black") 
  p <-  p + labs(x = "Groups", y = "Emotional ratings", fill='Times') 
  source("theme_1.R")
  p <-  p + theme_1() # 应用主题
  p <-  p+ theme(axis.line.x = element_blank(), axis.ticks.x = element_blank()) # 去掉X轴和刻度线
  #p_npg = p + scale_fill_npg()
  #p_npg
  p_aaas = p + scale_fill_aaas()
  p_aaas
  
  # Save plot
  # Saving as .eps was the only way to preserve the file in both illustrator and acrobat.   # ggsave(g, file="Figure.eps", fonts=c("FONT FAMILIES USED", "Roboto Condensed", "Roboto Condensed Light"))
  ggsave("f2.jpg", 
         plot = last_plot(), # or give ggplot object name as in myPlot,
         width = 10, height = 5, 
         units = "in", # other options c("in", "cm", "mm"), 
         dpi = 300)  
  
  