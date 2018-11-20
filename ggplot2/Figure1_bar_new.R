library(plyr)
library(tidyverse)
library(rio) # for importing
library(ggsci) # for scale_color (散点图) and scale_fill(条形图)
library(ggpubr)
library(gridExtra)
library(ggsignif) # Create significance layer


# First, import your raw data
# Attention do not use Chinese character in path or data
setwd("D:/R/plot3")

data_raw <- import("DataF23.csv", setclass = "data.table")

data_long <- gather(data_raw,Measures,value,-Intensity, -group) # 短变长

data_long <- filter(data_long, Measures == "Neg_valence" | Measures == "Neg_arousal" | Measures == "Neu_valence" | Measures == "Neu_arousal") 

# Second, 原始数据中求出每个bar所需的平均值和SD
# val1=menas, val2=se,标准误值（SE）= 标准差SD/√n， n是样本量。a是误差线上端，b是下端。
data_plot <- ddply(data_long,.(group, Intensity, Measures),summarize, 
                   val1 = mean(value), 
                   val2=sd(value, na.rm = TRUE)/sqrt(40)) %>%
  mutate(a = val1 -val2, b = val1 + val2) -> data_plot
# 第二步的平均值和标准误计算，也可以使用dplyr包的group_by()和summarize函数，搭配管道符号%>%。
# 但是dplyr的这两个函数写起来比plyr包的ddply这个函数要多几行，不如后者简洁。


# low-intensity Arousal
  data_p1 <- filter(data_plot, Intensity=="Low",  Measures == "Neg_arousal" | Measures == "Neu_arousal")
  data_p1$group <- ordered(data_p1$group, levels = c("Watch","Reapp","RII")) #按照喜好对数据进行排序。
  p1 <-  ggplot(data_p1, aes(group,val1, fill=Measures))
  p1 <-  p1 + geom_hline(yintercept=0, color="black", size=1)
  p1 <-  p1 + geom_col(position="dodge", alpha = 1) # with a coloured (black!) border
  p1 <-  p1 + geom_errorbar(aes(ymin = a, ymax= b),
                          width=0.1,
                          position=position_dodge(0.9),
                          color="black")
  p1 <-  p1 + labs(title="Arousal", x = "Low-intensity Groups", y = "Arousal Rating",fill="Picture") # lab=lable
  p1 <- p1 + scale_x_discrete(labels=c('Watch','Reappraisal','RII'))
  p1 <- p1 +  scale_y_continuous( breaks=seq(1,8,1)) # Y轴原点和取值范围
  p1 <- p1 +  coord_cartesian(ylim=c(1,8))
  
  # 应用主题
  source("theme_1.R")
  p1 <-  p1 + theme_1()
  # p1 <-  p1 +  theme(axis.line.x = element_blank(), axis.ticks.x = element_blank()) # 去掉X轴和刻度线
  p1_aaas <- p1 + scale_fill_lancet(labels=c("Negative", "Neutral")) # 通过lable修改图例内容

  #######################################
  ######################################
  
  # low-intensity Valence
  data_p2 <- filter(data_plot, Intensity=="Low",  Measures == "Neg_valence" | Measures == "Neu_valence")
  data_p2$group <- ordered(data_p1$group, levels = c("Watch","Reapp","RII"))
  p2 <-  ggplot(data_p2, aes(group,val1, fill=Measures))
  p2 <-  p2 + geom_hline(yintercept=0, color="black", size=1)
  p2 <-  p2 + geom_col(position="dodge", alpha = 1) # with a coloured (black!) border
  p2 <-  p2 + geom_errorbar(aes(ymin = a, ymax= b),
                            width=0.1,
                            position=position_dodge(0.9),
                            color="black")
  p2 <-  p2 + labs(title="Valence", x = "Low-intensity Groups", y = "Valence Rating",fill="Picture") # lab=lable
  p2 <- p2 + scale_x_discrete(labels=c('Watch','Reappraisal','RII'))
  p2 <- p2 +  scale_y_continuous( breaks=seq(1,8,1)) # Y轴原点和取值范围
  p2 <- p2 +  coord_cartesian(ylim=c(1,8))
  # 应用主题
  source("theme_1.R")
  p2 <-  p2 + theme_1()
  #p2 <-  p2 +  theme(axis.line.x = element_blank(), axis.ticks.x = element_blank()) # 去掉X轴和刻度线
  p2_aaas = p2 + scale_fill_lancet(labels=c("Negative", "Neutral"))

  # High-intensity Arousal
  data_p3 <- filter(data_plot, Intensity=="High",  Measures == "Neg_arousal" | Measures == "Neu_arousal")
  data_p3$group <- ordered(data_p3$group, levels = c("Watch","Reapp","RII"))
  p3 <-  ggplot(data_p3, aes(group,val1, fill=Measures))
  p3 <-  p3 + geom_hline(yintercept=0, color="black", size=1)
  p3 <-  p3 + geom_col(position="dodge", alpha = 1) # with a coloured (black!) border
  p3 <-  p3 + geom_errorbar(aes(ymin = a, ymax= b),
                            width=0.1,
                            position=position_dodge(0.9),
                            color="black")
  p3 <-  p3 + labs( x = "High-intensity Groups", y = "Arousal Rating",fill="Picture") # lab=lable
  p3 <- p3 + scale_x_discrete(labels=c('Watch','Reappraisal','RII'))
  p3 <- p3 +  scale_y_continuous(breaks=seq(1,8,1)) # Y轴原点和取值范围
  p3 <- p3 +  coord_cartesian(ylim=c(1,8))
  # 应用主题
  source("theme_1.R")
  p3 <-  p3 + theme_1()
  # p3 <-  p3 +  theme(axis.line.x = element_blank(), axis.ticks.x = element_blank()) # 去掉X轴和刻度线
  p3_aaas = p3 + scale_fill_aaas(labels=c("Negative", "Neutral"))
  
  
  #######################################
  ######################################
  
  # High-intensity Valence
  data_p4 <- filter(data_plot, Intensity=="High",  Measures == "Neg_valence" | Measures == "Neu_valence")
  data_p4$group <- ordered(data_p3$group, levels = c("Watch","Reapp","RII"))
  p4 <-  ggplot(data_p4, aes(group,val1, fill=Measures))
  p4 <-  p4 + geom_hline(yintercept=0, color="black", size=1)
  p4 <-  p4 + geom_col(position="dodge", alpha = 1) # with a coloured (black!) border
  p4 <-  p4 + geom_errorbar(aes(ymin = a, ymax= b),
                            width=0.1,
                            position=position_dodge(0.9),
                            color="black")
  p4 <-  p4 + labs( x = "High-intensity Groups", y = "Valence Rating",fill="Picture") # lab=lable
  p4 <- p4 + scale_x_discrete(labels=c('Watch','Reappraisal','RII'))
  p4 <- p4 +  scale_y_continuous( breaks=seq(1,8,1)) # Y轴原点和取值范围
  p4 <- p4 +  coord_cartesian(ylim=c(1,8))
  # 应用主题
  source("theme_1.R")
  p4 <-  p4 + theme_1()
  #p4 <-  p4 +  theme(axis.line.x = element_blank(), axis.ticks.x = element_blank()) # 去掉X轴和刻度线
  p4_aaas = p4 + scale_fill_aaas(labels=c("Negative", "Neutral"))
 
 ######################
 # Combine p1  p2 p3 p4

  p_all <- ggarrange(p1_aaas,p2_aaas,p3_aaas,p4_aaas, ncol=2, nrow = 2,
                     labels = c("A","B","C","D"))

  # Save plot
  ggsave("Figure2.svg",
         width=10, height=6)
  ggsave('Fig2.tiff', width=10, height=6, unit='in', dpi=300)
