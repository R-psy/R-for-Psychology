library(tidyverse)
library(rio) # for importing

# Data manipulation
setwd("E:/R/plot")
data_raw <- import("data2R3.csv", setclass = "data.table")
data_long <- rbind( # watching
                    data.frame( FC="mPFC",  Intensity=data_raw$r1_watch1, Group="watch", Valence="Negative" ),
                    data.frame( FC="vmPFC",  Intensity=data_raw$r2_watch1, Group="watch", Valence="Negative" ),
                    data.frame( FC="Postcentral", Intensity=data_raw$r3_watch1, Group="watch", Valence="Negative"),
                    data.frame( FC="mPFC",  Intensity=data_raw$r1_watch2, Group="watch", Valence="Neutral" ),
                    data.frame( FC="vmPFC",  Intensity=data_raw$r2_watch2, Group="watch", Valence="Neutral" ),
                    data.frame( FC="Postcentral", Intensity=data_raw$r3_watch2, Group="watch", Valence="Neutral"),
                    # GI
                    data.frame( FC="mPFC",  Intensity=data_raw$r1_intention1, Group="GI", Valence="Negative" ),
                    data.frame( FC="vmPFC",  Intensity=data_raw$r2_intention1, Group="GI", Valence="Negative" ),
                    data.frame( FC="Postcentral", Intensity=data_raw$r3_intention1, Group="GI", Valence="Negative"),
                    data.frame( FC="mPFC",  Intensity=data_raw$r1_intention2, Group="GI", Valence="Neutral" ),
                    data.frame( FC="vmPFC",  Intensity=data_raw$r2_intention2, Group="GI", Valence="Neutral" ),
                    data.frame( FC="Postcentral", Intensity=data_raw$r3_intention2, Group="GI", Valence="Neutral"),
                    # RII
                    data.frame( FC="mPFC",  Intensity=data_raw$r1_im1, Group="RII", Valence="Negative" ),
                    data.frame( FC="vmPFC",  Intensity=data_raw$r2_im1, Group="RII", Valence="Negative" ),
                    data.frame( FC="Postcentral", Intensity=data_raw$r3_im1, Group="RII", Valence="Negative"),
                    data.frame( FC="mPFC",  Intensity=data_raw$r1_im2, Group="RII", Valence="Neutral" ),
                    data.frame( FC="vmPFC",  Intensity=data_raw$r2_im2, Group="RII", Valence="Neutral" ),
                    data.frame( FC="Postcentral", Intensity=data_raw$r3_im2, Group="RII", Valence="Neutral")
                    )

# Data summary from data_long to data_stat
n <- 26
data_long %>%  
  group_by(FC,Group, Valence) %>%
  summarise(
    means = mean(Intensity),
    se = sd(Intensity)/sqrt(n()) # 标准误值（SE）= 标准差SD/√n， n是样本量。
  ) %>%
  mutate(a = means -se, b = means + se) -> data_stat


p <- ggplot(data=data_stat, aes(x=Group, y=means,fill= Valence, shape=Valence, colour=Valence))
p <- p + facet_grid(. ~ FC) # 根据FC的类别形成多个绘图框
p <- p + geom_line(aes(group=Valence, linetype=Valence),size=1) # Set line by valence 
#p <- p + geom_errorbar(aes(ymin=means - a, ymax=means + b), width=0.8)
#p <- p + geom_errorbarh(aes(xmin=delay - xsem, xmax=delay + xsem), height=0.05) 
#p <- p + geom_errorbar(aes(ymin=Intensity - ysem, ymax=Intensity + ysem), width=0.8) 
p <- p + geom_point(size=4)  #  shape=21, colour="black",
p <- p + scale_colour_hue(l=30) # Use darker colors (lightness=30)
p <- p + scale_fill_manual(values=c("white","black")) 
p <-  p + labs(x = "Groups", y = "Beta values") # lab=lable
p <- p +  theme_bw() 
p

# Save plot
# Saving as .eps was the only way to preserve the file in both illustrator and acrobat.   # ggsave(g, file="Figure.eps", fonts=c("FONT FAMILIES USED", "Roboto Condensed", "Roboto Condensed Light"))
#ggsave("Figure_inter.pdf", plot = last_plot(), # or give ggplot object name as in myPlot,
#       width = 10, height = 5)  
ggsave("Figure 5_new.tiff", plot = p, width=10, height=5, 
       dpi=600, compression = "lzw") 