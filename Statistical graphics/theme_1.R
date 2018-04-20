##' Parameters passed to theme_classic() function.
##' Color string (default 'white') for user defined uniform background.

theme_1 <- function(..., bg='white', textcolor="black"){
  require(grid) # ggplot2依赖网格图像系统（grid）进行绘图
  theme_classic(...) +
    theme(rect=element_rect(fill=bg),
          plot.background = element_rect(fill = "white", colour = "white"),
          # panel绘图面板
          panel.background = element_rect(fill = NA),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          # legend图释
          #legend.title = element_text(colour="black", size=10, face="bold"),
          # legend.position="right", #  legend.position : “left”,“top”, “right”, “bottom”.
          #legend.background = element_rect(fill = NA),
          # text所有文本属性. 
          text = element_text(family="Arial", colour = "black", size=14), 
          # axis坐标轴 .text标签 .line轴线 .ticks刻度线 .length长度
          axis.text = element_text(family="Arial", colour = "black", size=14),
          axis.line = element_line(color=textcolor, size = 1)
          # 设置坐标点
  )}

