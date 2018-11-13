##' A nice-looking ggplot2 theme: inward axis ticks, legend title excluded, and uniform background.
##' @title A nice-looking ggplot2 theme
##' @param ...
##' Parameters passed to theme_classic() function.
##' @param bg
##' Color string (default 'white') for user defined uniform background.
##' @return
##' ggplot2 theme object.
##' @example
##' library(ggplot2)
##' qplot(x=carat, y=price, color=cut, data=diamonds) + theme_zg()
##' @author ZG Zhao
##' @export
theme_0 <- function(..., bg='white'){
  require(grid)
  theme_classic(...) +
    theme(rect=element_rect(fill=bg),
          plot.margin=unit(rep(0.5,4), 'lines'),
          panel.background=element_rect(fill='transparent', color = "black"),
          panel.border=element_rect(fill='transparent', color='transparent'),
          panel.grid=element_blank(),
          axis.title = element_text(color= "black", vjust=0.1),
          axis.ticks.length = unit(-0.4,"lines"), # 刻度线长度
          axis.ticks = element_line(color= "black"),
          axis.text = element_text(color= "black", size = 15), # 刻度线和刻度标签之间的间距rel(0.8)
          legend.title=element_blank(),
          legend.key=element_rect(fill='transparent', color='transparent'))
}