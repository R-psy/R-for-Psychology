# https://xkdog.github.io/2017-11-28-literature-calc/

if (!require(tidyverse)) install.packages(tidyverse) # 载入tidyverse包，若无此包则先安装
reference_list <- read.csv("reference_list.csv") # 读入文件
reference_list <- mutate(
  reference_list,
  year = str_extract(reference_list$title, "(?<=\\()[0-9]{4}") %>%
    as.numeric()
)
mean(reference_list$year >= 2013, na.rm = TRUE) %>% 
  round(2) # 计算2013年以来（含）的文献占比，本保留两位小数