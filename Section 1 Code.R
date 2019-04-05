setwd("~/GitHub/MMSS_311_2")


packages <- c("dplyr", "ggplot2", "stringr", "lubridate")
install.packages(packages)

library(ggplot2)


qog <- read.csv("http://www.qogdata.pol.gu.se/data/qog_std_ts_jan19.csv")

print(dim(qog))
