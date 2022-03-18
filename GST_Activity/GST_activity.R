library(tidyverse)
library(ggplot2)

absorption.data <- read.csv("abs_data.csv")
absorption.data.method <- absorption.data %>% filter(str_detect(Property, pattern = "pH=6.5"))
absorption.data.pH <- absorption.data %>% filter(str_detect(Property, pattern = "Electric"))

p <- ggplot(data=absorption.data.method,mapping=aes(x=time,y=Abs, group=Property, color=Property)) + geom_point() +stat_smooth(method = 'loess', se = F, formula = "y ~ x") + scale_y_continuous(breaks = seq(0,1.5,0.1)) + scale_x_continuous(breaks = seq(0,4,0.5)) + labs(x = "Time (min)", y = "Absorption value (Wavelength = 340 nm)") + guides(color=guide_legend(title = NULL)) + theme(legend.position="top")

p

plot_data <- ggplot_build(p)$data
p2 <- plot_data[[2]]

q <- ggplot(data=absorption.data.pH,mapping=aes(x=time,y=Abs, group=Property, color=Property)) + geom_point() +stat_smooth(method = 'loess', se = F, formula = "y ~ x") + scale_y_continuous(breaks = seq(0,1.5,0.1)) + scale_x_continuous(breaks = seq(0,4,0.5)) + labs(x = "Time (min)", y = "Absorption value (Wavelength = 340 nm)") + guides(color=guide_legend(title = NULL)) + theme(legend.position="top")

q

plot_data <- ggplot_build(q)$data
q2 <- plot_data[[2]]

if(0) {
  abs.g <- absorption.data %>% filter(str_detect(Property, pattern = "Glass"))
  abs.e <- absorption.data %>% filter(str_detect(Property, pattern = "Electric pH=6.5"))
  abs.6.5 <- abs.e
  abs.b <- absorption.data %>% filter(str_detect(Property, pattern = "BioSample"))
  abs.6.0 <- absorption.data %>% filter(str_detect(Property, pattern = "pH=6.0"))
  abs.7.0 <- absorption.data %>% filter(str_detect(Property, pattern = "pH=7.0"))
  abs.7.5 <- absorption.data %>% filter(str_detect(Property, pattern = "7.5"))
  
  predict(loess(abs.g$Abs ~ abs.g$time, abs.g), abs.g$time)
}



