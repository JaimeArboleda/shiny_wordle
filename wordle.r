library("tidyverse")
setwd("~/projects/R/wordle")
data <- read.csv("./wordle_puntos.csv")
plays.count <- data %>% 
  group_by(persona) %>% 
  summarize(
    plays = n()
  )
data <- data[data['persona'] != 'Eva',]
data.grouped <- data %>% 
  group_by(persona, puntos) %>% 
  summarize(
    score = n() / plays.count[plays.count$'persona' == cur_group()[1][[1]],'plays'][[1]]
  )

g <- ggplot(data = data.grouped, aes(x = persona, y = puntos, size = score))
g + geom_point() + scale_size(range=c(0,9))
    