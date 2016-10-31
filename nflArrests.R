nfl <- read.csv("nfl_arrests_2011-2015.csv", header = TRUE)
head(nfl)
str(nfl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggrepel)
library(ggthemes)

max(nfl$arrests, na.rm = TRUE)
nfl %>% filter(is.na(arrests))
nfl %>% filter(day_of_week == "Wednesday")
nfl[is.na(nfl)] <- 0

seas <- data.frame(nfl %>% group_by(season, home_team) %>% summarise(sum(arrests)))
head(seas)
seas <- rename(seas, arrests = sum.arrests.)

pos <- position_jitter(width = 0.3)

num <- round(seas$arrests / 8, digits = 1)

seas <- seas %>% mutate(homeArrests = paste(home_team, sep = " ", as.character(num)))
seas2 <- seas %>% filter(season %in% c(2011, 2015))

plot <-  ggplot(seas2, aes(season, arrests, group = home_team)) + geom_point(size = 2, show.legend = F, color = "wheat3") + geom_line(show.legend = F, alpha = 0.17, color = "navy") + geom_text_repel(aes(label = homeArrests), show.legend = F, alpha = 0.75, size = 5, color = "dodgerblue3") + theme_fivethirtyeight()
plot <- plot + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.text.x = element_text(color = "#3d3d3d", size = 13), panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.minor.x = element_blank()) 
plot <- plot + labs(x = "Season", y = "Arrests", title = "Average number of arrests per game at NFL home games") + scale_y_log10()
plot + scale_x_continuous(breaks = c(2011, 2015), labels = c("2011", "2015"))
