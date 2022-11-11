
# 
library(tidyverse)
library(svglite)
library(ggtext)
library(googlesheets4)



tracks <- read_csv('donutstracks.csv',
                   show_col_types = FALSE)

donutbox <- ggplot(data = tracks, 
                   aes(x = reorder(trackname, donutstrack, 
                                   order = is.ordered(trackname), decreasing = FALSE), 
                       y = length)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal()

print(donutbox)                   

ggsave(file="donutbox.svg", plot=donutbox, width=2, height=2)