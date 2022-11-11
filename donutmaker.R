
# Load libraries
library(tidyverse)
library(svglite)
library(ggtext)
library(googlesheets4)

# Read in data
tracks <- read_csv('donutstracks.csv', show_col_types = FALSE)



# Set up the whole album donut chart
# Compute percentages and cumulative percentages
tracks$fraction = tracks$length / sum(tracks$length)
# ymax is the right (clockwise) edge of each slice
tracks$ymax = cumsum(tracks$fraction)
# ymin is the left (clockwise) edge of each slice
tracks$ymin = c(0, head(tracks$ymax, n=-1))

# Make the plot
donutrecord <- ggplot(tracks, aes(ymax=ymax, ymin=ymin, 
                               xmax=5.75, xmin=1.875, # diameters of recording spiral
                               fill = BPM)) + 
  geom_rect(fill = NA, color = 'gray66') + # rectangles that will be converted to 4-edged slices
  coord_polar(theta='y') + # converts rectangles to radial slices
  xlim(c(0, 6)) + # radius of a 12" record
  theme_void() +
  theme(legend.position = 'none')

print(donutrecord)                   

# Save the plot as a 12" SVG
ggsave(file="donutrecord.svg", plot=donutrecord, width=9, height=9)

# donutbox <- ggplot(data = tracks, 
#                    aes(x = reorder(trackname, donutstrack, 
#                                    order = is.ordered(trackname), decreasing = FALSE), 
#                        y = length)) + 
#   geom_bar(stat = "identity") +
#   scale_y_continuous(name = "track length in seconds") +
#   coord_flip() +
#   theme_minimal() +
#   theme(axis.title.y = element_blank()) 
# 
# print(donutbox)                   
# 
# ggsave(file="donutbox.svg", plot=donutbox, width=7, height=7)