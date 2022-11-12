
# Load libraries
library(tidyverse)
library(svglite)
library(ggtext)
library(googlesheets4)

# Read in data
tracks <- read_csv('donutstracks.csv', show_col_types = FALSE)
samples <- read_csv('donutssamples.csv', show_col_types = FALSE)


# Define dimensions of the album donut
outer = 6       # Half of a 12" record
leadin = 5.75   # Start of the recording groove
leadout = 1.875 # End of the recording groove


# Set up the whole album's donut chart
# Compute percentages and cumulative percentages
tracks$fraction = tracks$length / sum(tracks$length)
# ymax is the right (clockwise) edge of each slice
tracks$ymax = cumsum(tracks$fraction)
# ymin is the left (clockwise) edge of each slice
# Bump the max array by one position, add a zero up front
tracks$ymin = c(0, head(tracks$ymax, n=-1))

# Set up each sprinkle
latestyear = max(samples$sampleyear)
earliestyear = min(samples$sampleyear)
samples$place = 1 - ((samples$sampleyear-earliestyear) / (2006-earliestyear))
samples$groove = leadout + ((samples$place) * (leadin - leadout))
sprinklewidth = (1/20)

df = inner_join(tracks, samples, by="donutstrack")

genres = count(df, samplegenre)

# Make the donut
donutrecord <- ggplot(df, aes(ymax = ymax, ymin = ymin, 
                               xmax = leadin, xmin = leadout)) + 
  geom_rect(fill = NA, color = 'gray66') + # rectangles that will be converted to 4-edged slices
  geom_rect(aes(ymax = ymax, ymin = ymin,
                xmax = groove+sprinklewidth,
                xmin = groove-sprinklewidth, 
                fill = samplegenre)) +
  coord_polar(theta = 'y') + # converts rectangles to radial slices
  xlim(c(0, outer)) + 
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