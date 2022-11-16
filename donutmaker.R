
# Load libraries
library(tidyverse)
library(svglite)
library(ggtext)
library(googlesheets4)

# Read data into R from Google Sheets
donutsmain <- read_sheet('https://docs.google.com/spreadsheets/d/1hv66GrSazsAs6QvlvOZpnHyJc4u6_tbLDQ5m3wMPwWw/edit#gid=1315766256') %>%
  select(1:15) # Drop the last three rows

# Form three tidy data frames
# Donuts track list
tracklist <- donutsmain %>%
  distinct(donutstrack, .keep_all = TRUE) %>%
  select(donutstrack:bpm)

# Unique sample list
samplelist <- donutsmain[!duplicated(donutsmain[c('donutstrack','sampledtrack')]),] %>%
  select(donutstrack, sampledtrack:sampledyear) %>%
  na.exclude %>%
  filter(!is.na(sampledyear))


# Define dimensions of the album donut
outer = 6       # Half of a 12" record
leadin = 5.75   # Start of the recording groove 5.75
leadout = 1.875 # End of the recording groove


# Set up the whole album's donut chart
# Compute percentages and cumulative percentages
tracklist$fraction = tracklist$length / sum(tracklist$length)
# ymax is the right (clockwise) edge of each slice
tracklist$ymax = cumsum(tracklist$fraction)
# ymin is the left (clockwise) edge of each slice
# Bump the max array by one position, add a zero up front
tracklist$ymin = c(0, head(tracklist$ymax, n=-1))

# Set up each sprinkle
latestyear = max(samplelist$sampledyear)
earliestyear = min(samplelist$sampledyear)
samplelist$place = 1 - ((samplelist$sampledyear-earliestyear) / (2006-earliestyear))
samplelist$groove = leadout + ((samplelist$place) * (leadin - leadout))
sprinklewidth = (1/10)

df = inner_join(tracklist, samplelist, by="donutstrack")

# Make the donut
donutrecord <- ggplot(df, aes(ymax = ymax, ymin = ymin, 
                               xmax = leadin, xmin = leadout)) + 
  geom_rect(fill = NA, color = 'gray77') + # rectangles that will be converted to 4-edged slices
  geom_rect(aes(ymax = ymax, ymin = ymin,
                xmax = groove,
                xmin = groove-sprinklewidth, 
                fill = sampledgenre), color = 'gray77') + #sprinkles
  scale_fill_manual(values = c("#E05822", "#F72585", "#F0AC24", "#4071FF", "#7F16E0"), 
                    expand = c(0, 0)) + #sprinklecolors
  coord_polar(theta = 'y') + # converts rectangles to radial slices
  xlim(c(0, outer)) + 
  theme_void() +
  theme(legend.position = 'none',
        plot.margin = margin(0,0,0,0))

print(donutrecord)                   

# Save the plot as an SVG
ggsave(file = "donutrecord.svg", plot = donutrecord, 
       width = 7, height = 7, dpi = 72)