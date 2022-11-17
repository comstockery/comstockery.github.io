
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

# Sample instances list
instancelist <- donutsmain %>%
  select(donutstrack, trackname, sampledtrack:duration) %>%
  select(-c(sampledyear, sampledelement))


# Start making donuts!
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
samplelist$radius = 1 - ((samplelist$sampledyear-earliestyear) / (2006-earliestyear))
samplelist$groove = leadout + ((samplelist$radius) * (leadin - leadout))
sprinklewidth = (1/10)



df = inner_join(tracklist, samplelist, by="donutstrack")

df$sprinkle = df$ymin + ((df$ymax - df$ymin)*(runif(nrow(df), 0.10, 0.9)))


genres = count(df, sampledgenre)

# Make the donut
donutrecord <- ggplot(df) + 
  geom_rect(aes(ymax = ymax, ymin = ymin, 
                xmax = leadin, xmin = leadout), # rectangles that will be converted to 4-edged slices
            fill = NA, color = 'gray77') + 
  # geom_rect(aes(ymax = ymax, ymin = ymin,
  #               xmax = groove,
  #               xmin = groove-sprinklewidth, 
  #               fill = sampledgenre), color = 'gray77') + #widesprinkles
  geom_point(aes(x = groove, y = sprinkle,
                 fill = sampledgenre), 
             shape = 21, color = 'gray90', size = 5, stroke = 0.6) + #dotsprinkles
  scale_fill_manual(values = c("#E67343", "#F72585", "#F8B219", "#004AF7", "#9B53E6"), 
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