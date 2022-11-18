
# Load libraries
library(tidyverse)
library(svglite)
library(ggtext)
library(googlesheets4)
library(emojifont)

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
leadin = 5.75   # Start of the recording groove =11.5"
leadout = 1.88 # End of the recording groove 3.75
inner = 1.69    # End of spiral 3.375



# Set up the whole album's donut chart
# Compute percentages and cumulative percentages
tracklist$fraction = tracklist$length / sum(tracklist$length)
# ymax is the right (clockwise) edge of each slice
tracklist$ymax = cumsum(tracklist$fraction)
# ymin is the left (clockwise) edge of each slice
# Bump the max array by one position, add a zero up front
tracklist$ymin = c(0, head(tracklist$ymax, n=-1))

# Set up each sprinkle (dots)
# Time goes from the outer edge (confusingly: lead in) to inner edge (lead out)
# Scale time from 1955 to 2005 for ease of axes 
# latestyear = max(samplelist$sampledyear)
# earliestyear = min(samplelist$sampledyear)
latestyear = 2005
earliestyear = 1955
samplelist$radius = 1 - ((samplelist$sampledyear-earliestyear) / (latestyear-earliestyear))
samplelist$groove = leadout + ((samplelist$radius) * (leadin - leadout))
# sprinklewidth = (1/10)

# Create a function to translate years to groove positions (timeline)
vlinecalc <- function(year) {
  vlinegroove <- leadout + ((1 - ((year - earliestyear) / (latestyear-earliestyear))) * (leadin-leadout))
  return(vlinegroove)
}

# samplelist$groove = vlinecalc(samplelist$sampleyear)

# Join datasets to map samples to the Donut tracklist
df = inner_join(tracklist, samplelist, by='donutstrack')

# Place sprinkles within their slice, adding randomness to avoid overplotting
df$sprinkle = df$ymin + ((df$ymax - df$ymin)*(runif(nrow(df), 0.10, 0.90)))


# Make a table for year labels
yearlabels <- data.frame(
  label = c(1955, 1970, 1990, 2005),
  x= vlinecalc(c(1955, 1970, 1990, 2005)),
  y = 1,
  angle = 0)


# Check that there are only 5 genres in dataset
genres = count(df, sampledgenre)

# Make the donut
donutrecord <- ggplot(df) + 
  # Create two sets of rectangles that will be converted to 4-edged slices
  # First, the rectangles for the track slices
  geom_rect(aes(ymax = ymax, ymin = ymin, 
                xmax = leadin, xmin = leadout), 
            fill = NA, color = 'gray77', 
            linewidth = 0.3) +
  # Overlay the rectangles for the total record (outer and inner edges) 
  geom_rect(aes(ymax = 0, ymin = 1,
                xmax = outer, xmin = inner),
            fill = NA, color = 'gray33', 
            linewidth = 0.5) +
  
  # Add gridlines for certain years (grooves)
  geom_vline(xintercept = vlinecalc(c(1970,1990)), color = 'gray77') +
  # geom_rect(aes(ymax = ymax, ymin = ymin,
  #               xmax = groove,
  #               xmin = groove-sprinklewidth, 
  #               fill = sampledgenre), color = 'gray77') + #widesprinkles
  
  # Add labels for gridlines (grooves)
  geom_richtext(data = yearlabels,
                mapping = aes(x = x, y = y, label = label,
                              hjust = 0.5, vjust = 0.5,
                              angle = angle),
                color = 'gray50', fill = 'white', 
                label.color = NA,
                size = 6,
                label.padding = grid::unit(rep(0, 4), "pt")) +
 
  # Add dots representing samples in each track (sprinkles)
  geom_point(aes(x = groove, y = sprinkle,
                 fill = sampledgenre), 
             shape = 21, color = 'gray90', size = 5, stroke = 0.6) + 
  
  # Add colors for the sprinkles and remove plot buffers
  scale_fill_manual(values = c("#E67343", "#F72585", "#F8B219", "#004AF7", "#9B53E6")) + 

  # geom_text(aes(x = groove, y = sprinkle, 
  #               label = emoji('cow')), family="OpenSansEmoji", size=5) + #emoji?
  
  # Converts rectangles to radial slices
  coord_polar(theta = 'y') + 
  
  # Remove any gaps around the plot edge 
  scale_x_continuous(limits = c(0, 6), expand = expansion(0,0)) +
  scale_y_continuous(limits = c(0, 1), expand = expansion(0,0)) + 
  
  # Remove other plot elements and legend
  theme_void() +
  theme(legend.position = 'none')

donutrecord


# Save the plot as an SVG
ggsave(file = "donutrecord.svg", plot = donutrecord, 
       width = 7, height = 7, dpi = 72)