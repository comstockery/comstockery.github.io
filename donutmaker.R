
# Load libraries
library(tidyverse)
library(svglite)
library(ggtext)
library(googlesheets4)
library(emojifont)

# Flags for code chunks
datamaker = TRUE; # TRUE at the start of a session; otherwise FALSE
donutmaker = FALSE; # TRUE to generate main album donut; otherwise FALSE


if(datamaker) {

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

} # End of the datamaker code chunk

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

# Join datasets to map samples to the Donut tracklist
df1 = inner_join(tracklist, samplelist, by='donutstrack')

# Place sprinkles within their slice, adding randomness to avoid overplotting
df1$sprinkle = df1$ymin + ((df1$ymax - df1$ymin)*(runif(nrow(df1), 0.10, 0.90)))


# Make a table for year labels
yearlabels <- data.frame(
  label = c(1955, 1970, 1990, 2005),
  x= vlinecalc(c(1955, 1970, 1990, 2005)),
  y = 1,
  angle = 0)

# Count the genres in dataset
genres = count(df1, sampledgenre)


# Make the donut for entire album, but only if the donutmaker is TRUE!
if(donutmaker) {

donutrecord <- ggplot(df1) + 
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
            linewidth = 0.6) +
  
  # Add gridlines for certain years (grooves)
  geom_vline(xintercept = vlinecalc(c(1970,1990)), color = 'gray77') +

  # Add labels for gridlines (grooves)
  geom_richtext(data = yearlabels,
                mapping = aes(x = x, y = y, label = label,
                              hjust = 0.5, vjust = 0.5,
                              angle = angle),
                color = 'gray50', fill = NA, 
                label.color = NA,
                size = 4,
                label.padding = unit(rep(0, 4), "pt")) +
 
  # Add dots representing samples in each track (sprinkles)
  geom_point(aes(x = groove, y = sprinkle,
                 fill = sampledgenre), 
             shape = 21, color = 'gray90', size = 5, stroke = 0.6) + 
  
  # Add colors for the sprinkles and remove plot buffers
  scale_fill_manual(values = c("#E67343", "#F72585", "#F8B219", "#004AF7", "#9B53E6")) + 

  # geom_text(aes(x = groove, y = sprinkle, 
  #               label = emoji('cow')), family="OpenSansEmoji", size=5) + #emoji?
  
  # Remove any gaps around the plot edge 
  scale_x_continuous(limits = c(0, 6), expand = expansion(0,0)) +
  scale_y_continuous(limits = c(0, 1), expand = expansion(0,0),
                     labels = NULL) +
  
# Converts rectangles to radial slices
  coord_polar(theta = 'y') +
  # Remove other plot elements and legend
  theme_void() +
  theme(legend.position = 'none', 
        panel.border = element_blank(),
        legend.key = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        panel.grid  = element_blank(),
        plot.margin = unit(rep(-0.5,4), "inches"))

print(donutrecord)

# Save the plot as an SVG
ggsave(file = "donutrecord.svg", plot = donutrecord, 
       width = 7, height = 7, dpi = 72)

} # End of the 'donutmaker' code chunk


# Make a timeline of the sampled songs' release years (count, stacked by genre)
df2 <- samplelist %>%
  distinct(sampledtrack, .keep_all = TRUE) 

# Count the number of samples of each genre for each year. 
# Plot the values on an axis from 1955 through 2005. 
samplesbyyear <- ggplot(df2,
       aes(sampledyear, fill = sampledgenre)) +
  geom_bar(color = "gray77", width = 1) +
  scale_fill_manual(values = c("#E67343", "#F72585", "#F8B219", "#004AF7", "#9B53E6")) +
# Remove any gaps around the plot edge 
  scale_x_continuous(limits = c(1955, 2005), expand = expansion(0,0)) +
  scale_y_continuous(limits = c(0, 8), expand = expansion(add = c(0, 1)),
                     breaks = seq(0, 8, 2)) +
  theme_minimal() +
  theme(legend.position = 'none',
        axis.line.x = element_line(color = "gray55"),
        axis.ticks = element_blank(),
        axis.ticks.x = element_line(color = "gray77"),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "gray88"))


print(samplesbyyear)

# Save the plot as an SVG
ggsave(file = "samplesbyyear.svg", plot = samplesbyyear, 
       width = 7, height = 2, dpi = 72) 
  