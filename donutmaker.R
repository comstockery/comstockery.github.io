
# Load libraries
library(tidyverse)
library(svglite)
library(ggtext)
library(googlesheets4)
library(emojifont)

# Flags for code chunks
datamaker = TRUE; # TRUE at the start of a session; otherwise FALSE
donutmaker = FALSE; # TRUE to generate main album donut; otherwise FALSE
barmaker = TRUE; # TRUE to generate secondary (count) graphs; otherwise FALSE
bitemaker = FALSE; # TRUE for the track-by-track donut bites; otherwise FALSE


# Define color palette (NA values are defined separately)
colorvalues = c("electronic/dance" = "#E67343", 
                "hip-hop/rap/r&b" = "#F72585", 
                "other" = "#F8B219",
                "rock/pop" = "#004AF7",
                "soul/funk/disco" = "#9B53E6")
navalue = "#AAAAAA"

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
  select(donutstrack, sampledtrack:sampledyear) 
# %>%
#   na.exclude %>%
#   filter(!is.na(sampledyear))

# Sample instances list
instancelist <- donutsmain %>%
  select(donutstrack, trackname, length, sampledtrack:duration) %>%
  select(-sampledelement)

} # End of the datamaker code chunk


# Make the donut for entire album, but only if the donutmaker is TRUE!
if(donutmaker) {


# Start making donuts!

# Define dimensions of the album donut
aouter = 6       # Half of a 12" record
aleadin = 5.75   # Start of the recording groove =11.5"
aleadout = 1.88 # End of the recording groove 3.75
ainner = 1.69    # End of spiral 3.375


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
samplelist$groove = aleadout + ((samplelist$radius) * (aleadin - aleadout))
# sprinklewidth = (1/10)

# Create a function to translate years to groove positions (timeline)
vlinecalc <- function(year) {
  vlinegroove <- aleadout + ((1 - ((year - earliestyear) / 
                              (latestyear-earliestyear))) * (aleadin-aleadout))
  return(vlinegroove)
}

# Join datasets to map samples to the Donut tracklist
df1 <- inner_join(tracklist, samplelist, by='donutstrack')

# Place sprinkles within their slice, adding randomness to avoid overplotting
df1$sprinkle <- df1$ymin + ((df1$ymax - df1$ymin)*(runif(nrow(df1), 0.10, 0.90)))


# Calculate the angle and hjust of the track names
df1$angle <-  90 - (360 * df1$ymin) 

# Horizontal justification flips on the left side
df1$hjust <- ifelse(df1$angle < -90, 0, 1)
df1$vjust <- ifelse(df1$angle < -90, 0, 1)

# The angle flips on the left side of the album
df1$angle <- ifelse(df1$angle < -90, df1$angle+180, df1$angle)


# Make a table for year labels
yearlabels <- data.frame(
  label = c('<strong>1955</strong>', '<strong>1970</strong>', 
            '<strong>1990</strong>', '<strong>2005</strong>'),
  x = vlinecalc(c(1955, 1970, 1990, 2005)),
  y = 1,
  angle = 0)

# Count the genres in dataset
genres = count(df1, sampledgenre)


donutrecord <- ggplot(df1) + 
  # Create two sets of rectangles that will be converted to 4-edged slices
  
  # First, the rectangles for the  the total record (outer and inner edges) 
  geom_rect(aes(ymax = 0, ymin = 1,
                xmax = aouter, xmin = ainner),
            fill = 'gray11', color = 'gray11', 
            linewidth = 0.6) +

# Overlay the rectangles for track slices 
  geom_rect(aes(ymax = ymax, ymin = ymin, 
                xmax = aleadin, xmin = aleadout), 
            fill = 'gray22', color = 'gray88', 
            linewidth = 0.4) +
  
  # Add gridlines for certain years (grooves)
  geom_vline(xintercept = vlinecalc(c(1970,1990)), color = 'gray88',
             linewidth = 0.2, linetype = 'dotted') +

  # Add dots representing samples in each track (sprinkles)
  geom_point(aes(x = groove, y = sprinkle,
                 fill = sampledgenre), 
             shape = 21, color = 'gray88', size = 5, stroke = 0.5) +
  
  # Add colors for the sprinkles and remove plot buffers
  scale_fill_manual(values = colorvalues, na.value = navalue) + 

  # Add labels for gridlines (grooves)
  geom_richtext(data = yearlabels,
                mapping = aes(x = x, y = y, 
                              label = label,
                              # hjust = 0.5 means centered horizontally,
                              # vjust = 0 means above the dashed line
                              hjust = 0.5, vjust = 0,
                              angle = angle),
                color = 'gray99', fill = NA, 
                label.color = NA,
                size = 3,
                label.padding = unit(rep(0, 4), "pt")) +
  
  # Add labels for track names (slices)
  geom_richtext(mapping = aes(x = (0.99*aleadin), y = ymin + 0.0015,
                          label = trackname,
                          # hjust = 0.5 means centered horizontally,
                          # vjust = 0 means above the dashed line
                          hjust = hjust, vjust = vjust, angle = angle),
                color = 'gray77', fill = NA,
                label.color = NA,
                size = 1.5,
                label.padding = unit(rep(0, 4), "pt")) +
  
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


if(barmaker) {

# Make a timeline of the sampled songs' release years (count, stacked by genre)
df2 <- samplelist %>%
  distinct(sampledtrack, .keep_all = TRUE) 

# Count the number of samples of each genre for each year. 
# Plot the values on an axis from 1955 through 2005. 
samplesbyyear <- ggplot(df2,
       aes(sampledyear, fill = sampledgenre)) +
  geom_bar(color = "gray88", linewidth = 0.5) +
  # Add vertical line for Donuts release
  geom_segment(x = 2006, xend = 2006, 
               y = 0, yend = 8,
               color = "gray44", linewidth = 0.5, linetype = 'dotted') +
  geom_richtext(data = data.frame(),
                mapping = aes(x = 2006, y = 7.8, 
                            label = 'Donuts released',
                            angle = -90,
                            hjust = 0, vjust = 0), 
              color = 'gray44', fill = NA,
              label.color = NA,
              size = 3,
              label.padding = unit(rep(0, 4), "pt")) +

  scale_fill_manual(values = colorvalues, na.value = navalue) + 
  # Remove any gaps around the plot edge 
  scale_x_continuous(limits = c(1955, 2006), expand = expansion(0, 1)) +
  scale_y_continuous(limits = c(0, 8), expand = expansion(add = c(0, 0.5)),
                     breaks = seq(0, 8, 2)) +
  theme_minimal() +
  theme(legend.position = 'none',
        axis.line.x = element_line(color = "gray55"),
        axis.ticks = element_blank(),
        axis.ticks.x = element_line(color = "gray55"),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "gray88"))


print(samplesbyyear)

# Save the plot as an SVG
ggsave(file = "samplesbyyear.svg", plot = samplesbyyear, 
       width = 7, height = 2, dpi = 72) 


df3 <- instancelist %>%
  group_by(donutstrack, trackname, type) %>%
  distinct(sampledtrack, .keep_all = TRUE) %>%
  # summarize(typecount = n_distinct(sampledtrack), .groups = 'drop') %>%
  mutate(id = 1:n()) %>%
  arrange(donutstrack, type, id) 

# df3 <- instancelist %>%
#   group_by(donutstrack, trackname, sampledgenre, type) %>%
#   summarize(typecount = n_distinct(sampledtrack), .groups = 'drop') %>%
#   arrange(donutstrack) 


# df3 <- instancelist %>%
#   group_by(donutstrack, type) %>%
#   mutate(count = n_distinct(sampledtrack)) %>%
#   ungroup() %>%
#   select(c(donutstrack, trackname, type, count))


samplesbytype <- ggplot(df3) +
  # Plot circles that are actually lines around the individual donuts
  geom_point(aes(x = id, y = reorder(trackname, -donutstrack)),
             color = 'gray88', shape = 21, fill = 'white', size = 2.5, stroke = 4) +
  # Overlay donuts colored by their genre
  geom_point(aes(x = id, y = reorder(trackname, -donutstrack),
                 color = sampledgenre),
             shape = 21, fill = NA, size = 2.5, stroke = 3) +
  # Map the colors to the genres
  scale_color_manual(values = colorvalues, na.value = navalue) + 
  # Add a slight margin in each donut box
  scale_x_continuous(expand = expansion(add = c(0.6, 0.6))) +
  # Facet the plot by sample type and allow the axes to be sized by their max donut count
  facet_grid(~factor(type, levels = c('structural', 'surface', 'lyric')),
             scales = 'free', space = 'free') +
  # Set up the theme for the plot
  theme_minimal() + # order: axis, legend, panel, plot, strip
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = 'none',
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'gray88'),
        panel.border = element_rect(color = 'gray55', fill = NA, size = 0.8),
        panel.spacing.x = unit(1.5, 'lines'),
        strip.text.x = element_text(size=12, face='bold'),
        strip.clip = 'off') 

print(samplesbytype)

ggsave(file = "samplesbytype.svg", plot = samplesbytype, 
       width = 5, height = 7, dpi = 72) 
  
} # End of the 'barmaker' code chunk


if(bitemaker) {

# Define dimensions of the track donut
touter = 3.5        # Half of a 7" record
tleadin = 3.31      # Start of the recording groove 6.625
tleadout = 1.75     # End of the recording groove 3.5
tinner = 0.75       # Inside hole 1.5  

# Calculate the rectangle edges
# This requires math!

typegroove = tibble(type = c('structural', 'surface', 'lyric'),
                    typerank = c(1:3))

# Add an initial ranking of the sample types
df4 <- left_join(instancelist, typegroove, by = "type")

df4$ymin = df4$samplestart / df4$length
df4$ymax = df4$sampleend / df4$length

# Use a bunch of dplyr functions to determine unique sample/type combo
# positions within each donut
df5 <- df4 %>%
  group_by(trackname, type) %>%
  distinct(sampledtrack, .keep_all = TRUE) %>%
  arrange(donutstrack, typerank) %>%
  mutate(id = 1:n()) %>% # counting the sample/type combos with an index
  mutate(tx = (typerank * 10) + id) %>% # aggregate index across types
  ungroup() %>%
  arrange(donutstrack, tx) %>%
  group_by(donutstrack) %>%
  mutate(groove = 1:n()) %>% # indexing across tracks
  ungroup() %>%
  select(donutstrack, sampledtrack, type, groove) %>%
  group_by(donutstrack) %>%
  mutate(maxgroove = max(groove))
  
df6 <- left_join(df4, df5, by = c('donutstrack', 'sampledtrack', 'type'))

tgroovewidth = 0.90
# df6$xmin = (((1 / (2*df6$maxgroove)) + ((df6$groove - 1) / df6$maxgroove)) - (tgroovewidth / (2*df6$maxgroove)))
# df6$xmax = (df6$xmin + (tgroovewidth / df6$maxgroove))

df6$gmin = (((1 / (2*df6$maxgroove)) + ((df6$groove - 1) / df6$maxgroove)) - (tgroovewidth / (2*df6$maxgroove)))
df6$gmax = (df6$gmin + (tgroovewidth / df6$maxgroove))

df6$xmin = tleadout + (df6$gmin * (tleadin-tleadout))
df6$xmax = tleadout + (df6$gmax * (tleadin-tleadout))
    
# Write a loop that cycles through the whole album
for(i in 1:max(df6$donutstrack/2)) {

# Filter the dataframe for just one track (i)
donutbite <- filter(df6, donutstrack ==  i) %>%
# Then plot it!
ggplot() +
  # The main geoms are rectangles that represent when certain samples are playing
  geom_rect(aes(xmin = xmin, xmax = xmax,
                ymin = ymin, ymax = ymax, fill = sampledgenre), 
            color = "gray88", linewidth = 0.5) + 
  # Add colors
  scale_fill_manual(values = colorvalues, na.value = navalue) + 
  # Clip the graph to just the necessary limits, remove any gaps
  scale_x_continuous(limits = c(0, 3.5), expand = expansion(0,0)) +
  scale_y_continuous(limits = c(0, 1), expand = expansion(0,0),
                     labels = NULL) +
  # Add a 'label' for each track
  geom_richtext(aes(x = 0, y = 0, label = trackname, 
                    hjust = 0.5, vjust = 0.5),
                color = 'gray33', fill = NA,
                label.color = NA,
                size = 4) +
  # Make it a donut!
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

# Viewing each plot as it's made
print(donutbite)
plotname = i

# Save each plot as an svg
ggsave(file = paste0(plotname, ".svg"), plot = donutbite, 
       width = 3, height = 3, dpi = 72) 

} # End of donutbite function

} # End of the 'bitemaker' code chunk

