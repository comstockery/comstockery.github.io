
# Load libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(googlesheets4)
  library(svglite)
  library(ggtext)
  library(geomtextpath)
  })


# Flags for code chunks
datamaker = TRUE; # TRUE at the start of a session; otherwise FALSE
donutmaker = TRUE; # TRUE to generate main album donut; otherwise FALSE
barmaker = TRUE; # TRUE to generate secondary (count) graphs; otherwise FALSE
bitemaker = TRUE; # TRUE for the track-by-track donut bites; otherwise FALSE
linermaker = TRUE; # TRUE to generate the HTML for donut bite liner notes; otherwise FALSE


# Define color palette (NA values are defined separately)
colorvalues = c("electronic/dance" = "#E78B0E", 
                "hip-hop/rap/r&b" = "#DD3B87", 
                "other" = "#0E98D7",
                "rock/pop" = "#0E2D81",
                "soul/funk/disco" = "#6A3B93")

navalue = "#AAAAAA"


if(datamaker) {

# Read data into R from Google Sheets
donutsmain <- read_sheet('https://docs.google.com/spreadsheets/d/14y4hoZEaIZVR0N2K-vOASqkKbftjNUWV3ijvIi0TSXE/edit#gid=1315766256') %>%
  select(1:15) # Drop the last three rows
# If this gets a 'gargle' error then run this function to pull up the Google / Tidyverse login: gs4_auth(email = NA)

# Form three tidy data frames
# Donuts track list
tracklist <- donutsmain %>%
  distinct(donutstrack, .keep_all = TRUE) %>%
  select(donutstrack:bpm)

# Unique sample list
samplelist <- donutsmain[!duplicated(donutsmain[c('donutstrack','sampledtrack')]),] %>%
  select(donutstrack, sampledtrack:sampledyear) 

# Sample instances list
instancelist <- donutsmain %>%
  select(donutstrack, trackname, boxsetside, length, sampledtrack:duration) %>%
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


# Create a function to translate years to groove positions (timeline)
vlinecalc <- function(year) {
  vlinegroove <- aleadout + ((1 - ((year - earliestyear) / 
                 (latestyear-earliestyear))) * (aleadin-aleadout))
  return(vlinegroove)
}

# Join datasets to map samples to the Donut tracklist
df1 <- inner_join(tracklist, samplelist, by='donutstrack') %>%
  drop_na()

# Place sprinkles within their slice, adding randomness to avoid overplotting
df1$sprinkle <- df1$ymin + ((df1$ymax - df1$ymin)*(runif(nrow(df1), 0.10, 0.90)))


# Calculate the angle and hjust of the track names
df1$angle <-  90 - (360 * (df1$ymax + df1$ymin)/2) 

# Horizontal justification flips on the left side
df1$hjust <- ifelse(df1$angle < -90, 0, 1)
df1$vjust <- ifelse(df1$angle < -90, 0, 1)

# The angle flips on the left side of the album
df1$angle <- ifelse(df1$angle < -90, df1$angle+180, df1$angle)


# Make a table for year labels
yearlabels <- data.frame(
  label = c('<strong>1955</strong>', '<strong>1970</strong>', '<strong>1980</strong>', 
            '<strong>1990</strong>', '<strong>2005</strong>'),
  x = vlinecalc(c(1955, 1970, 1980, 1990, 2005)),
  y = 1,
  angle = 0)

# Count the genres in dataset
genres = count(df1, sampledgenre)

df1outro <- df1 %>% filter(donutstrack != 1)
tracklistoutro <- df1outro %>% distinct(trackname, .keep_all = TRUE) 

donutrecord <- ggplot(df1) + 

  # Start with a white circle for the background of the album label
  geom_rect(aes(xmin = 0, xmax = ainner,
                ymax = 1, ymin = 0),
            fill = 'white') +  
  
  # Create two sets of rectangles that will be converted to 4-edged slices
  # First, the rectangles for the  the total record (outer and inner edges) 
  geom_rect(aes(ymax = 1, ymin = 0,
                xmax = aouter, xmin = ainner),
            fill = 'gray11', color = 'gray11', 
            linewidth = 0.6) +

  # Overlay the rectangles for track slices 
  geom_rect(aes(ymax = ymax, ymin = ymin, 
                xmax = aleadin, xmin = aleadout), 
            fill = 'gray22', color = 'gray44', 
            linewidth = 0.4) +
  
  # Add gridlines for certain years (grooves)
  geom_vline(xintercept = vlinecalc(c(1970,1980,1990)), color = 'gray11',
             linewidth = 0.4, linetype = 'solid') +

  # Add dots representing samples in each track (sprinkles)
  geom_point(aes(x = groove, y = sprinkle,
                 fill = sampledgenre), 
             shape = 21, color = 'gray88', 
             alpha = 0.95, size = 5, stroke = 0.6) +
  
  # Add colors for the sprinkles and remove plot buffers
  scale_fill_manual(values = colorvalues, na.value = navalue) + 

  # Add labels for gridlines (grooves)
  geom_richtext(data = yearlabels,
                mapping = aes(x = x, y = y, 
                              label = label,
                              # hjust = 0.5 means centered horizontally,
                              # vjust = 0 means above the dashed line
                              hjust = 0.5, vjust = 0.5,
                              angle = angle),
                color = 'gray99', fill = NA, 
                label.color = NA,
                size = 4.5,
                label.padding = unit(rep(0, 4), "pt")) +
  
  # Add labels for track names (slices)
  geom_richtext(data = tracklistoutro, 
                mapping = aes(x = (0.99*aleadin), y = (ymin + ymax)/2,
                          label = trackname,
                          # hjust = 0.5 means centered horizontally,
                          # vjust = 0 means above the dashed line
                          hjust = hjust, vjust = 0.5, 
                          angle = angle),
                color = 'gray77', fill = NA, alpha = 0.7,
                label.color = NA,
                size = 3.9,
                label.padding = unit(rep(0, 4), "pt")) +
  
  geom_textvline(aes(xintercept = aouter,
                     label = "start of album >>"), 
                 hjust = 0.025, vjust = 1, linetype = 0,
                 size = 3.4, color = 'gray77') +
  
  geom_textvline(aes(xintercept = aouter,
                     label = "<< end of album"), 
                 hjust = 0.975, vjust = 1, linetype = 0,
                 size = 3.4, color = 'gray77') +
  
  # Remove any gaps around the plot edge 
  scale_x_continuous(limits = c(0, 6), expand = expansion(0,0)) +
  scale_y_continuous(limits = c(0, 1), expand = expansion(0,0),
                     labels = NULL) +
  
  # Convert rectangles to radial slices
  coord_polar(theta = 'y') +
  # Remove other plot elements and legend
  theme_void() +
  theme(legend.position = 'none', 
        panel.border = element_blank(),
        legend.key = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        panel.grid  = element_blank(),
        plot.margin = unit(rep(-0.82,4), "inches"))

print(donutrecord)

# Save the plot as an SVG
ggsave(file = "donutrecord.svg", plot = donutrecord,
       width = 7, height = 7)

} # End of the 'donutmaker' code chunk


if(barmaker) {

# Make a timeline of the sampled songs' release years (count, stacked by genre)
  
# Data frame of unique sampled tracks  
df2 <- drop_na(samplelist) %>%
  distinct(sampledtrack, .keep_all = TRUE) 

# Count the number of samples of each genre for each year. 
# Plot the values on an axis from 1955 through 2005. 
samplesbyyear <- ggplot(df2,
       aes(sampledyear, fill = sampledgenre)) +
  geom_bar(color = "gray88", linewidth = 0.5) +
  # Add vertical line for Donuts release
  geom_segment(x = 2006, xend = 2006, 
               y = 0, yend = 9,
               color = "gray66", linewidth = 0.1, linetype = 'solid') +
  # Add vertical label for when Donuts was released
  geom_richtext(data = data.frame(),
                mapping = aes(x = 2006, y = 8.8, 
                            label = '<em>Donuts</em><br>released<br>(2006)',
                            angle = 0,
                            hjust = 1.1, vjust = 1), 
              color = 'gray55', fill = NA,
              label.color = NA,
              size = 3.5,
              label.padding = unit(rep(0, 4), "pt")) +
  # Add color mapping
  scale_fill_manual(values = colorvalues, na.value = navalue) + 
  # Remove any gaps around the plot edge 
  scale_x_continuous(limits = c(1955, 2006), expand = expansion(0, 0)) +
  scale_y_continuous(limits = c(0, 9), expand = expansion(0, 0),
                     breaks = seq(0, 9, 1)) +
  # Set up theme
  theme_minimal() +
  theme(legend.position = 'none',
        axis.line.x = element_line(color = "gray44"),
        axis.text = element_text(size = 10),       
        axis.ticks = element_blank(),
        axis.ticks.x = element_line(color = "gray44"),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "gray88"))


print(samplesbyyear)

# Save the plot as an SVG
ggsave(file = "samplesbyyear.svg", plot = samplesbyyear, 
       width = 7, height = 2) 

# Data frame that makes an index for each instance of a sample by type
df3 <- instancelist %>%
  group_by(donutstrack, trackname, type) %>%
  distinct(sampledtrack, .keep_all = TRUE) %>%
  mutate(id = 1:n()) %>%
  arrange(donutstrack, type, id) 

supersources <- df3 %>%
  group_by(sampledtrack) %>%
  summarise(count = n_distinct(type)) %>%
  ungroup() %>%
  filter(count == 3)

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
        panel.grid.major.y = element_line(color = 'gray77', linewidth = 0.4),
        panel.spacing.x = unit(1.3, 'lines'),
        strip.text.x = element_text(size=12, face='bold'),
        strip.clip = 'off') 

print(samplesbytype)

ggsave(file = "samplesbytype.svg", plot = samplesbytype, 
       width = 5, height = 7) 
  
} # End of the 'barmaker' code chunk

# Make donut bites, or track-by-track visualizations of samples' genre, timing, and type
if(bitemaker) {

# Define dimensions of the track donut
touter = 3.5        # Half of a 7" record
tleadin = 3.31      # Start of the recording groove 6.625
tleadout = 1.75     # End of the recording groove 3.5
tinner = 0.75       # Inside hole 1.5  

square = 1.5          # Base size of the output SVG



# Establish a ranking for the samples based on their type
typegroove = tibble(type = c('lyric', 'surface', 'structural'),
                    typerank = c(1:3))

# Add an initial ranking of the sample types
df4 <- left_join(instancelist, typegroove, by = "type")

# Calculate the rectangle edges
# The y dimension is along the circumference (duration)
df4$ymin = df4$samplestart / df4$length
df4$ymax = df4$sampleend / df4$length

# Arrange the samples by how common they are in a track
df5 <- df4 %>%
  group_by(donutstrack, type, sampledtrack) %>%
  mutate(use = sum(duration)) %>%
  ungroup() %>%
  group_by(donutstrack) %>%
  arrange(donutstrack, -use)

# Let the track grooves take up 90% of the width
tgroovewidth = 0.90

df5$groove = df5$typerank

df5$gmin = (((1 / (2*3)) + ((df5$groove - 1) / 3)) - (tgroovewidth / (2*3)))
df5$gmax = (df5$gmin + (tgroovewidth / 3))

# The x dimension is outward from the center
df5$xmin = tleadout + (df5$gmin * (tleadin-tleadout))
df5$xmax = tleadout + (df5$gmax * (tleadin-tleadout))

# Write a loop that cycles through the whole album
for(i in 1:max(df5$donutstrack)) {

# Filter the dataframe for just one track (i)
donutbite <- filter(df5, donutstrack ==  i) %>%
# Then plot it!
ggplot() +
  # The rectangles represent when certain samples are playing
  geom_rect(aes(xmin = xmin, xmax = xmax,
                ymin = ymin, ymax = ymax, fill = sampledgenre),
            color = "gray88", linewidth = 0.3, alpha = 1) +
  # Add colors
  scale_fill_manual(values = colorvalues, na.value = navalue) + 
  # Clip the graph to just the necessary limits, remove any gaps
  scale_x_continuous(limits = c(0, 3.5), expand = expansion(0,0)) +
  scale_y_continuous(limits = c(0, 1), expand = expansion(0,0),
                     labels = NULL) +
  # Add a 'label' for each track
  # geom_richtext(aes(x = 0, y = 0, label = trackname, 
  #                   hjust = 0.5, vjust = 0.5),
  #               color = 'gray33', fill = NA,
  #               label.color = NA,
  #               size = 4) +
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
        plot.margin = unit(rep((-0.15*square),4), "inches"))

# View each plot as it's made
print(donutbite)

# Save each plot as an svg
ggsave(file = paste0(i, ".svg"), plot = donutbite, 
       width = square, height = square) 

# Select a few tracks to dig into for liner notes
linernotes = c(1:max(tracklist$donutstrack))

if (i %in% linernotes) {
  
donutbite <- donutbite + 
  theme(plot.margin = unit(rep((-0.36*square),4), "inches"))
  
  ggsave(file = paste0(i, "X.svg"), plot = donutbite, 
         width = 2.5*square, height = 2.5*square) 
  }

} # End of donutbite function



# Make the 32nd donut: a legend that explains the grooves
# Track 5, "The New" is good for this
df6 <- data.frame(filter(df5, donutstrack == 5)) %>%
  select(donutstrack, type, xmin, xmax) %>%
  mutate(ymin = 0, ymax = 1, label = paste(type, "samples"))

donutlegend <- ggplot(df6) +
  geom_rect(aes(xmin = xmin, xmax = xmax,
                ymin = ymin, ymax = ymax),
            fill = 'gray77', color = 'gray88', 
            linewidth = 0.3, alpha = 1) +
  geom_textvline(data = df6, aes(xintercept = (xmin+xmax)/2, 
                                 label = label), 
                 hjust = 0.5, vjust = 0.5, linetype = 0,
                 size = 2.4, color = 'gray33') +
  geom_textvline(aes(xintercept = tleadin, 
                     label = "start >>"), 
                 hjust = 0.03, vjust = 1, linetype = 0,
                 size = 2.6, color = 'gray33') +
  geom_textvline(aes(xintercept = tleadin, 
                     label = "<< end"), 
                 hjust = 0.97, vjust = 1, linetype = 0,
                 size = 2.6, color = 'gray33') +
  
  # Clip the graph to just the necessary limits, remove any gaps
  scale_x_continuous(limits = c(0, 3.5), expand = expansion(0,0)) +
  scale_y_continuous(limits = c(0, 1), expand = expansion(0,0),
                     labels = NULL) +
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
        plot.margin = unit(rep((-0.15*square),4), "inches"))

# Save the legend as an svg
ggsave(file = "0.svg", plot = donutlegend, 
       width = square, height = square) 

ggsave(file = "0X.svg", plot = donutlegend, 
       width = 2.5*square, height = 2.5*square) 

} # End of the 'bitemaker' code chunk


# Make the liner notes for donut bites
if(linermaker) {
  
  genretext <- data.frame(sampledgenre = unique(df3[c("sampledgenre")]),
                          sampledgenretext = c('soultext', 'hiphoptext', 'rocktext', 'unknowntext', 'electronictext','othertext'))

  df7 <- inner_join(df3, genretext, by="sampledgenre") |>
    group_by(donutstrack,trackname,type) |>
    summarize(tracksintype = str_flatten(paste0('<span class = "',sampledgenretext,' credit">',
                                          sampledartist,' - ',
                                          sampledtrack,' (',
                                          sampledyear,')</span>')), .groups = "drop") |>
    arrange(donutstrack, match(type, c("structural", "surface", "lyric"))) |>
    mutate(typecaps = toupper(type))

  
  df8 <- df7 |>
    group_by(donutstrack) |>
    summarize(gatefoldcredit = str_flatten(paste0(typecaps, tracksintype),"<br>"))|>
    mutate(gatefoldcredit = str_replace_all(gatefoldcredit, "NA", "?????"))

    
  
} # End of the 'linermaker' code chunk 
  