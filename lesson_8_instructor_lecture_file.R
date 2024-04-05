# load the packages for this chapter
library("tidyverse")    # the core tidyverse packages (tibble, ggplot2, etc.)
library("datasets")     # the sample datasets (iris, ChickWeight, etc.)
library("ggforce")      # needed for geom_circle(), geom_arc(), and facet_matrix()

# GET THE DATA

# get irises data, which is available to you when you load the library datasets
irises <- as_tibble(iris)
irises

# get chicks data
chicks = as_tibble(ChickWeight)
chicks <- chicks %>% 
  rename(Weight = weight) %>%           # fix capitalization
  mutate(Chick = as.numeric(Chick),     # fix order of Chick column
         Chick = factor(Chick, ordered = TRUE))
chicks

# get mortality data, download from ecampus and place in your working directory
mortality_long <- readRDS("mortality_long.rds")
mortality_long

# get data about California cities, download from ecampus and place in your working directory
ca_cities <- readRDS("california_cities.rds")
ca_cities

# MORE SKILLS FOR SCATTER PLOTS we worked with Scatter Plots in Lesson 4. They are ways to visualize relationships between variables. In this case we will look at the sepal length and sepal width of an iris flower, which is the lowest "petals" of a blooming Iris plant. 

# The shape parameter. We have three species of Irises that we are plotting. Setosa, versicolor, virginica. By using the shape parameter in a ggplot command we can assign shapes, as well as colors, in the scatter plot to each species. If you zoom this plot in the files pane, you will see that for example there isn't much variation in the setosa species in sepal length, but there is setosa variation in width.  
ggplot(irises, aes(x = Sepal.Length, y = Sepal.Width, color = Species,
                   shape = Species)) +
  geom_point(size = 3)

# The alpha parameter - this sets the opacity or transparency of each point in the scatter plot based on the number of occurrences of length or width within the data. The more rows of equal sepal length that are identical, the darker the dot, or data point that is displayed. 

ggplot(irises, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point(size = 5, alpha = .5)

# Set shape to a literal value. (Not shown in chapter)
# 15 is a square. You can experiment with other numbers or even use characters, like "k" 
ggplot(irises, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point(size = 3, shape = 15)


# MORE SKILLS FOR BAR PLOTS

# Calculate some summary statistics for the irises data by creating a tibble that groups the iris data by species and summarizes by getting the mean (or average) petal length. And the standard deviation of the petal length. Standard deviation is a measure of how dispersed the data is from the average. Low, or small standard deviation means that the data is clustered around the mean(or average). A high standard deviation means that the data is more spread out. For example a group of student test scores that (20, 22, 70, 80, 90, 100) has an average score of 64, but with a high standard deviation. Test scores of (63, 64, 64, 64, 65, 66) has the same average or mean but with a low standard deviation. 
iris_stats <- irises %>% group_by(Species) %>%
  summarize(Mean.Petal.Length = mean(Petal.Length), 
            SD.Petal.Length = sd(Petal.Length))
# when you display the iris_stats data you see that virginica specises has the longest mean petal length, but also has the highest standard deviation of lengths.
iris_stats

# The geom_col() function will plot just one of these columns, the average length of petals. 
ggplot(iris_stats, 
       aes(x = Species, y = Mean.Petal.Length, fill = Species)) +
  geom_col()

# Error bars, these can be used in a visualization to indicate some measure of margin of error. In this case, we will use the standard deviation column in the iris_stats tibble as an overlay of the bar chart in order to show that while the average length of virginica is very high, the range of lengths is quite scattered.
# by using the geom_errorbar function you can display the averages as a bar chart and also the standard deviation from the averages. Zoom this plot and you will see that setosa petal lengths tend to cluster around the average, but virginica lengths vary more. 
ggplot(iris_stats) +
  geom_col(aes(x = Species, y = Mean.Petal.Length, fill = Species)) +
  geom_errorbar(aes(x = Species, 
                    ymin = Mean.Petal.Length - SD.Petal.Length, 
                    ymax = Mean.Petal.Length + SD.Petal.Length), 
                width = .25)

# Line types - you can change the line types in a line plot to better clarify your visualization. The mortality_long data shows deaths per 100,000 children in 4 age groups by year, and also by decade. By setting a linetype parameter in the ggplot function to age group you will get different line styles per age group. 
ggplot(mortality_long, aes(x = Year, y = DeathsPer100K, linetype = AgeGroup)) +
  geom_line(size = 1)

# Another line types example, recall that the chicks data set displays weight gain among chicks over time based on 4 different diets. This example displays different  line types  by diet. It also turns off the color legend in the visualization.  
ggplot(chicks, aes(x = Time, y = Weight, color = Chick, linetype = Diet)) +
  geom_line(size = 1) +
  guides(color = "none")  # turns off color legend

# PLOT SMOOTH LINES
# You can plot smooth lines to display trends in a more human friendly visual. 

# with a confident interval (not shown in chapter) this first example shows the deviation range, which is called a confidence interval,  in the plot as a gray area around each line. 
ggplot(mortality_long, aes(x = Year, y = DeathsPer100K, color = AgeGroup)) +
  geom_smooth()

# without a confidence interval, you can opt not to display the confidence interval in the plot by setting the se parameter of the geom_smooth function to false. Both plots show a dramatic decline in deaths per for children ages 1-4 over time, a less dramatic drop for the other age groups.  
ggplot(mortality_long, aes(x = Year, y = DeathsPer100K, color = AgeGroup)) +
  geom_smooth(se = FALSE)

# plot annotations. Adding labels to a plot can help explain the visualization. For example during your analysis you may have found that a spike in deaths in 1918, was attributed to a Spanish flu epidemic. Another label might be added for when penicillin was invented, allowing treatment of infections that didn't exist before. The geom_segment function defines sets the beginning and ending of the of the placement of label on the x axis, which is the year, and the y axis, which is the deaths per 100,000. The geom_label function creates a box in the plot and defines where it should be placed and what it should say.   
ggplot() +
  geom_line(data = mortality_long,
            aes(x = Year, y = DeathsPer100K, color = AgeGroup)) +
  geom_segment(aes(x = 1918, y = 1550, xend = 1918, yend = 1700)) +
  geom_label(aes(x = 1918, y = 1700, label = "1918 Spanish Flu")) +
  geom_segment(aes(x = 1928, y = 650, xend = 1928, yend = 750)) +
  geom_label(aes(x = 1940, y = 800, label = "Penicillin Invented"))


# HOW TO PLOT SHAPES

# plot some shapes, unrelated to datasets, just to show how it works. Use the geom_segment to draw a line starting at plot point 5 and ending at 10. The geom_rect function draws a blue rectangle with a width on the x-axis of 5 starting at 15 and ending at 20, it shows no fill and the outline of the shape as blue. The geom_tile function sets an opaque figure begining at position 27.5 on the x axis with a width of 5 and height of 10. With the midpoint of the tile at 7.5. The geom_circle function sets the center of the circle on the x axis at 40. The center of the circle on the y axis at 7.5. This is true for both the geom_arc and geom_ellipse functions. The geom_arc function also has an r parameter that sets the radius or the bend width of the arc. Note that the start is set to 0 and the end is set to pi. The a and b parameters of the geom_ellipse function set the heighth and width of the oval and the angle sets the "tilt" of the ellipse or oval.  
ggplot() +
  geom_segment(aes(x = 5, y = 5, xend = 10, yend = 10)) +
  geom_rect(aes(xmin = 15, xmax = 20, ymin = 5, ymax = 10), 
            color = "blue", fill = NA) +
  geom_tile(aes(x = 27.5, y = 7.5, width = 5, height = 10)) +
  geom_circle(aes(x0 = 40, y0 = 7.5, r = 5)) +
  geom_arc(aes(x0 = 50, y0 = 7.5, r = 10, start = 0, end = pi)) +
  geom_ellipse(aes(x0 = 60, y0 = 7.5, a = 3, b = 6, angle = 0)) +
  coord_fixed()

# draw circles around some key points (not shown in chapter), in this example you are drawing circles around any versicolor species iris with a petal width greater than 1.5. This might emphasize outliers in your data. 
ggplot() + 
  geom_point(data = irises, 
             aes(x = Petal.Length, y = Petal.Width, color = Species),
             size = 3) +
  geom_circle(data = filter(irises, 
                            Species == "versicolor" & Petal.Width > 1.5),
              aes(x0 = Petal.Length, y0 = Petal.Width, r = .1)) +
  coord_fixed()


# HOW TO CREATE CUSTOM PLOTS
# A baseball field includes and infield that is always a square 90 feet on each side. 
# The pitching mound is always 18 feet in diameter. 
# there must be at least 325 feet between home plate and the back fence at the foul line. 
# there must be at least 400 feet between home plate and the back fence at center field. 
ggplot() + 
  # infield + foul lines - create four diagonal lines one from home to outfield foul line in right and center field one each for the lines between first and second base and second and third base
  geom_segment(aes(x = 0, y = 0, xend = -230, yend = 230)) +
  geom_segment(aes(x = 0, y = 0, xend = 230, yend = 230)) + 
  geom_segment(aes(x = -63.6, y = 63.6, xend = 0, yend = 127.2)) +
  geom_segment(aes(x = 63.6, y = 63.6, xend = 0, yend = 127.2)) +
  # create the pitchers mound
  geom_circle(aes(x0 = 0, y0 = 59.5, r = 9)) +
# create the outfield arc, 73/180pi == 74 degrees 
  geom_arc(aes(x0 = 0, y0 = 160, r = 240, 
               start = -73/180*pi, end = 73/180*pi)) +
  # coordinate fixing
  coord_fixed()

# HOW TO ADD PLOT COMPONENTS WITH A FUNCTION
# baseball example as well but this time stored as a reusable function
plot_baseball_field <- function() {
  bball_field <- c(coord_fixed(),
    geom_segment(aes(x = 0, y = 0, xend = -230, yend = 230)),
    geom_segment(aes(x = 0, y = 0, xend = 230, yend = 230)),
    geom_segment(aes(x = -63.6, y = 63.6, xend = 0, yend = 127.2)),
    geom_segment(aes(x = 63.6, y = 63.6, xend = 0, yend = 127.2)),
    # pitchers mound
    geom_circle(aes(x0 = 0, y0 = 59.5, r = 9)),
    # outfield arc, 73/180pi == 74 degrees 
    geom_arc(aes(x0 = 0, y0 = 160, r = 240, start = -73/180*pi, 
                end = 73/180*pi)) )
  return(bball_field)
}
# call the function plot_baseball_field with all the geom functions embedded in it
ggplot() + plot_baseball_field()

# how to add data to the custom plot by creating a dataframe that stores the location of four baseball hits from a game.
baseball_hits <- data.frame(hitx = c(-100,-25, 100, 150),
                            hity = c(200, 50, 350, 275))
#display the data frame
baseball_hits

# use a geom_point fuction to pass the data frame to a ggplot() function sepcifying the x and y axises as the columns of the data frame and then call the function that draws the basic baseball field
ggplot() + 
  geom_point(data = baseball_hits,
             aes(x = hitx, y = hity)) +
  plot_baseball_field() 


# MAPS

# view some data frames that contain map data
map_data("world")                # all countries in world
map_data("usa")                  # only USA
map_data("france")               # only France
map_data("state")                # all states in USA
map_data("state", "california")  # only California
map_data("county")               # all counties in USA
map_data("county", "california") # only California counties

# plot some maps secify that the x axis is longitude or long and y axis is latitude the group column is a column in the data frame that groups and assigns numbers to each country - 1 for Aruba, 2 for Afghanistan and so on. The group = group assigns each border data point to a country. The geom_polygon parameters specify that the fill for each country (or group) should be white and the outline (borders) should be black. The coord_quickmap() function works like the coord_fixed() function but is special to maps and it fixes the aspect ratio of the visual. 
ggplot(data = map_data("world"), 
       aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "white", color = "black") + 
  coord_quickmap()

# another way to code the map (not shown in chapter) passing all parameter including the setting of the x and y axis aes() function inside the geom_polygon() function
ggplot() + 
  geom_polygon(data = map_data("world"), 
               aes(x = long, y = lat, group = group),
               fill = "white", color = "black") + 
  coord_quickmap()

# get the CA map data
ca_map <- map_data("state", "california")
ca_map

# plot the CA map (not shown in chapter)
ggplot(ca_map, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "white", color = "black") + 
  coord_quickmap()

# view data to plot on map
ca_cities

# combine plot of map, ggplot() and geom_polygon() functions with scatter plot geom_point() function to incude cities by population with the population determing the opacity of the data points in the scatter plot 
ggplot(data = ca_map, aes(x = long, y = lat)) + 
  geom_polygon(fill = "white", color = "black") + 
  geom_point(data = ca_cities, 
             aes(x = lng, y = lat, alpha = population)) +
  coord_quickmap()


# TUNING PLOTS
# limits, labels, scale, colors, transformations, annotations

# Zoom in on part of a plot using the facet_zoom() function. The xlim parameter sets the starting and ending points for the x axis, (the longitude) and the ylim parameter does the same thing for y axis, (the latitude), the zoom.size parameter specifies the size of the zoom. 
ggplot(ca_map, aes(long, lat)) + 
  geom_polygon(fill = "white", color = "black") + 
  geom_point(data = ca_cities, 
             aes(x = lng, y = lat, alpha = population)) +
  facet_zoom(xlim = c(-123,-121.5), ylim = c(37,38.5), zoom.size = 1) 

# Set the X and Y axis limits, in this case we just want to focus on the first 10 days of each chicks life to gage the weight gain of newborn chicks, additionally we are only looking at one diet type, remember there are four in the data set.
ggplot(filter(chicks, Diet == 1), 
       aes(x = Time, y = Weight, color = Chick)) +
  geom_line() +
  xlim(0,10) + 
  ylim(0, 150)

# Limit the axes without losing data
ggplot(ca_map, aes(long, lat)) + 
  geom_polygon(fill = "white", color = "black") + 
  geom_point(data = ca_cities, 
             aes(x = lng, y = lat, alpha = population)) +
  coord_cartesian(xlim = c(-123,-121.5), ylim = c(37,38.5))

# Titles and labels forcing descriptive labels onto a plot in this case we want a title that shows that we are only plotting diet type 1. We want the x axis to say "days elapsed" and y axis to say "Weight (Grams)". We are coloring each chick studied, in this case 20 different chicks using diet 1 on each of the lines. 
ggplot(filter(chicks, Diet == 1), 
       aes(x = Time, y = Weight, color = Chick)) +
  geom_line() + 
  labs(title = "Diet 1", x = "Days Elapsed", y = "Weight (Grams)") +
  theme(plot.title = element_text(hjust = 0.5))


# LEGENDS

# hide the legend, in the chicks data the legend isn't too useful, the chicks are arbitrarily assigned a number, like an auto increment primary key. This will surpress the legend within the display. 
ggplot(chicks, aes(x = Time, y = Weight, color = Chick)) +
  geom_line() + 
  theme(legend.position = "none")

# position the legend - move the legend to the bottom of the plot 
ggplot(irises, aes(x = Petal.Length, y = Petal.Width, color = Species)) + 
  geom_point(size = 3) +
  theme(legend.position = "bottom")

# set the title for a legend
ggplot(irises, aes(x = Petal.Length, y = Petal.Width, color = Species,
                   size = Petal.Width * Petal.Length)) + 
  geom_point() +
  guides(size = guide_legend(title = "Petal Size"))

# hide an unwanted legend
ggplot(irises, aes(x = Petal.Length, y = Petal.Width, color = Species,
                   size = Petal.Width * Petal.Length)) + 
  geom_point() +
  guides(size = "none")

# Axes text and ticks hiding the ticks and text on each axis can lead to a cleaner visual 
# example with text and ticks showing
ggplot(data = ca_map, aes(x = long, y = lat)) + 
  geom_polygon(fill = "white", color = "black") + 
  geom_point(data = ca_cities, 
             aes(x = lng, y = lat, alpha = population)) +
  coord_quickmap() +
  labs(x = "", y = "") 
 
# same plot with text and ticks hidden, in this case the latitude and longitude aren't critical to the visualization's intent
ggplot(data = ca_map, aes(x = long, y = lat)) + 
  geom_polygon(fill = "white", color = "black") + 
  geom_point(data = ca_cities, 
             aes(x = lng, y = lat, alpha = population)) +
  coord_quickmap() +
  labs(x = "", y = "") +
 theme(axis.ticks = element_blank(),
      axis.text = element_blank())


# COLORS

# The library for color palettes this will load many colors that you can use in a polot
library("RColorBrewer")

# display all available palettes
display.brewer.all()

# use a palette in a plot to change colors automatically
ggplot(irises, aes(x = Petal.Length, y = Petal.Width, color = Species)) + 
  geom_point(size = 3) +
  scale_color_brewer(palette = "Set2")

# set values manually
ggplot(irises, aes(x = Petal.Length, y = Petal.Width, color = Species)) + 
  geom_point(size = 3) +
  scale_color_manual(values = c("red","green","blue"))


# PLOT THEMES - these are predefined color themes that can be used in ggplot to automatically change some aspects of a plot automatically. The following theme sets the plot background to white, the grid lines to gray, and outer lines to black

# themes can be added line components
ggplot(irises, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point(size = 3) +
  theme_bw()


# FACET MATRIX creating a grid of plots, this isn't something you will display to users but it might help you at the start of a project to spot trends that may require investigation. 

# create a pair-wise grid of scatter plots using the facet_matrix function to specify the four columns in the plot. The .panel_x and .panel_y functions tell ggplot to use the variables that corresspond to each that location in the grid. 
ggplot(irises, aes(x = .panel_x, y = .panel_y, color = Species)) +
  geom_point() +
  facet_matrix(vars(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width))

# another way to pass variables to the grid
ggplot(irises, aes(x = .panel_x, y = .panel_y, color = Species)) +
  geom_point() +
  facet_matrix(vars(Sepal.Length:Petal.Width))

# create a pair-wise grid of 3 different plot types
ggplot(irises, aes(x = .panel_x, y = .panel_y)) +
  geom_point(aes(color = Species)) +
  geom_autodensity(aes(fill = Species)) +
  geom_boxplot(aes(fill = Species)) +
  facet_matrix(vars(Sepal.Length:Petal.Width), 
               layer.lower = 1, 
               layer.diag = 2, 
               layer.upper = 3)
