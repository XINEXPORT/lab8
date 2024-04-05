#Christine Hoang
#lab8
library("tidyverse")

library("dplyr")

library("ggplot2")

#1.	Read the data from the CSV file into a tibble and display it.
ramen_ratings <- read_csv("ramen-ratings.csv")
ramen_ratings

glimpse(ramen_ratings)

##convert Stars column to numbers
ramen_ratings$Stars <- as.numeric(ramen_ratings$Stars)

#2.	Create a tibble named ramen_stats that groups the data by style and calculates the mean stars and standard deviation for each style.
ramen_stats <- ramen_ratings %>%
  group_by(Style) %>%
  summarize(mean_stars = mean(Stars, na.rm = TRUE),
            sd_stars = sd(Stars, na.rm = TRUE))
ramen_stats

#3.	Create a bar plot that displays the mean stars for each style.
ggplot(ramen_stats, 
       aes(x = Style, y = sd_stars, fill=Style)) +
  geom_col()

#4. Add error bars to the bar plot created in the previous step and use the standard deviation of the stars to determine the length of the error bar.
ggplot(ramen_stats) + 
  geom_col(aes(x=Style, y = sd_stars, fill=Style))+
  geom_errorbar(aes(x=Style,
                    ymin = mean_stars-sd_stars,
                    ymax = mean_stars+sd_stars))

#5.	Create a plot that displays a map of the world.
ggplot(data = map_data("world"), aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", color = "black") +
  coord_quickmap()

#6.	Create a tibble named mean_ratings that contains the mean number of stars for each country.
mean_ratings<-ramen_ratings %>%
  group_by(Country) %>%
  summarize(MeanRating=mean(Stars))
mean_ratings

#7.	Create a scatter plot using the mean_ratings tibble. Using Country as the x-axis, MeanRating as the y-axis and color = country. 
ggplot(data=mean_ratings,aes(x=Country,y=MeanRating))+
  geom_point(aes(color=Country))

#8.	Improve the appearance of the scatter plot by adding a title. The title should be your name,  and removing the labels, ticks, and text for the x and y axes.
ggplot(data=mean_ratings,aes(x=Country,y=MeanRating))+
  geom_point(aes(color=Country))+
  labs(title="Christine Hoang")+
  theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        )






