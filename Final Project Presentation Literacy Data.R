### Project Support for Valerie

# Read csv data file
getwd()
setwd("C:/Users/valer/Desktop")
getwd
library(dplyr)

literacy_df <- read.csv("./valerie_literacy_data.csv")
literacy_df <- arrange(literacy_df, -literacy_df$Lit_A)
View(literacy_df)

# Average of all Lit_A
average_Lit_A <- mean(literacy_df$Lit_A)
average_Lit_A


# Average of top 10% Lit_A
nrow(literacy_df)
literacy_df_top_10p <- literacy_df[1:(0.1*nrow(literacy_df)), ]
View(literacy_df_top_10p)

average_Lit_A_top_10p <- mean(literacy_df_top_10p$Lit_A)
average_Lit_A_top_10p

# Average of bottom 10% Lit_A
literacy_df_bottom_10p <- literacy_df[(0.9*nrow(literacy_df)):nrow(literacy_df), ]
View(literacy_df_bottom_10p)

average_Lit_A_bottom_10p <- mean(literacy_df_bottom_10p$Lit_A)
average_Lit_A_bottom_10p

getwd()
rm(list = ls())

library(tidyverse)

us_counties <-map_data("county")
head(us_counties)
nrow(us_counties)

literacy_df <- read.csv("./valerie_literacy_data.csv")
literacy_df <- arrange(literacy_df, -literacy_df$Lit_A)
literacy_df$County <- tolower(str_remove(literacy_df$County, " County"))
nrow(literacy_df)

### Data of all counties in the U.S.
county_literacy_df <- literacy_df %>% 
  select(County, Lit_A) %>% 
  unique() %>%  
  arrange(-Lit_A)
nrow(county_literacy_df) 
average_Lit_A <- mean(county_literacy_df$Lit_A)
average_Lit_A

### Data of top 10 % counties in the U.S.
top10p_df <- county_literacy_df[1:(nrow(county_literacy_df)*0.1), ]
nrow(top10p_df)
top10p_average_Lit_A <- mean(top10p_df$Lit_A)
top10p_average_Lit_A

### Data of bottom 10 % counties in the U.S.
bottom10p_df <- county_literacy_df[(nrow(county_literacy_df)*0.9):nrow(county_literacy_df), ]
nrow(bottom10p_df)
bottom10p_average_Lit_A <- mean(bottom10p_df$Lit_A)
bottom10p_average_Lit_A

### Plotting top 10 % counties' Lit_A level
literacy_df_top_10p <- us_counties %>%
  left_join(top10p_df, by = c("subregion" = "County"))
nrow(literacy_df_top_10p)

ggplot() + 
  geom_polygon(data = literacy_df_top_10p,
               mapping = aes(x = long, y = lat, group = group, fill = Lit_A),
               color = "black", size = 0.1) + 
  scale_fill_continuous(low = "blue", high = "green", 
                        na.value = "white") +
  theme(axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank()) +
  ggtitle("Literacy of Top 10 Percent Counties in the U.S.")


### Plotting bottom 10 % counties' Lit_A level
literacy_df_bottom_10p <- us_counties %>%
  left_join(bottom10p_df, by = c("subregion" = "County"))
nrow(literacy_df_bottom_10p)

ggplot() + 
  geom_polygon(data = literacy_df_bottom_10p,
               mapping = aes(x = long, y = lat, group = group, fill = Lit_A),
               color = "black", size = 0.1) + 
  scale_fill_continuous(low = "red", high = "orange", 
                        na.value = "white") +
  theme(axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank()) +
  ggtitle("Literacy of Bottom 10 Percent Counties in the U.S.")

