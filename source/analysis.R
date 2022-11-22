library("tidyverse")
library("dplyr")
library("stringr")
library("ggplot2")
library("plotly")
library("rjson")
library("tidyr")


# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# 
#loading data 
get_data <- function(num_records=-1) {
  fname <- "~/info201/data/incarceration_trends.csv"
  df <- read.csv(fname, nrows=num_records, stringsAsFactors = FALSE)
  return(df)
}
incarceration_df <- get_data()
View(incarceration_df)
#
##What is the average  total jail  population rate across all counties of the most recent year?
filter_max_year <- incarceration_df %>%
  filter(year== max(year, na.rm=TRUE))
  View(filter_max_year)

avg_total_rate <- filter_max_year %>%
  summarize(average = mean(total_jail_pop_rate, na.rm=TRUE))%>%
  pull(average)
        
##Which county has the highest highest jail population as of the most recent year?
filter_max_year <- incarceration_df %>%
  filter(year== max(year, na.rm=TRUE))

max_jail_prop <- filter_max_year %>%
  group_by(county_name)%>%
 mutate(prop = (total_jail_pop)/(total_pop))%>%
  filter(!is.na(prop))%>%
  mutate(prop = max(prop))%>%
  select(prop, county_name)%>%
  arrange(-prop)%>%
  subset(prop == max(prop))%>%
  pull(county_name)

##
  ##What is the proportion of number of black people in jail?

black_jail_prop <- filter_max_year%>%
  filter(!is.na(black_jail_pop))%>%
  filter(!is.na(total_jail_pop))%>%
  summarise(pop_rate =sum(black_jail_pop)/sum(total_jail_pop))%>%
  select(pop_rate)%>%
pull(pop_rate)
## What proportion do black people make up in the USA
black_total_prop <- filter_max_year%>%
  filter(!is.na(black_pop_15to64))%>%
  filter(!is.na(total_pop_15to64))%>%
  summarise(total_pop_rate =sum(black_pop_15to64)/sum(total_pop_15to64))%>%
  select(total_pop_rate)%>%
  pull(total_pop_rate)
  
##What is the proportion of number of black people in jail?

white_jail_prop <- filter_max_year%>%
  filter(!is.na(white_jail_pop))%>%
  filter(!is.na(total_jail_pop))%>%
  summarise(pop_rate =sum(white_jail_pop)/sum(total_jail_pop))%>%
  select(pop_rate)%>%
  pull(pop_rate)
## What proportion do black people make up in the USA
white_total_prop <- filter_max_year%>%
  filter(!is.na(white_pop_15to64))%>%
  filter(!is.na(total_pop_15to64))%>%
  summarise(total_pop_rate =sum(white_pop_15to64)/sum(total_pop_15to64))%>%
  select(total_pop_rate)%>%
  pull(total_pop_rate)
## 

 
#Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>

get_year_jail_pop <- function(){
data <-  incarceration_df %>%
  filter(!is.na(total_jail_pop))%>%
  group_by(year)%>%
  select(year, total_jail_pop)%>%
  summarise(sum_pop = sum(total_jail_pop))

return(data)

}
get_year_pop <- get_year_jail_pop()
View(get_year_pop)




# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function(){
 pop <- ggplot(get_year_jail_pop())+
   geom_col(
     mapping = aes(x = year, y = sum_pop))+
   labs(
     x= "Year",
     y= "Total Jail Population",
     title = "Increase of Jail Population in US (1970-2018)",
   )
 
  return(pop)   
} 

plot <- print(plot_jail_pop_for_us())

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
#getting the vector of states




get_jail_pop_by_states <- function(states){
  
  prison_df <- incarceration_df %>%
    filter(state %in% states)%>%
    group_by(state, year)%>%
    summarize(sum_pop = sum(total_prison_pop,
              na.rm=TRUE), 
              .groups = "drop")
    
 return(prison_df)
}
View(get_jail_pop_by_states(c("GA","WA","AL", "CA", "TX")))

  
  plot_jail_pop_by_states <- function(states){
    plot_state <- ggplot(data= get_jail_pop_by_states(states),
                  aes(
                    x = year, 
                    y = sum_pop,
                    group = state
                    )
                )+
      geom_line(
        aes(linetype= state, color = state)
        )+
      labs(x= "Year",
           y= "Total Jail Population",
        title = "Growth of Prison Population By State")
      
  return(plot_state)  
  }
  plot_jail_pop_by_states(c("GA","WA","AL", "CA", "TX"))
# See Canvas
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#

  
  black_pop_prop <- function(){
    
    black_prop_df <- incarceration_df%>%
    filter(year == 2016)%>%
    filter(!is.na(black_jail_pop_rate))%>%
    filter(!is.na(black_pop_15to64))%>%
    mutate( black_prop = (black_pop_15to64/total_pop))%>%
    mutate(black_jail_prop = (black_prison_pop/total_prison_pop))%>%
    select(black_jail_prop, black_prop)
    
    return(black_prop_df)
  }
    
   black_pop<- function(){
     plot_1 <- ggplot(data = black_pop_prop())+
      geom_point(mapping= aes(x= black_prop ,y= black_jail_prop))+
      labs(x= "Proportion of Black population in given County",
           y= "Number of Black people in jail",
           title = "Number of Black people in Jail vs Proportion of Black population in County")
   return(plot_1) 
  }
  
  black_pop()
  
white_pop_prop <- function(){
white_prop_df <- incarceration_df%>%
filter(year == 2016)%>%
filter(!is.na(white_prison_pop))%>%
filter(!is.na(total_prison_pop))%>%
filter(!is.na(white_pop_15to64))%>%
mutate( white_prop = (white_pop_15to64/total_pop))%>%
mutate(white_jail_prop = (white_prison_pop/total_prison_pop))%>%
select(white_jail_prop, white_prop)
    return(white_prop_df)
}
white_pop_prop()
    
plot_2 <- function(){
plot2 <- ggplot(data = white_pop_prop())+
geom_point(mapping= aes(x= white_prop ,y= white_jail_prop))+
labs(x= "Proportion of white population in given county",
           y= "Proportion of White people in jail",
           title = "Proportion of white people in Jail vs Proportion of white population in County")
    return(plot2)
    }
    plot_2()
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
  state_name <- read.csv("https://raw.githubusercontent.com/jasonong/List-of-US-States/master/states.csv")
    
 names(state_name)[names(state_name)== "Abbreviation"]<- "state"
    
#    data1 <- incarceration_df %>%
#      filter(year == 2016)%>%
#      group_by(state)%>%
#   summarize(sum_tot_pop = mean((black_pop_15to64/total_pop_15to64), na.rm=TRUE))%>%
#      select(state, sum_tot_pop)
#    View(data1)
#     nrow(data1)
#    get_thing <- incarceration_df%>%
#     filter(year == 2016)%>%
#      filter(!is.na(total_jail_pop))%>%
#      filter(!is.na(black_jail_pop))%>%
#       group_by(state)%>%
#       summarise(sum_jail_pop = mean((black_jail_pop/total_jail_pop), na.rm=TRUE))%>%
#       select(state, sum_jail_pop)
#   View(get_thing)
#   nrow(get_thing)
#    get_data <- left_join(get_thing, data1, by= "state")
#     get_data <- get_data %>%
#       mutate(diff_tot_pop = (sum_jail_pop)-(sum_tot_pop))
#   
# new_data <- left_join(state_name, get_data, by="state")%>%
# mutate(state = tolower(new_data$state))%>%
#   select(state, diff_tot_pop)
# View(new_data)
 data_map1 <- function(){
   state_name <- read.csv("https://raw.githubusercontent.com/jasonong/List-of-US-States/master/states.csv")
 
 names(state_name)[names(state_name)== "Abbreviation"]<- "state"
 
get_thing <- incarceration_df%>%
       filter(year == 2018)%>%
         group_by(state)%>%
         summarise(sum_jail_pop = mean((black_jail_pop/total_jail_pop), na.rm=TRUE))%>%
         select(state, sum_jail_pop)

 
 new_data <- left_join(state_name, get_thing, by="state")

 data_thing<-new_data %>% 
 mutate(state = tolower(new_data$State))%>%
   select(state, sum_jail_pop)
 

   #Join eviction data to the U.S. shapefile
   
  
 
    #View(state_shape)
    # Draw the map setting the `fill` of each state using its eviction rate

     state_shape <- map_data("state") %>% # load state shapefile
       rename(state = region) %>% # rename for joining
       left_join(data_thing, by="state")
     
     plot_map <- ggplot(state_shape) +
      geom_polygon(
        mapping = aes(x = long, y = lat, group = group, fill = sum_jail_pop),
        color = "white", # show state outlines
        size = .1        # thinly stroked
      ) +
      coord_map() + # use a map-based coordinate system
      scale_fill_continuous(low = "#132B43", high = "Red") +
      labs(fill = "Black Jail Population Proportion")
     
     return(plot_map)
   }
   data_map1()
 
      # variable containing map styles (defined in next code snippet)
    
 
 ###function for white population rate
#   state_name <- read.csv("https://raw.githubusercontent.com/jasonong/List-of-US-States/master/states.csv")
#     
#     names(state_name)[names(state_name)== "Abbreviation"]<- "state"
#     
#     
#     wh_get_thing <- incarceration_df%>%
#       filter(year == 2018)%>%
#       group_by(state)%>%
#       summarise(sum_jail_pop = mean(black_pop_15to64/total_pop_15to64), na.rm=TRUE)%>%
#       select(state, sum_jail_pop)
#     
#     
#     wh_new_data <- left_join(state_name, wh_get_thing, by="state")
#   
#     wh_data_thing<-wh_new_data %>% 
#       mutate(state = tolower(wh_new_data$State))%>%
#       select(state, sum_jail_pop)
#     
#     
#     #Join eviction data to the U.S. shapefile
#     state_shape <- map_data("state") %>% # load state shapefile
#       rename(state = region) %>% # rename for joining
#       left_join(wh_data_thing, by="state") # join eviction data
#     #View(state_shape)
#    return(state_shape)
#   }
#      # Draw the map setting the `fill` of each state using its eviction rate
#    map_2 <- function(){
#      map2<- ggplot(state_shape) +
#       geom_polygon(
#         mapping = aes(x = long, y = lat, group = group, fill = sum_jail_pop),
#         color = "white", # show state outlines
#         size = .1        # thinly stroked
#       ) +
#       coord_map() + # use a map-based coordinate system
#       scale_fill_continuous(low = "#132B43", high = "Red") +
#       labs(fill = "Black Population Proportion")
#     return(map2)
#     
#    }
#    map_2()
#     # variable containing map styles (defined in next code snippet)
#     
#    #  tot_get_data <- incarceration_df%>%
#    #    filter(year == 2016)%>%
#    #    group_by(state)%>%
#    #    summarise(tot_sum_pop = mean((black_pop_15to64/total_pop_15to64), na.rm=TRUE))%>%
#    #    select(tot_sum_pop, state)
#    #  
#    #  View(tot_get_data)
#    #  
#    # new_data <- left_join(state_name, tot_get_data, by="state")%>%
#    #    mutate(state = tolower(new_data$state))%>%
#    #    select(state, tot_sum_pop)
#    #  View(new_data)
#    #  
#    #  
#    #  # Join eviction data to the U.S. shapefile
#    #  state_shape <- map_data("state") %>% # load state shapefile
#    #    rename(state = region) %>% # rename for joining
#    #    left_join(new_data, by="state") # join eviction data
#    #  #View(state_shape)
#    #  # Draw the map setting the `fill` of each state using its eviction rate
#    #  ggplot(state_shape) +
#    #    geom_polygon(
#    #      mapping = aes(x = long, y = lat, group = group, fill = tot_sum_pop),
#    #      color = "white", # show state outlines
#    #      size = .1        # thinly stroked
#    #    ) +
#    #    coord_map() + # use a map-based coordinate system
#    #    scale_fill_continuous(low = "#132B43", high = "Red") +
#    #    labs(fill = "Total Population Proportion Black Americans")
# # See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


