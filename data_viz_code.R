# Homework 1 (wootwoot)
# Youbeen Shim; YS3EP; otherwise known as "your favorite student"
# Due: 2017, Sep. 28th 5PM

#SETUP
# Load the best library in the world
library(tidyverse)

###############
# Part 1 - 1: #
###############

u_cuisine <- read_csv("usercuisine.csv")
r_cuisine <- read_csv("chefmozcuisine.csv")
rating <- read_csv("rating_final.csv")

# Restaurant Data #
# Datasets of interest to us:
#   usercuisine - userID; Rcuisine (user's prefered food type)
#   chefmozcuisine - placeID; Rcuisine (food type that the resturant)
#   rating_final - userID; placeID; rating; food_rating; service_rating

# Main challenge in cleaning: the dataset has some conflicting/multiple values that will
# inevitably generate duplicates we do not want!
# Secondary challenge: create a convincing visualization that conveys what I want

# MAIN: attempt 3 (...actually attempt number 6 but i deleted 5 and came back to work on 3)
# NOTE! Pipeline things: filter(), mutate(), group_by(), summarize(), ungroup(), arrange(), 
#                        inner_join(), right_join(), left_join(), anti_join()

#1-1
rating %>%
  # filter(userID=="U1004") %>% # in place to test things out before i use the full data
  inner_join(r_cuisine, by="placeID") %>%
  inner_join(u_cuisine, by="userID") %>%
  # concerns: have to delete the muliply generated data entries
  #-> should find a way to find a matching preference -> delete all other repeats
  #-> if none of the preferences match, select a random generated data and delete all else
  #-> concern: sometimes multiple preferences match with multiple food types offered
  mutate(matching=ifelse(Rcuisine.x==Rcuisine.y, 1, 0)) %>%
  # this allows me to see all the data and filter all the match if i want to later.
  # compared to just filter(Rcuisine.x==Rcuisine.y) -> will NOT work
  # filter(matching=="It's a match!") -> will only give me the matching ones
  # groupby userid, placeid, cuisine -> take median
  # group by user & placeid -> row_number
  # select(userID, placeID, rating, Rcuisine.x, Rcuisine.y, matched) %>% # to clean the dataset
  group_by(userID, placeID) %>%
  # summarize(med=mean(rating)) %>% #attempt to get rid of duplicates, does not do it fully
  arrange(desc(matching)) %>%
  filter(row_number()==1L) %>%
  mutate(samecuisine=ifelse(matching==1,"Same Cuisine","Different Cuisine")) %>%
  mutate(Rating=ifelse(rating==2, "2 Stars", ifelse(rating==1, "1 Star", "0 Stars"))) %>%
  ggplot(aes(x=matching, fill = Rating)) + 
  geom_bar(stat="count", position=position_dodge(), color="black") +
  labs(title="Restaurant Ratings based on Cuisine of Interest: \n Same Cuisine vs Different Cuisine") +
  xlab("Restaurant Rating") + ylab("") + theme_minimal() 
  #graph based on count

rating %>%
  inner_join(r_cuisine, by="placeID") %>%
  inner_join(u_cuisine, by="userID") %>%
  mutate(matching=ifelse(Rcuisine.x==Rcuisine.y, 1, 0)) %>%
  group_by(userID, placeID) %>%
  arrange(desc(matching)) %>%
  filter(row_number()==1L) %>%
  mutate(samecuisine=ifelse(matching==1,"Same","Different")) %>%
  mutate(Rating=ifelse(rating==2, "2 Stars", ifelse(rating==1, "1 Star", "0 Stars"))) %>%
  select(userID, placeID, samecuisine, Rating) %>%
  ggplot(aes(x=samecuisine)) +
  geom_bar(aes(fill=Rating), position="fill") +
  labs(title="Restaurant Ratings based on Cuisine of Interest: \n Same Cuisine vs Different Cuisine") +
  xlab("Restaurant Rating") + theme_minimal()
  #relative stacked bar chart

###############
# Part 1 - 2: #
###############

#1-2
rating %>%
  inner_join(r_cuisine, by="placeID") %>%
  inner_join(u_cuisine, by="userID") %>%
  filter(rating != 0) %>% 
  # The only difference btw 1-1 and 1-2
  mutate(matching=ifelse(Rcuisine.x==Rcuisine.y, 1, 0)) %>%
  group_by(userID, placeID) %>%
  arrange(desc(matching)) %>%
  mutate(samecuisine=ifelse(matching==1,"Same","Different")) %>%
  mutate(Rating=ifelse(rating==2, "2 Stars", ifelse(rating==1, "1 Star", "0 Stars"))) %>%
  select(userID, placeID, samecuisine, Rating) %>%
  ggplot(aes(x=samecuisine)) +
  geom_bar(aes(fill=Rating), position="fill") +
  labs(title="Restaurant Ratings based on Cuisine of Interest: \n Same Cuisine vs Different Cuisine") +
  xlab("Restaurant Rating") + theme_minimal()
#relative stacked bar chart

###############
# Part 1 - 3: #
###############
# If restaurant allows smoking and the customer is a smoker, will they be more likely
# to give higher rating?

userpro <- read.csv("userprofile.csv")
geo <- read.csv("geoplaces2.csv")

# Datasets to use:
# userprofile (smoker - TRUE,FALSE)
# geoplaces2 (smoking_area - none, not permitted, only at bar, permitted, section)
# rating_final (rating)

# First, join all the necessary data
smoke <- userpro %>%
  inner_join(rating, by="userID") %>%
  inner_join(geo, by="placeID") %>%
  # Select the variables of interest for the question
  select(userID, smoker, placeID, smoking_area, rating) %>%
  # Filter the entries that we do not have enough information on
  filter(smoking_area != "none") %>%
  # Create a variable "Smoker" that will lable user as either smoker or non-smoker
  mutate(smoker=ifelse(smoker=="true", "Smoker", "Non-Smoker")) %>% 
  # this step is only for ease of visualization
  mutate(newrating=ifelse(rating==2, "2 Stars", ifelse(rating==1, "1 Star", "0 Stars"))) %>%
  # re-label all the restaurants to one of "no smoking", "partial smoking", and "smoking"
  mutate(able2smoke=ifelse(smoking_area == "not permitted", "No Smoking Area", smoking_area)) %>%
  mutate(able2smoke=ifelse(smoking_area == "only at bar", "Limited Smoking Area", able2smoke)) %>%
  mutate(able2smoke=ifelse(smoking_area == "permitted", "Smoking Area", able2smoke)) %>%
  mutate(able2smoke=ifelse(smoking_area == "section", "Limited Smoking Area", able2smoke)) %>%
  # select final variables of interest - this will be the input to create mosaic plot
  select(smoker, able2smoke, rating)

mosaicplot(table(smoke), xlab="", ylab="", sub="Non-Smokers vs Smokers") + scale_fill_brewer(palette = "Blues")


############
# Part 2 : #
############
# Create a visualization that suggests that people use the bikes to get to work. 
# In order to be convincing, you should at least show variation over days of the week, and also times of day.
library(lubridate)
library(ggplot2)
bike <- read_csv("201508-citibike-tripdata.csv")

bike %>%
  select(starttime) %>%
  # Only ever need to use start time -late trips, extended trips, and otherwise unpredictable trips will be 
  # "silenced" by other data since there will be so little of it. If people use bikes to get to work, we 
  # need only concern ourselves with when they go to work
  mutate(starttime = mdy_hms(starttime)) %>%
  mutate(bikinghour=hour(starttime), bikingday=wday(starttime)) %>%
  # make hours and days into things that we can graph into :D
  group_by(bikinghour, bikingday) %>%
  summarize(count=n()) %>%
  ggplot(aes(x=bikinghour, y=count)) +
  geom_line(aes(color=as.factor(bikingday))) + # as.factor because R was treating everything as continuous...
  scale_fill_brewer(palette = "Dark2") + 
  theme_minimal()
