rm(list = ls())

library(ggplot2)
library(dplyr)
library(tidyr)

setwd("/Your directory")


# Exercise 1

sparrows <- read.table("Sparrows.txt", h = T)

sparrows_day4 <-  filter(sparrows, day == 4) # Create a subset from day four

error.bar <- sparrows_day4 %>%
  group_by(rank_name) %>%
  summarise(averages = mean(logit.motility, na.rm = T),
            sem = sd(logit.motility, na.rm = T)/sum(is.na(logit.motility) != T)) %>%
  as.data.frame() # Create a summary table with means and standard errors for the error bars.

error.bar # Make sure it is ok. Note that there are NAs in the data, so you might need some tricks to get mean and sd to work, as well as to get the total number of observations.
  
ex2 <- ggplot(sparrows_day4, aes(x = rank_name, y = logit.motility)) + 
  geom_jitter(width = 0.3, colour = "lightblue") + # Now you have your jitter plot
  geom_errorbar(data = error.bar, # Override the data
                mapping = aes(x = rank_name, # Override the aes
                              y = averages,
                              ymin = averages - sem,
                              ymax = averages + sem),
                width = 0.2) + # Now you have your errorbars
  geom_point(data = error.bar, # Override the data
             mapping = aes(x = rank_name, #Override the aes
                           y = averages),
             size = 0.7) + # Now you have the means plotted
  scale_x_discrete(labels = c("Dominant",
                              "Subordinate 1",
                              "Subordinate 2",
                              "Subordinate 3")) +
  labs( x = "Social rank",
        y = "Logit(Proportion of motile sperm)",
        title = "Ejaculate quality covaries with social rank \n of male House Sparrows") +
  theme_classic()
print(ex2)


#Exercise 2


ex3 <- ggplot(filter(sparrows,
                     manipulation == "before"), #subset your data
              aes(x = day,
                  y = mda)) +
  scale_y_log10(breaks = c(0,1,5,10,20,40,60),
                "log10 (MDA in sperm)") + #log-scale your y axis
  geom_line(aes(group = id), # Make lines by the id of each individual
            colour = "lightblue") +
  geom_point(colour = "darkblue") + # add your points
  scale_x_discrete(breaks = c(1,2,4),
                   labels = c(0, 24, 72),
                   "Time of sexual abstinence (hours)") +
  labs(title = "Sexual abstinence leads to increased oxidative \n damage in ejaculates of House Sparrows") +
  theme_classic()
  
print(ex3)

# Exercise 3

data("ChickWeight") # Read your data

str(ChickWeight) # Control what is within your data

summ_chicks <- ChickWeight %>% # take the table
  group_by(Time, Diet) %>% # Group the table by Time and Diet
  summarise("Weight" = mean(weight)) %>% # Take the grouped table and get the mean weights and put them in a new column called "Weight
  as.data.frame() # Ungroup the summarized table (Now your summary table is like any other table in R)

str(summ_chicks) # Control your new summary table

p <- ggplot(summ_chicks, #data
            aes(x = Time, # Coordinate layer
                y = Weight,
                shape = Diet, # Point shape for each diet
                linetype = Diet)) # Line type for each diet
p + geom_point() + # Add points to the mapping above
  geom_path() + # Add lines
  theme_classic() + # Make the figure nicer
  theme(legend.position = c(0.1,0.8), # Define the position of your legend
        plot.title = element_text(hjust = 0.5)) + # Define the poistion of your title
  labs(title = "Weights of chicks fed \non 4 different diets",
       x = "Time (days)",
       y = "Chick weight (g)") # Add title and axes names.



# Bonus


sparrows_after <- filter(sparrows, manipulation == "after") #subset data

sparrows_after <- bind_cols(sparrows_after,
                            rename(select(filter(sparrows,
                                                 manipulation == "before"),
                                          rank_name),
                                   rank_0 = rank_name)) #add to the subset the ranks before the manipulation


levels(sparrows_after$rank_0) <- c("Dominant", "Subordinate 1", "Subordinate 2", "Subordinate 3") #Change the level names of rank_name

levels(sparrows_after$rank_name) <- c("Dominant", "Subordinate 1", "Subordinate 2", "Subordinate 3") # same as above


bonus <- ggplot(data = sparrows_after,
                aes(x = rank_name,
                    y = logit.motility)) +
  geom_jitter(colour = "darkblue", size = 0.4, width = 0.3) +
  scale_x_discrete(labels = c("Dominant", "Subordinate 1", "Subordinate 2", "Subordinate 3")) +
  facet_wrap( ~ rank_0, ncol = 2 ) + #Create the multiple panel plot using the initial ranks to split the data.
  stat_summary(fun.y = mean, colour="violet", geom="line", na.rm = TRUE, aes(group=1)) + #add a line the goes through the mean of each rank.
  theme_classic() +
  labs(x = "Final social rank",
       y = "logit(Proportion of motile sperm)",
       title = "Male House Sparrows adapt their ejaculate \n to a new position in a social hierarchy")

print(bonus)
