tidyverse_packages()

library(tidyverse)
library(lubridate)
# Import dataset--------------------------------
data <- read_csv("data/ICD10_F43.2_patients.csv")
data_tib <- as_tibble(data)
data_tib %>% glimpse()

# Data wrangling-------------------------------

# Replace sex variables with more descriptive names
data_tib$gender <- as.character(data_tib$gender)
data_tib$gender[data_tib$gender == "f"] <- "Female"
data_tib$gender[data_tib$gender == "m"] <- "Male"
data_tib$gender <- as.factor(data_tib$gender)

data_tib <- data_tib %>% 
            mutate(Birth_year = year(date_of_birth)) %>%
            glimpse()

data_tib <- data_tib %>% 
    mutate(age = `therapy-year` - Birth_year) %>%
    glimpse()

# How many males in dataset?
males_tib <- data_tib %>% 
    filter(data_tib$gender == "m") %>% 
    glimpse()

# How many females in dataset?
females_tib <- data_tib %>% 
    filter(data_tib$gender == "f") %>% 
    glimpse()

# Visualise the dataset
fig1 <- ggplot(data = data_tib, aes(x = age, y = `total-sessions`, color = Sex)) + 
    geom_point(alpha = 0.5) + 
    labs(
        title = "Visualisation of data set",
        subtitle = "Therapy sessions according to age and sex",
        x_lab= "Age",
        y_lab="Sessions")+
    theme_tq()

# Add interactivity
ggplotly(fig1)

# Which age group attended the most sessions
fig2 <- ggplot(data = data_tib, aes(x = age, fill = `total-sessions`, color = Sex)) + 
    geom_bar() + 
    #labs(
        #title = "Visualisation of data set",
        #subtitle = "Therapy sessions according to age and sex",
        #x_lab= "Age",
        #y_lab="Sessions")+
    theme_tq()

ggplotly(fig2)

# Remove outliers
# Remove all ages less than 13 and more than 70
# Remove all data where 4 < total-sessions <= 20

data_tib <- data_tib %>% 
            filter(age > 12, age < 71, `total-sessions`<= 20,`total-sessions`>4) %>% 
            glimpse()

# Which age group attended the most sessions


# check if outliers were removed
summary(data_tib)
fig_no_outliers <- ggplot(data = data_tib, aes(x = age, y = `total-sessions`, color = Sex)) + 
    geom_point(alpha = 0.5) + 
    labs(
        title = "Visualisation of data set",
        subtitle = "Therapy sessions according to age and sex",
        x_lab= "Age",
        y_lab="Sessions")+
    theme_tq()

# Visualisation with outliers removed
fig_no_outliers
ggplotly(fig_no_outliers)

# Split up data into females and males
males_tib <- data_tib %>% 
    filter(Sex == "Male") %>% 
    glimpse()

females_tib <- data_tib %>% 
    filter(Sex =="Female") %>% 
    glimpse()

# Split data up into data and labels (prepare to apply machine learning model)
female_dataset <- females_tib %>% 
                select(age) %>% 
                glimpse()

female_labels <- females_tib %>% 
                select(`total-sessions`) %>% 
                glimpse()

males_dataset <- males_tib %>% 
                select(age) %>% 
                glimpse()

males_labels <- males_tib %>% 
                select(`total-sessions`) %>% 
                glimpse()




