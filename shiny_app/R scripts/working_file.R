install.packages("ggthemes")

library(tidyverse)
library(lubridate)
library(tidyquant)
library(plotly)
library(ggthemes)

# Import dataset--------------------------------
data <- read_csv("shiny_app/data/ICD10_F43.2_patients.csv")
data %>% glimpse()
data_tib <- as_tibble(data)

data_tib %>% glimpse()

# Replace sex variables with more descriptive names
data_tib$gender <- as.character(data_tib$gender)
data_tib$gender[data_tib$gender == "f"] <- "Female"
data_tib$gender[data_tib$gender == "m"] <- "Male"
data_tib$gender <- as.factor(data_tib$gender)

data_tib %>% glimpse()

data_tib <- data_tib %>% 
    mutate(Birth_year = year(date_of_birth)) %>%
    glimpse()

data_tib <- data_tib %>% 
    mutate(age = therapy_year - Birth_year) %>%
    glimpse()

# How many males in dataset?
males_tib <- data_tib %>% 
    filter(data_tib$gender == "Male") %>% 
    glimpse()

# How many females in dataset?
females_tib <- data_tib %>% 
    filter(data_tib$gender == "Female") %>% 
    glimpse()

# Visualise the dataset
fig1 <- ggplot(data = data_tib, 
               aes(x = age, y = total_sessions, color = gender)) + 
    geom_point(alpha = 0.5) + 
    labs(
        title = "Visualisation of data set",
        subtitle = "Therapy sessions according to age and sex") +
    xlab("Age") +
    ylab("Sessions") +
    theme_tq()

# Add interactivity
ggplotly(fig1)

# Which age group attended the most sessions
fig2 <- ggplot(data = data_tib, 
               aes(x = age, fill = total_sessions, color = gender)) + 
    geom_bar() + 
    #labs(
    #title = "Visualisation of data set",
    #subtitle = "Therapy sessions according to age and sex",
    #x_lab= "Age",
    #y_lab="Sessions")+
    theme_tq()

ggplotly(fig2)

# Remove outliers
# Remove all ages less than 13 and more than 68
# Remove all data where 4 < total-sessions <= 20

data_tib <- data_tib %>% 
    filter(age > 13, age < 68, total_sessions<= 20,total_sessions>4) %>% 
    glimpse()

data_tib <- data_tib %>% 
            add_count(age,name="total_people") %>%
            group_by(age) %>% 
            arrange(age) %>%
            view()

data_tib <- data_tib %>% 
            mutate(sessions_per_age = tapply(total_sessions, age, FUN=sum)) %>% 
            view()

data_tib_selective <- data_tib %>% 
                        select(c("gender","age", "total_people","sessions_per_age")) %>% 
                        view()

# Remove duplicates
data_tib_selective <- data_tib_selective[!duplicated(data_tib_selective[,c("gender","age", "total_people","sessions_per_age")]),] %>% 
                    view()

# Calculate average sessions per age
data_tib_selective <- data_tib_selective %>% 
                    mutate(avg_sessions = round(sessions_per_age/total_people, 1)) %>% 
                    view()

# Rename column names
data_tib_selective <- data_tib_selective %>% 
                rename(average_sessions = avg_sessions) %>% 
                glimpse()

# Info to display in shiny app as a table
display_info <- data_tib_selective %>% 
                select(c("gender", "age","average_sessions")) %>% 
                view()

# Medical aid rates
medical_aid_rates <- read_csv("shiny_app/data/medical_aid_rates_2020.csv")
view(medical_aid_rates)
medical_aid_rates <- as_tibble(medical_aid_rates)

colors_for_plot <- c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695')

fig7<-ggplot(data=medical_aid_rates, aes(x=medical_aid, y=rate)) +
    geom_bar(stat="identity", fill = "grey")+
    geom_text(aes(label=rate), vjust=0, hjust=-0.1, size=3.5)+
    xlab("Rate (ZAR)") +
    ylab("Medical Aid Provider")+
    theme_tq()+
    coord_flip()

fig7 + theme_few() + scale_colour_few()

