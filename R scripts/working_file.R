tidyverse_packages()

# Import dataset--------------------------------
data <- read_csv("data/ICD10_F43.2_patients.csv")
data_tib <- as_tibble(data)

# Explore data----------------------------------


# Data wrangling-------------------------------
data_tib <- data_tib %>% 
            mutate(Birth_year = year(`date-of-birth`)) %>%
            glimpse()

data_tib <- data_tib %>% 
    mutate(age = `therapy-year` - Birth_year) %>%
    glimpse()

males_tib <- data_tib %>% 
    filter(data_tib$Sex == "m") %>% 
    glimpse()

females_tib <- data_tib %>% 
    filter(data_tib$Sex == "f") %>% 
    glimpse()

fig <- ggplot(data = data_tib, aes(x = age, y = `total-sessions`, color = Sex)) + 
    geom_point(alpha = 0.5) + 
    labs(
        title = "Visualisation of data set",
        subtitle = "Therapy sessions according to age and sex",
        x_lab= "Age",
        y_lab="Sessions")+
    theme_tq()

ggplotly(fig)


