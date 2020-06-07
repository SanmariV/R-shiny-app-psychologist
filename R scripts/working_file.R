

# Import dataset--------------------------------
data <- read_csv("data/ICD10_F43.2_patients.csv")
data_tib <- as_tibble(data)

# Explore data----------------------------------
males_tib <- data_tib %>% 
            filter(data_tib$Sex == "m") %>% 
            glimpse()

females_tib <- data_tib %>% 
               filter(data_tib$Sex == "f") %>% 
               glimpse()

# Data wrangling-------------------------------

