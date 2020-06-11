getwd()
#setwd('C:/Users/Sanmari Vivier/Dropbox/Personal/r-shiny-app/shiny_app/R scripts')

library(tidyverse)
library(lubridate)
library(tidyquant)
library(plotly)
library(ggthemes)

# Import dataset--------------------------------

data <- read_csv("ICD10_F43.2_patients.csv")
data %>% glimpse()
data_tib <- as_tibble(data)

data_tib %>% glimpse()

# Data exploration & Wrangling--------------------------------
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
    mutate(age = round(therapy_year - Birth_year),0) %>%
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
        title = "Visualisation of data set: Outliers removed, 1099 participants",
        subtitle = "Therapy sessions according to age and sex") +
    xlab("Age") +
    ylab("Sessions") +
    theme_tq()

fig1

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
    glimpse()

data_tib <- data_tib %>% 
    mutate(sessions_per_age = tapply(total_sessions, age, FUN=sum)) %>% 
    glimpse()

data_tib_selective <- data_tib %>% 
    select(c("gender","age", "total_people","sessions_per_age")) %>% 
    glimpse()

# Remove duplicates
data_tib_selective <- data_tib_selective[!duplicated(data_tib_selective[,c("gender","age", "total_people","sessions_per_age")]),] %>% 
    glimpse()

# Calculate average sessions per age
data_tib_selective <- data_tib_selective %>% 
    mutate(avg_sessions = round(sessions_per_age/total_people, 1)) %>% 
    glimpse()

# Rename column names
data_tib_selective <- data_tib_selective %>% 
    rename(average_sessions = avg_sessions) %>% 
    glimpse()

# Info to display in shiny app as a table
display_info <- data_tib_selective %>% 
    select(c("gender", "age","average_sessions")) %>% 
    glimpse()

display_info$age <- as.integer(display_info$age)

# Medical aid rates
medical_aid_rates <- read_csv("medical_aid_rates_2020.csv")
glimpse(medical_aid_rates)
medical_aid_rates <- as_tibble(medical_aid_rates) %>% 
                    arrange(rate,desc(rate)) %>% 
                    glimpse()

colors_for_plot <- c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695')

fig7<-ggplot(data=medical_aid_rates, aes(x=rate, y=medical_aid)) +
    geom_bar(stat="identity", fill = "dark grey")+
    geom_text(aes(label=rate), vjust=1.6, color="white", size=3.5)+
    labs(
       title = "Medical Aid Rates for Psychologists for Tariff Code 205 – 2020",
        subtitle = "Rate for Psychology assessment, consultation, counselling
                    and/or therapy (individual or family). Duration: 51-60min.",
        x = "Rate (ZAR)",
        y = "Medical Aid Provider"
        ) +
    theme_few()+
    coord_flip()

Gender <- c("Female", "Male")
Age <- seq(14,68,1)
?hover
# Shiny App---------------------------------
ui <- fluidPage(
    #<< first-row
    fluidRow(
        textOutput("text"),

    #<< second-row
    fluidRow(
        column(4,
                selectInput("gender", "Gender",
                               choices = Gender,
                               width = "100%")),
        column(4, selectInput("age", "Age", choices = Age, width = "100%"))),       
    
    #>> third row
    fluidRow(
        column(12, tableOutput("display_this"))),
    
    #>> fourth row
    fluidRow(
        column(6, plotOutput("medical_aid_rates", width = 500, height = 500)),
        column(6, plotlyOutput("visualise_data_set", width = 500, height = 500))),
    
    #>> fifth row
    fluidRow(
        textOutput("text2"))
))


server <- function(input, output, session) {
    
    selected <- reactive(display_info %>% 
                        filter(gender == input$gender, age == input$age))
    
    output$text <- renderText(
        "
        \n
        The rates displayed apply to the NHRPL (National Health Reference Price List) Code: 86205 and ICD10 Code: F43.2 for the year of 2020.
        \nAll tariffs are inclusive of VAT.\nRates are subject to change by your medical aid and the
        rates mentioned here are dependent on your level of cover. Please check with your medical aid.\n
        \n
        The information contained in this document is a guide only. The author cannot be held liable for any
        inaccuracies in the information.\n
        \n
        PLEASE VERIFY WITH YOUR MEDICAL AID.
        \n
        "
    )
    
    output$display_this <- renderTable(
        selected()
    )
    
    output$medical_aid_rates <- renderPlot({
        ggplot(data=medical_aid_rates, aes(x=medical_aid, y=rate)) +
            geom_bar(stat="identity", fill = "steelblue")+
            geom_text(aes(label=rate), vjust=0.5, hjust=1.3, color="white", size=3.5)+
            labs(
                title = "**2020 Medical Aid Rates for Psychologists: Tariff Code 86205",
                #subtitle = "Rate for Psychology assessment, consultation, counsellingand/or therapy (individual or family). Duration: 51-60min.",
                x = "South African Medical Aid Provider",
                y = "Rate (ZAR)"
            ) +
            theme_few()+
            theme(plot.title=element_text(size=9, face="bold", colour="black")) +
            theme(plot.subtitle=element_text(size=7, face="bold", colour="black")) +
            theme(axis.title.x = element_text(face="bold", colour="black")) +
            theme(axis.title.y = element_text(face="bold", colour="black")) +
            coord_flip()},res=96)
    
    output$visualise_data_set <- renderPlotly({
        print(
        ggplotly(ggplot(data = data_tib, 
               aes(x = age, y = total_sessions, color = gender)) + 
            geom_point(alpha = 0.5) + 
            labs(
                title = "Visualisation of data set: Outliers removed, 1099 participants",
                subtitle = "Therapy sessions according to age and sex") +
            xlab("Age") +
            ylab("Sessions") +
            theme_tq() +
            theme(plot.title=element_text(size=9, face="bold", colour="black")) +
            theme(plot.subtitle=element_text(size=7, face="bold", colour="black")) +
            theme(axis.title.x = element_text(face="bold", colour="black")) +
            theme(axis.title.y = element_text(face="bold", colour="black")) 
            ))
        })
    
    output$text2 <- renderText(
        "   **Rate for Psychology assessment, consultation, counsellingand/or therapy (individual or family). Duration: 51-60min."
    )
    
}

shinyApp(ui, server)

