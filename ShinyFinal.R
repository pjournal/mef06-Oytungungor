#libraries
library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)


# Read in the data
df <- read_excel("foreign_students_by_nationality_2021_2022.xlsx")

# Preprocessing
colnames(df) <- c("university", "type", "province", "nationality", "male_students", "female_students", "total_students")
df$male_students <- as.integer(df$male_students)
df$female_students <- as.integer(df$female_students)
df$total_students <- as.integer(df$total_students)

library(shiny)

# Define the UI
ui <- fluidPage(
  titlePanel("International Students"),
  sidebarLayout(
    sidebarPanel(
      selectInput("aggregate_by", "Aggregate by:",
                  choices = c("type", "province", "university"),
                  selected = "type"),
      selectInput("nationalities", "Nationalities:",
                  choices = unique(df$nationality),
                  multiple = TRUE)
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # Filter the data based on the selected nationalities
  filtered_data <- df %>% filter(nationality %in% input$nationalities)

}

  # Reactive expression to generate the plot
  plot <- reactive({
    if (input$aggregate_by == "type") {
      data <- filtered_data %>%
        group_by(nationality, type) %>%
        summarise(count = sum(total_students))
    }
      
      
      ggplot(data, aes(x = nationality, y = count, fill = type)) +
        geom_bar(stat = "identity", position = "dodge") +
        ylab("Student Count") +
        labs(x = "Nationality", y = "Student Count", fill = "Type")
    } else if (input$aggregate_by == "province") {
      data <- filtered_data %>%
        group_by(nationality, province) %>%
        summarise(count = sum(total_students)) %>%
        arrange(desc(count)) %>%
        top_n(10)
      
      ggplot(data, aes(x = nationality, y = count, fill = province)) +
        geom_bar(stat = "identity", position = "dodge") +
        ylab("Student Count") +
        labs(x = "Nationality", y = "Student Count", fill = "Province")
    } else if (input$aggregate_by == "university") {
      data <- filtered_data %>%
        group_by(nationality, university) %>%
        summarise(count = sum(total_students)) %>%
        arrange(desc(count)) %>%
        top_n(10)
    }
      ggplot(data, aes(x = nationality, y = count, fill = university))
             shinyApp(ui = ui, server = server)
             
             