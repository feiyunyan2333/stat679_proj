library(shiny)
library(ggplot2)

file_path <- "/Users/lixinyao/Downloads/movies.csv"
movies_data <- read.csv(file_path)

movies_data$rating_group <- ifelse(movies_data$rating %in% c("R", "PG-13"), movies_data$rating, "Other")

ui <- fluidPage(
  titlePanel("Movie Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("question", "Choose Question:", choices = c("Relationship between Runtime and Gross", "Other Visualization")),
      conditionalPanel(
        condition = "input.question == 'Relationship between Runtime and Gross'",
        selectInput("genre", "Choose Genre:", choices = unique(movies_data$genre))
      ),
      conditionalPanel(
        condition = "input.question == 'Other Visualization'",
        selectInput("rating", "Choose Rating:", choices = unique(movies_data$rating_group))
      )
    ),
    
    mainPanel(
      plotOutput("visualization")
    )
  )
)

server <- function(input, output) {
  selected_data <- reactive({
    if (input$question == "Relationship between Runtime and Gross") {
      subset(movies_data, genre == input$genre)
    } else if (input$question == "Other Visualization") {
      subset(movies_data, rating_group == input$rating)
    }
  })
  
  output$visualization <- renderPlot({
    if (input$question == "Relationship between Runtime and Gross") {
      ggplot(selected_data(), aes(x = runtime, y = gross, color = rating_group)) +
        geom_point(alpha = 0.7) +
        scale_color_manual(values = c("R" = "red", "PG-13" = "green", "Other" = "blue")) +
        labs(title = paste("Relationship between Runtime and Gross (Movies with Top 100 gross in", input$genre, ")"),
             subtitle = "Colored by Rating Group",
             x = "Runtime (minutes)",
             y = "Gross (in billions)") +
        theme_minimal()
    } else if (input$question == "Other Visualization") {
      
      ggplot(selected_data(), aes(x = ..., y = ...)) +
        geom_...() +
        labs(title = "Other Visualization",
             subtitle = paste("Top 100 Movies with Rating:", input$rating),
             x = "X Axis Label",
             y = "Y Axis Label") +
        theme_minimal()
    }
  })
  
}



shinyApp(ui, server)