library(shiny)
library(ggplot2)
library(tidyverse)
library(tmap)
library(mice)

data("World")
colnames(World)[2] <- "release_country"

file_path <- "https://raw.githubusercontent.com/feiyunyan2333/stat679_proj/main/movies.csv"
movies_data <- read_csv(file_path)

movie2 <- movies_data |>
  separate(col = released, into = c("release_month", "release_year"), sep = ",") |>
  separate(col = release_month, into = c("release_month"), sep = " ", extra = "drop") |>
  separate(col = release_year, into = c("release_year", "release_country"), sep = "\\(") |>
  separate(col = release_country, into = c("release_country"), sep = "\\)", extra = "drop")

movie.long <- movie2 |>
  drop_na() |>
  mutate(if_genre = 1) |>
  pivot_wider(names_from = genre, values_from = if_genre)
movie.long[is.na(movie.long)] = 0


movies_data$rating_group <- ifelse(movies_data$rating %in% c("R", "PG-13"), movies_data$rating, "Other")

genre_choice <- c("Adventure", "Action", "Comedy", "Biography", "Animation")

ui <- fluidPage(
  titlePanel("Movie Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("question", "Choose Question:", choices = c("Relationship between Runtime and Gross", "Movie Releases Analysis")),
      conditionalPanel(
        condition = "input.question == 'Relationship between Runtime and Gross'",
        selectInput("genre", "Choose Genre:", choices = unique(movies_data$genre))
      ),
      conditionalPanel(
        condition = "input.question == 'Movie Releases Analysis'",
        selectInput("place", "Choose country:", choices = unique(movie.long$release_country)),
        selectInput("genre2","Choose Genre:", choices = genre_choice)
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
    } else {
      subset(movies_data, country == input$place & genre == input$genre)
    }
  })
  
  # map_data <- reactive(if (input$question == "Movie Releases Analysis"){
  #   filter(World, name == input$place)
  # })

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
       } 
   
    else {
      movie.us <- movie.long |>
        group_by(release_country, release_year) |>
        summarise(gross = mean(gross), score = mean(score),
                  runtime = mean(runtime), cnt = n(),
                  Adventure = sum(Adventure),
                  Action = sum(Action),
                  Comedy = sum(Comedy),
                  Biography = sum(Biography), Animation = sum(Animation)) |>
        mutate(release_year = as.numeric(release_year)) |>
        filter(release_country == input$place)
      
     years <- sort(unique(movie.us$release_year), decreasing = TRUE)
      
     movie.us <- movie.us |>
        filter(release_year %in% years[1:6]) |>
        mutate(release_year = as.character(release_year))
     
      
    plot_data <- World[, c(2)] |>
        filter(release_country == input$place) |>
        left_join(movie.us)
      
    fill.index <- which(genre_choice == input$genre2)

    tm_shape(plot_data) +
      tm_polygons(col = as.character(genre_choice[fill.index])) +#, style = "jenks") +
      tm_facets(by = "release_year", ncol = 3)
        
    }
  })
  
}

shinyApp(ui, server)