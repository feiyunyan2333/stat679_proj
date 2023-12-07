library(shiny)
library(ggplot2)
library(tidyverse)
library(tmap)
library(mice)

data("World")
colnames(World)[2] <- "release_country"
movie <- read_csv('https://raw.githubusercontent.com/VivianBeagle/STAT679/main/movies.csv')
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
movie1 <- movie |>
  drop_na() |>
  mutate(genre_numeric = as.numeric(factor(genre)))

movie.numeric <- movie1[, c(4,6,7,12,13,15,16)]
corr <- cor(movie.numeric)

movies_data$rating_group <- ifelse(movies_data$rating %in% c("R", "PG-13"), movies_data$rating, "Other")

genre_choice <- c("Adventure", "Action", "Comedy", "Biography", "Animation")

ui <- fluidPage(
  titlePanel("Movie Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("question", "Choose Question:", choices = c("Relationship between Runtime and Gross", "Movie Releases Analysis","Correlation Matrix for Numerical Features","Gross vs Budget",'Release Date Analysis',"Directors and Actors")),
      conditionalPanel(
        condition = "input.question == 'Relationship between Runtime and Gross'",
        selectInput("genre", "Choose Genre:", choices = unique(movies_data$genre))
      ),
      conditionalPanel(
        condition = "input.question == 'Movie Releases Analysis'",
        selectInput("place", "Choose country:", choices = unique(movie.long$release_country)),
        selectInput("genre2","Choose Genre:", choices = genre_choice)
      ),
      conditionalPanel(
        condition = "input.question == 'Correlation Matrix for Numerical Features'",
      ),
      conditionalPanel(
        condition = "input.question == 'Gross vs Budget'",
        selectInput("genre3", "Choose Genre:", choices = unique(movies_data$genre))
      ),
      
      conditionalPanel(
        condition = "input.question == 'Release Date Analysis'",
        selectInput("type", "Choose type:", choices = c("release date","release year")),
      ),
      conditionalPanel(
        condition = "input.question == 'Directors and Actors'",
        selectInput("type1", "Choose type:", choices = c("Director","Actor")),
        selectInput("type2", "Choose type:", choices = c("vs Gross","vs Score")),
      ),
      
      
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
    } else if (input$question == "Movie Releases Analysis") {
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
    
    else if (input$question == "Movie Releases Analysis") {
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
    else if (input$question == "Correlation Matrix for Numerical Features") {
      movie1 <- movie |>
        drop_na() |>
        mutate(genre_numeric = as.numeric(factor(genre)))
      
      movie.numeric <- movie1[, c(4,6,7,12,13,15,16)]
      corr <- cor(movie.numeric)
      ggplot(melt(corr), aes(Var1, Var2)) +
        geom_tile(aes(fill = value)) +
        geom_text(aes(label = round(value, 2))) +
        scale_fill_gradient2() +
        labs(x = "Movie Features", y = "Movie Features", title = "Correlation Matric for Numericak Features") +
        theme_minimal()
      
    }
    
    else if(input$question ==  'Gross vs Budget'){
      data=subset(movies_data, genre == input$genre3)
      ggplot(data, aes(budget, gross), color = rating_group) +
        geom_point() +
        geom_smooth(method = "lm", se = TRUE) +
        labs(title = "Gross vs Budget") +
        theme_minimal()
      
    }
    
    else if(input$question ==  'Release Date Analysis'){
      if(input$type=="release date"){
        movie2 <- movie |>
          separate(col = released, into = c("release_month", "release_year"), sep = ",") |>
          separate(col = release_month, into = c("release_month"), sep = " ", extra = "drop") |>
          separate(col = release_year, into = c("release_year", "release_country"), sep = "\\(") |>
          separate(col = release_country, into = c("release_country"), sep = "\\)", extra = "drop")
        
        
        month_order <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
        
        movie2.2 <- movie2 |>
          drop_na() |>
          group_by(release_month) |>
          summarise(movies = n(),
                    sum_gross = mean(gross)) |>
          mutate(log_gross = log10(sum_gross))
        
        movie2.2$release_month <- factor(movie2.2$release_month, levels = month_order)
        
        ggplot(movie2.2, aes(x = release_month)) +
          geom_col(aes(y = movies, fill = release_month)) +
          geom_point(aes(y = sum_gross / 342458.8), size = 3, col = "blue") +
          geom_text(aes(y = movies, label = paste(sep = "\n", "Number", movies))) +
          geom_text(aes(y = sum_gross/ 342458.8 + 50, label = paste(sep = "\n", "Gross", round(sum_gross, 0))), col = "blue") +
          scale_y_continuous(name = "Number of Movies", sec.axis = sec_axis(~.*342458.8, name = "Average of Gross")) +
          theme(axis.text.x = element_text(angle = 45),legend.position = "none")
        
      
      }
      else{
        
        movie2 |>
          drop_na() |>
          group_by(release_year) |>
          summarise(movies = n(),
                    mean_gross = mean(gross)) |>
          mutate(log_gross = log10(mean_gross)) |>
          ggplot(aes(x = release_year)) +
          geom_col(aes(y = movies, fill = release_year)) +
          geom_point(aes(y = mean_gross / 1257285), size = 3, col = "blue") +
          geom_text(aes(y = movies, label = paste(sep = "\n", "Number", movies)), check_overlap = TRUE) +
          geom_text(aes(y = mean_gross/ 1257285 - 5, label = round(mean_gross, 0)), col = "blue", check_overlap = TRUE) +
          scale_y_continuous(name = "Number of Movies", sec.axis = sec_axis(~.*342458.8, name = "Average of Gross")) +
          theme(axis.text.x = element_text(angle = 90),
                legend.position = "none")
      }
    }
    
    else if(input$question ==  'Directors and Actors'){
      movie3 <- movie2 |>
        drop_na() |>
        group_by(director, star) |>
        summarise(use = n(),
                  gross = mean(gross),
                  score = mean(score))
      movie_dir <- movie3 |>
        group_by(director) |>
        summarise(gross = mean(gross),
                  score = mean(score)) |>
        arrange(desc(gross))
      movie_star <- movie3 |>
        group_by(star) |>
        summarise(gross = mean(gross),
                  score = mean(score)) |>
        arrange(desc(gross))
      if(input$type1=="Director"){
        if(input$type2=="vs Score"){
          ggplot(movie_dir[1:15,]) +
            geom_col(aes(x = score, y = reorder(director, score), fill = director)) +
            labs(x = "average score", y = "director", title = "Director vs Score (top 15)") +
            theme(legend.position = "none")
          #        axis.text.x = element_text(angle = 90))
        }
        else{
          ggplot(movie_dir[1:15,]) +
            geom_col(aes(x = gross, y = reorder(director, gross), fill = director)) +
            labs(x = "average gross", y = "director", title = "Director vs Gross (top 15)") +
            theme(legend.position = "none",
                  axis.text.x = element_text(angle = 90))
        }
      }
      else{
        if (input$type2=="vs Score"){
          ggplot(movie_star[1:15,]) +
            geom_col(aes(x = score, y = reorder(star, score), fill = star)) +
            labs(x = "average score", y = "director", title = "Star vs Score (top 15)") +
            theme(legend.position = "none",
            )
        }
        else{
          ggplot(movie_star[1:15,]) +
            geom_col(aes(x = gross, y = reorder(star, gross), fill = star)) +
            labs(x = "average gross", y = "director", title = "Star vs Gross (top 15)") +
            theme(legend.position = "none",
                  axis.text.x = element_text(angle = 90))
        }
        
      }
      
    }
    
    
  })
  
}

shinyApp(ui, server)
