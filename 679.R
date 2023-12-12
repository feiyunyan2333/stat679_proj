library(shiny)
library(ggplot2)
library(tidyverse)
library(tmap)
library(mice)
library(reshape2)
library(tidygraph)
library(ggraph)
library(MASS)
library(tidymodels)
library(tidyr)
library(rpart.plot)


data("World")
colnames(World)[3] <- "release_country"
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

movie <- read_csv('https://raw.githubusercontent.com/feiyunyan2333/stat679_proj/main/movies.csv')
movie2.2 <- movie |>
  separate(col = released, into = c("release_month", "release_year"), sep = ",") |>
  separate(col = release_month, into = c("release_month"), sep = " ", extra = "drop") |>
  separate(col = release_year, into = c("release_year", "release_country"), sep = "\\(") |>
  separate(col = release_country, into = c("release_country"), sep = "\\)", extra = "drop") |>
  drop_na() |>
  filter(release_month %in% c("June", "July", "May", "December",  "October",   "September", "February",  "April", "August", "March", "November",  "January"))

pred <- movie2.2[c(14,3,5,6,7,8,9,15,17)]

genre.cnt <- pred |>
  group_by(genre) |>
  summarise(gross = mean(gross)) |>
  slice_max(gross, n = 5) |>
  pull(genre)

month.cnt <- pred |>
  group_by(release_month) |>
  summarise(gross = mean(gross)) |>
  arrange(-gross) |>
  slice_max(gross, n = 5) |>
  pull(release_month)

country.cnt <- pred |>
  group_by(release_country) |>
  summarise(gross = mean(gross)) |>
  arrange(-gross) |>
  slice_max(gross, n = 5) |>
  pull(release_country)

for (i in 1:dim(pred)[1]){
  
  if (pred$genre[i] %in% genre.cnt){
    for (j in 1:length(genre.cnt)){
      if (pred$genre[i] == genre.cnt[j]){
        pred$genre[i] = length(genre.cnt)+1-j
        break
      } 
    }
  }else {
    pred$genre[i] = 0
  }
  
  if (pred$release_month[i] %in% month.cnt){
    for (j in 1:length(month.cnt)){
      if (pred$release_month[i] == month.cnt[j]){
        pred$release_month[i] = length(month.cnt)+1-j
        break
      } 
    }
  }else {
    pred$release_month[i] = 0
  }
  
  if (pred$release_country[i] %in% country.cnt){
    for (j in 1:length(country.cnt)){
      if (pred$release_country[i] == country.cnt[j]){
        pred$release_country[i] = length(country.cnt)+1-j
        break
      } 
    }
  }else {
    pred$release_country[i] = 0
  }
  
}

pred.new <- pred |>
  mutate(genre = as.numeric(genre), release_month = as.numeric(release_month),
         release_year = as.numeric(release_year), release_country = as.numeric(release_country),
         runtime = log1p(runtime), budget = log1p(budget)) |>
  drop_na()

weight <- pred.new$votes / sum(pred.new$votes)

pred.new <- pred.new[, c(-6, -7, -8)]

q <- quantile(pred.new$release_year, 0.9)
for (i in 1:dim(pred.new)[1]){
  if (pred.new$release_year[i] < q) {
    pred.new$release_year[i] <- 0
  }else {
    pred.new$release_year[i] = pred.new$release_year[i] - q
  }
}

# Create a decision tree model specification
model <- rpart(budget ~., data = pred.new, weights = weight, method = "anova")



genre_choice <- c("Adventure", "Action", "Comedy", "Biography", "Animation")

ui <- fluidPage(
  titlePanel("Movie Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("question", "Choose Question:", choices = c("Dataset Overview", "Region Analysis","Runtime Analysis","Budget Analysis",'Release Time Analysis',"Directors and Actors Analysis")),
      
      conditionalPanel(
        condition = "input.question == 'Runtime Analysis'",
        selectInput("gross_score", "Choose Analysis Subject:", choices = c("Runtime vs Gross", "Runtime vs Score")),
        conditionalPanel(
          condition = "input.gross_score == 'Runtime vs Gross'",
          sliderInput("year_range1", "Select Year Range:", min = min(movie$year), max = max(movie$year), value = c(min(movie$year), max(movie$year)), step = 1),
          selectInput("genre", "Choose Genre:", choices = unique(movies_data$genre))
        ),
        conditionalPanel(
          condition = "input.gross_score == 'Runtime vs Score'",
          uiOutput("score_message")
        )),
      
      conditionalPanel(
        condition = "input.question == 'Region Analysis'",
        selectInput("Country", "Choose Analysis Type:", choices = c("Release Movie Number","Overall Country Distribution")),
        conditionalPanel(
          condition = "input.Country == 'Release Movie Number'",
          selectInput("place", "Choose Region:", choices = unique(movie.long$release_country)),
          selectInput("genre2","Choose Genre:", choices = genre_choice)),
        conditionalPanel(
          condition = "input.Country == 'Overall Country Distribution'",
          uiOutput("country_message")
        ),  
      ),
      
      conditionalPanel(
        condition = "input.question == 'Dataset Overview'",
        selectInput("data", "Choose Analysis Type:", choices = c("Correlation Matrix for Numerical Features","Data Table Sorted by Score","Prediction Analysis")),
        conditionalPanel(
          condition = "input.data == 'Correlation Matrix for Numerical Features'",),
        conditionalPanel(
          condition = "input.data == 'Data Table Sorted by Score'",
          uiOutput("data_message")),  
        conditionalPanel(
          condition = "input.data == 'Prediction Analysis'",
          selectInput("genre9", "Genre", choice = c(genre.cnt, "Other")),
          selectInput("country", "Release Region", choice = c(country.cnt, "Other")),
          selectInput("month", "Release Month", choice = c(month.cnt, "Other")),
          numericInput("year", "Release Year", value = 2020, step = 1),
          sliderInput("runtime", "Runtime (min)", min = 0, max = 240, value = 0, step = 0.1),
          dataTableOutput("pred")
        ),
      ),
      
      conditionalPanel(
        condition = "input.question == 'Budget Analysis'",
        selectInput("budget", "Choose Visualization Subject:", choices = c("Budget vs Gross","Budget vs Score")),
        conditionalPanel(
          condition = "input.budget == 'Budget vs Gross'",
          sliderInput("year_range", "Select Year Range:", min = min(movie$year), max = max(movie$year), value = c(min(movie$year), max(movie$year)), step = 1),
          selectInput("genre3", "Choose Genre:", choices = unique(movies_data$genre))
        ),
        conditionalPanel(
          condition = "input.budget == 'Budget vs Score'",
          uiOutput("budget_message")
        ),  
      ),
      
      conditionalPanel(
        condition = "input.question == 'Release Time Analysis'",
        selectInput("type", "Choose Analysis Subject:", choices = c("release date","release year")),
      ),
      conditionalPanel(
        condition = "input.question == 'Directors and Actors Analysis'",
        selectInput("type1", "Choose Analysis Subject:", choices = c("Director","Actor")),
        selectInput("type2", "Choose Analysis Subject:", choices = c("vs Gross","vs Score")),
      ),
    ),
    
    mainPanel(
      plotOutput("visualization"),
    )
  )
)

server <- function(input, output) {
  selected_data <- reactive({
    if (input$question == "Runtime Analysis") {
      subset(movies_data, genre == input$genre)
    } else if (input$question == "Movie Releases Analysis") {
      subset(movies_data, country == input$place & genre == input$genre)
    } 
  })
  
  output$visualization <- renderPlot({
    if (input$question == "Runtime Analysis") {
      if (input$gross_score == 'Runtime vs Gross'){
        data <- subset(movies_data, genre == input$genre & year >= input$year_range1[1] & year <= input$year_range1[2])
        data$rating_group <- ifelse(data$rating %in% c("R","PG", "PG-13"), data$rating, "Other")
        ggplot(data, aes(x = runtime, y = gross, color = rating_group)) +
          geom_point(alpha = 0.7) +
          geom_smooth(method = "lm", se = TRUE,aes(group = 1)) +
          scale_color_manual(values = c("R" = "red", "PG-13" = "green","PG"="orange", "Other" = "blue")) +
          labs(title = paste("Relationship between Runtime and Gross (Movies with Top 100 gross in", input$genre, ")"),
               subtitle = "Colored by Rating Group",
               x = "Runtime (minutes)",
               y = "Gross (in billions)") +
          theme_minimal()
        
      }else if (input$gross_score == 'Runtime vs Score') {
        output$score_message <- renderUI({
          link <- "https://vivianbeagle.github.io/STAT679/runtime.html"
          message <- paste("Click this link: ", tags$a(href = link, target = "_blank", "Click here"))
          return(HTML(message))
        })
      }}
    
    else if (input$question == "Region Analysis") {
      if (input$Country == 'Release Movie Number'){
        filterCountry = input$place
        if (filterCountry == "United States") {filterCountry = "United States of America"}
        if (filterCountry == "Bahamas") {filterCountry = "The Bahamas"}
        
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
          mutate(release_year = as.character(release_year),
                 release_country = filterCountry)

        plot_data <- World[, c(3)] |>
          filter(release_country == filterCountry) |>
          left_join(movie.us)
        
        fill.index <- which(genre_choice == input$genre2)
        
        tm_shape(plot_data) +
          tm_polygons(col = as.character(genre_choice[fill.index])) +#, style = "jenks") +
          tm_facets(by = "release_year", ncol = 3)}
      else {
        output$country_message <- renderUI({
          link <- "https://vivianbeagle.github.io/STAT679/pie.html"
          message <- paste("See the link: ", tags$a(href = link, target = "_blank", "Click here"))
          return(HTML(message))})
      }
      
    }
    else if (input$question == "Dataset Overview") {
      if(input$data=="Correlation Matrix for Numerical Features"){movie1 <- movie |>
        drop_na() |>
        mutate(genre_numeric = as.numeric(factor(genre)))
      
      movie.numeric <- movie1[, c(4,6,7,12,13,15,16)]
      corr <- cor(movie.numeric)
      ggplot(melt(corr), aes(Var1, Var2)) +
        geom_tile(aes(fill = value)) +
        geom_text(aes(label = round(value, 2))) +
        scale_fill_gradient2() +
        labs(x = "Movie Features", y = "Movie Features", title = "Correlation Matric for Numericak Features") +
        theme_minimal()}
      else if (input$data=="Data Table Sorted by Score") {
        output$data_message <- renderUI({
          link <- "https://vivianbeagle.github.io/STAT679/data.html"
          message <- paste("Click the link: ", tags$a(href = link, target = "_blank", "Click here"))
          return(HTML(message))})
      }
      else{
        
        rpart.plot(model, type = 4, cex = 0.8, box.palette = "BlGnYl")
        output$pred <- renderDataTable({
          newdata = data.frame(matrix(NA, 1, (dim(pred.new)[2]-1)))
          colnames(newdata) <- colnames(pred.new)[-1]
          newdata$runtime <- log1p(input$runtime)
          
          if (input$genre9 %in% genre.cnt){
            for (j in 1:length(genre.cnt)){
              if (input$genre9 == genre.cnt[j]){
                newdata$genre = length(genre.cnt)+1-j
                break
              } 
            }
          }else {
            newdata$genre = 0
          }
          
          if (input$month %in% month.cnt){
            for (j in 1:length(month.cnt)){
              if (input$month == month.cnt[j]){
                newdata$release_month = length(month.cnt)+1-j
                break
              } 
            }
          }else {
            newdata$release_month = 0
          }
          
          if (input$country %in% country.cnt){
            for (j in 1:length(country.cnt)){
              if (input$country == country.cnt[j]){
                newdata$release_country = length(country.cnt)+1-j
                break
              } 
            }
          }else {
            newdata$release_country = 0
          }
          
          if (input$year < q) {
            newdata$release_year <- 0
          }else {
            newdata$release_year = input$year - q
          }
          if (newdata$release_year > 5) {newdata$release_year = 5}
          
          predictions <- predict(model, newdata)
          newdata$`Predicted Budget(k)` <- round(exp(-1+predictions)/1000, 5)
          colnames(newdata)[4] <- "release_region"
          
          newdata})
        
      }
    }
    
    else if(input$question ==  'Budget Analysis'){
      if(input$budget=="Budget vs Gross"){
        data <- subset(movie, genre == input$genre3 & year >= input$year_range[1] & year <= input$year_range[2])
        data$rating_group <- ifelse(data$rating %in% c("R","PG", "PG-13"), data$rating, "Other")
        ggplot(data, aes(budget, gross, color = rating_group)) +
          geom_point() +
          geom_smooth(method = "lm", se = TRUE,aes(group = 1)) +
          labs(title = "Gross vs Budget") +
          theme_minimal()
      }
      else{
        output$budget_message <- renderUI({
          link <- "https://siyanwang123.github.io/stat679_budget_score/imdb-table.html"
          message <- paste("Click the link: ", tags$a(href = link, target = "_blank", "Click here"))
          return(HTML(message))})
      }
    }
    
    else if(input$question ==  'Release Time Analysis'){
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
    
    else if(input$question ==  'Directors and Actors Analysis'){
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

