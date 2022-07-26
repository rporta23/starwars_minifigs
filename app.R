library(tidyverse)
library(shiny)
library(plotly)

data1 <- read_csv(here::here("plotly1.csv")) |>
  mutate(ratio = round(num_minifigs/minutes, 2))

data2 <- read_csv(here::here("plotly2.csv")) |>
  mutate(ratio = round(num_minifigs/minutes, 2))

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Screen Time Versus Number of Minifigs: Star Wars Main Films"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "gender",
        label = "Gender",
        choices = c("masculine", "feminine"),
        selected = c("masculine", "feminine")
      ),
      checkboxGroupInput(
        inputId = "is_human",
        label = "Character is Human?",
        choices = c("human", "non human"),
        selected = c("human", "non human")
      ),
      checkboxGroupInput(
        inputId = "species",
        label = "Species",
        choices = unique(c(data1$species)),
        selected = unique(data1$species)
      ),
      imageOutput("logo")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("combined"),
      plotlyOutput("by_era"),
      uiOutput("tab"),
      uiOutput("tab2")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  selected_data <- reactive({
    data1 |>
      filter(gender %in% input$gender, species %in% input$species, is_human %in% input$is_human)
  })

  selected_data2 <- reactive({
    data2 |>
      filter(gender %in% input$gender, species %in% input$species, is_human %in% input$is_human)
  })

  output$combined <- renderPlotly(
    if(nrow(selected_data() > 0)){
      p <- ggplotly(
        ggplot(selected_data(),
               aes(
                 x = minutes,
                 y = num_minifigs,
                 color = gender,
                 text = paste("Name:", name, "\n#Minifigs:", num_minifigs, "\nScreen time:", minutes, "\nSpecies:", species, "\n#Figs per minute of screentime:", ratio)
                 )
               ) +
          geom_point(alpha = 0.5) +
          # geom_smooth() + not working
          scale_color_manual(values = c(
            "feminine" = "#f43b93",
            "masculine" = "#8cc63f"
          )) +
          labs(
            title = "All Films Combined (Including Rogue One and Solo)",
            x = "Screentime (minutes)",
            y = "Number of Minifigs",
            color = "Gender"
          ),
        tooltip = "text"
      ) } else {
        p <- plotly_empty()
      }
  )

  output$by_era <- renderPlotly(
    if(nrow(selected_data2() > 0)){
      p <- ggplotly(
        ggplot(selected_data2(),
               aes(
                 x = minutes,
                 y = num_minifigs,
                 color = gender,
                 text = paste("Name:", name, "\n#Minifigs:", num_minifigs, "\nScreen time:", minutes, "\nSpecies:", species, "\n#Figs per minute of screentime:", ratio))) +
          geom_point(alpha = 0.5) +
          scale_color_manual(values = c(
            "feminine" = "#f43b93",
            "masculine" = "#8cc63f"
          )) +
          facet_grid(~era) +
          geom_jitter() +
          labs(
            title = "By Era (Ep. 1-9 Only)",
            x = "Screentime (minutes)",
            y = "Number of Minifigs",
            color = "Gender"
          ),
        tooltip = "text"
      )
    } else {
      p <- plotly_empty()
    }
  )

  url <- a("IMDB", href="https://www.imdb.com/list/ls027631145/")
  output$tab <- renderUI({
    tagList("Screen time data from", url)
  })

  url2 <- a("BrickLink", href="https://www.bricklink.com/v2/main.page")
  output$tab2 <- renderUI({
    tagList("Minifig data from", url2)
  })

  output$logo <- renderImage({
    list(src = "circle_logo.png",
         width = 150,
         height = 100)
  }, deleteFile = FALSE)
}

# Run the application
shinyApp(ui = ui, server = server)
