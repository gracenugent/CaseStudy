library(shiny)
library(vroom)
library(tidyverse)
library(forcats)

dir.create("neiss", showWarnings = FALSE)

download <- function(name) {
  url <- "https://raw.github.com/hadley/mastering-shiny/main/neiss/"
  download.file(paste0(url, name), paste0("neiss/", name), quiet = TRUE)
}

download("injuries.tsv.gz")
download("population.tsv")
download("products.tsv")

injuries <- vroom("neiss/injuries.tsv.gz")
products <- vroom("neiss/products.tsv")
population <- vroom("neiss/population.tsv")

prod_codes <- setNames(products$prod_code, products$title)

count_top <- function(df, var, n = 5) {
  df %>%
    
    # fct_infreq() orders categories from most common to least common
    # fct_lump() then combines all categories after the top n into "Other"
    # Keeping this order makes the tables easier to read and keeps the largest groups first
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
    
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}

# UI
ui <- fluidPage(
  
  fluidRow(
    column(
      8,
      selectInput(
        "code",
        "Product",
        choices = prod_codes,
        width = "100%"
      )
    ),
    
    column(
      2,
      selectInput(
        "y",
        "Y axis",
        choices = c("rate", "count")
      )
    ),
    
    # Added numericInput so the user can control how many rows appear in the summary tables
    column(
      2,
      numericInput(
        "n_rows",
        "Rows to show",
        value = 5,
        min = 1,
        max = 20
      )
    )
  ),
  
  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  
  fluidRow(
    column(12, plotOutput("age_sex"))
  ),
  
  fluidRow(
    column(2, actionButton("prev_story", "Previous")),
    column(2, actionButton("next_story", "Next")),
    column(8, textOutput("narrative"))
  )
)

# Server
server <- function(input, output, session) {
  
  selected <- reactive({
    injuries %>%
      filter(prod_code == input$code)
  })
  
  # Added input$n_rows so the number of displayed rows updates for diagnosis
  output$diag <- renderTable(
    count_top(selected(), diag, n = input$n_rows),
    width = "100%"
  )
  
  # Added input$n_rows so the number of displayed rows updates for body part
  output$body_part <- renderTable(
    count_top(selected(), body_part, n = input$n_rows),
    width = "100%"
  )
  
  # Added input$n_rows so the number of displayed rows updates for location
  output$location <- renderTable(
    count_top(selected(), location, n = input$n_rows),
    width = "100%"
  )
  
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
  output$age_sex <- renderPlot({
    
    if (input$y == "count") {
      
      summary() %>%
        ggplot(aes(age, n, colour = sex)) +
        geom_line() +
        labs(y = "Estimated number of injuries")
      
    } else {
      
      summary() %>%
        ggplot(aes(age, rate, colour = sex)) +
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people")
    }
    
  }, res = 96)
  
  story_index <- reactiveVal(1)
  
  observeEvent(input$code, {
    story_index(1)
  })
  
  # Moves forward through the narratives when the "Next" button is clicked
  # When the user reaches the last narrative, it loops back to the first
  observeEvent(input$next_story, {
    
    narratives <- selected() %>% pull(narrative)
    n <- length(narratives)
    
    if (story_index() == n) {
      story_index(1)
    } else {
      story_index(story_index() + 1)
    }
  })
  
  # Moves backward through the narratives when the "Previous" button is clicked
  # When the user goes backward from the first narrative, it loops to the last
  observeEvent(input$prev_story, {
    
    narratives <- selected() %>% pull(narrative)
    n <- length(narratives)
    
    if (story_index() == 1) {
      story_index(n)
    } else {
      story_index(story_index() - 1)
    }
  })
  
  output$narrative <- renderText({
    
    narratives <- selected() %>% pull(narrative)
    
    narratives[story_index()]
  })
}

# Run app
shinyApp(ui, server)
