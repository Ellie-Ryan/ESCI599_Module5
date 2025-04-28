
library(shiny)
library(vroom) # Fast file reading 
library(tidyverse)
library(rsconnect)

injuries <- vroom::vroom("neiss/injuries.tsv.gz")
products <- vroom::vroom("neiss/products.tsv")
population <- vroom::vroom("neiss/population.tsv")

ui <- fluidPage(
  fluidRow(
    # one row for product codes 
    column(width = 6, # range from 0-12 for column width
           selectInput("code", "Product", 
                       choices = setNames(products$prod_code, products$title),
                       width = "100%")
    ),
    # add selection option to format plot using rate or count 
    column(width = 2, selectInput("y", "Y axis", c("rate", "count")))
    ),

  # one row for 3 tables, with 4 columns that are 1/3 of 12 column width
  fluidRow(
    column(width = 4, tableOutput("diag")),
    column(width = 4, tableOutput("body_part")),
    column(width = 4, tableOutput("location"))
  ),
  
  # one row for the number of injuries plot
  fluidRow(
    column(width = 12, plotOutput("age_sex"))
  ),
  
  # Add narrative "tell me a story" button
  fluidRow(
    column(width = 2, actionButton("story", "Tell me a story")),
    column(width = 10, textOutput("narrative"))
  )
)


server <- function(input, output, session) {
  # convert selected variable to reactive expression
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  
  # write function to truncate tables to show only top 5 for each variable
  count_top <- function(df, var, n = 5) {
    df %>%
      mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
      group_by({{ var }}) %>%
      summarise(n = as.integer(sum(weight)))
  }
  
  # table for diagnosis
  output$diag <- renderTable(count_top(selected(), diag), 
                             width = "100%") # force table to take up max width
  # table for injured body part
  output$body_part <- renderTable(count_top(selected(), body_part), width = "100%")
  # table for location of injury
  output$location <- renderTable(count_top(selected(), location), width = "100%")

  
  # Format data for plot by joining population data and calculating rate
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>% # count injuries for product code by age and sex
      left_join(population, by = c("age", "sex")) %>% # pair population with age/sex
      mutate(rate = n / population * 1e4) # calculate injury rate by age/sex
  })
  # Plot of estimated number of injuries (rate OR count)
  output$age_sex <- renderPlot({
    if (input$y == "count") { # condition on input$y to plot rate or count
      summary() %>%
        ggplot(aes(age, n, colour = sex)) +
        geom_line() +
        labs(y = "Estimated number of injuries") # count
    } else {
      summary() %>%
        ggplot(aes(age, rate, colour = sex)) +
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people") # rate
    }
  }, res = 96)
  
  # Add narrative button to trigger new story 
  # Create reactive object that updates when input changes or button is pressed
  narrative_sample <- eventReactive( 
    list(input$story, selected()),
    selected() %>% 
      # pull single, randomly selected narrative from selected input
      pull(narrative) %>% 
      sample(1)
  )
  # Display narrative output as text
  output$narrative <- renderText(narrative_sample())
}
shinyApp(ui = ui, server = server)

