library(shiny)

ui <- fluidPage(
  tags$head(
    tags$script("
      $(document).on('keydown', function(e) {
        if(e.key === 'w') {
          $('#up').click(); // Simulate click on the 'Move Up' button
        } else if(e.key === 's') {
          $('#down').click(); // Simulate click on the 'Move Down' button
        }
      });
    ")
  ),
  titlePanel("Cycling Dropdown Navigation"),
  fluidRow(
    column(4,
           selectInput("filter", "Choose Option:",
                       choices = c("Option 1", "Option 2", "Option 3", "Option 4"),
                       selected = "Option 1"),
           actionButton("up", "Move Up"),
           actionButton("down", "Move Down"),
           textOutput("selectedOption")
    )
  )
)

server <- function(input, output, session) {
  # Define choices outside of observeEvent to avoid scoping issues
  choices <- c("Option 1", "Option 2", "Option 3", "Option 4")

  observeEvent(input$up, {
    current_index <- match(input$filter, choices)
    # Calculate new index for cycling up
    if (current_index > 1) {
      updated_choice <- choices[current_index - 1]
    } else {
      updated_choice <- choices[length(choices)] # Wrap to the last option
    }
    updateSelectInput(session, "filter", selected = updated_choice)
  })

  observeEvent(input$down, {
    current_index <- match(input$filter, choices)
    # Calculate new index for cycling down
    if (current_index < length(choices)) {
      updated_choice <- choices[current_index + 1]
    } else {
      updated_choice <- choices[1] # Wrap to the first option
    }
    updateSelectInput(session, "filter", selected = updated_choice)
  })

  output$selectedOption <- renderText({
    paste("You have selected:", input$filter)
  })
}

shinyApp(ui, server)
