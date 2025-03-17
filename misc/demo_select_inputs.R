# Load the necessary libraries
library(shiny)
library(shinyFiles)

# Define the UI
ui <- fluidPage(
  titlePanel("Shiny Application with File Selection"),

  tabPanel("Run",
           sidebarLayout(
             sidebarPanel(
               # Directory selection
               shinyDirButton("dir", "Select Directory", "Please select a directory"),
               verbatimTextOutput("dirPath"),

               # File dropdowns
               selectInput("file1", "Select File 1", choices = NULL),
               selectInput("file2", "Select File 2", choices = NULL),
               selectInput("file3", "Select File 3", choices = NULL),
               selectInput("file4", "Select File 4", choices = NULL),

               # Load Runs button
               actionButton("load_runs", "Load Runs")
             ),
             mainPanel(
               verbatimTextOutput("load_status") # Display load status
             )
        )
    )
  )

# Define the server
server <- function(input, output, session) {
  # Initialize shinyFiles
  shinyDirChoose(input, "dir", roots = c(home = "~/Github/envisionr"))

  # Reactive to store the selected directory path
  dirPath <- reactive({
    parseDirPath(roots = c(home = "~/Github/envisionr"), input$dir)
  })

  # Output the selected directory path
  output$dirPath <- renderPrint({
    dirPath()
  })

  # Reactive to get the list of files in the selected directory
  fileList <- reactive({
    path <- dirPath()
    if (is.null(path)) return(NULL)

    files <- list.files(path, full.names = FALSE)
    if (length(files) == 0) return(NULL)

    files
  })

  # Update the choices for the selectInput widgets in the Run tab
  observe({
    files <- fileList()
    if (is.null(files)) return(NULL)

    updateSelectInput(session, "file1", choices = files, selected = files[1])
    updateSelectInput(session, "file2", choices = files, selected = files[min(2, length(files))])
    updateSelectInput(session, "file3", choices = files, selected = files[min(3, length(files))])
    updateSelectInput(session, "file4", choices = files, selected = files[min(4, length(files))])
  })

  # Reactive values to store loaded data
  loadedData <- reactiveValues(A = NULL, B = NULL, C = NULL, D = NULL)

  # Load selected files into memory as A, B, C, D
  observeEvent(input$load_runs, {
    path <- dirPath()
    if (is.null(path)) {
      output$load_status <- renderText("No directory selected.")
      return()
    }

    selected_files <- c(input$file1, input$file2, input$file3, input$file4)
    selected_files <- selected_files[!is.na(selected_files) & selected_files != ""]

    if (length(selected_files) == 0) {
      output$load_status <- renderText("No files selected.")
      return()
    }

    file_paths <- file.path(path, selected_files)

    # Load files (assuming they are RData or CSV for demonstration)
    tryCatch({

      load_datacube <- function(x){
        load(x)
        return(dc)
      }

      if (length(file_paths) >= 1) loadedData$A <- load_datacube(file_paths[1])
      if (length(file_paths) >= 2) loadedData$B <- load_datacube(file_paths[2])
      if (length(file_paths) >= 3) loadedData$C <- load_datacube(file_paths[3])
      if (length(file_paths) >= 4) loadedData$D <- load_datacube(file_paths[4])

      output$load_status <- renderText("Files loaded successfully!")
      browser()
    }, error = function(e) {
      output$load_status <- renderText(paste("Error loading files:", e$message))
    })
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
