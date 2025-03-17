
#' Shiny UI function
#'
#' @param request

ui <- function(request) {
  fluidPage(
    shinyjs::useShinyjs(),

    titlePanel(
      div(icon("chart-line"), "EnvisionR")
    ),

    tags$head(

      # Link external CSS file
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),

      # Note the wrapping of the string in HTML()
      tags$style(
        HTML("
          #fields_aux .btn {
            padding: 2px;
            text-align: center;
            font-size:10px;
            margin:2px;
          }
        ")
      ),

      # Define hot keys
      tags$script("
        $(document).on('keydown', function(e) {
          if (e.shiftKey) { // Check if Shift is held down
            if(e.key === 'Q' || e.key === 'q') {
              $('#left').click();
            } else if (e.key === 'E' || e.key === 'e') {
              $('#right').click();
            } else if (e.key === 'A' || e.key === 'a') {
              $('#back').click();
            } else if (e.key === 'D' || e.key === 'd') {
              $('#forward').click();
            } else if (e.key === 'W' || e.key === 'w') {
              $('#up').click();
            } else if (e.key === 'S' || e.key === 's') {
              $('#down').click();
            }
          }
        });

        // Trigger Shiny event when hyperlink (Filter fields) is clicked
        $(document).on('click', '#show', function() {
          Shiny.setInputValue('show', Math.random());  // Ensures event triggers every time
        });
      ")
    ),
    fluidPage(
      tabsetPanel(
        tabPanel("View",
          fluidPage(
            uiOutput("run_label", style = 'text-align: center;'),
            style = "border-style: 0px; margin-top:10px;",
            fluidRow(
              style = "margin-top: 5px",
              column(
                width = 4,
              ),
              column(
                width = 4,
              ),
              column(
                width = 4,
              )
            ),
            shinyjs::hidden(
              fluidRow(
                style = "",
                column(
                  width = 4,
                  div(style = "display: flex; justify-content: center;",
                      actionButton(
                        inputId = "left",
                        label = "-1 run [Q]",
                        style = "margin-bottom: 0px; margin-right: 5px"),
                      actionButton(
                        inputId = "right",
                        label = "+1 run [E]",
                        style = "margin-bottom: 0px; margin-right: 5px")
                  )
                ),
                column(
                  width = 4,
                  div(style = "display: flex; justify-content: center;",
                      actionButton(
                        inputId = "up",
                        label = "-1 field [W]",
                        style = "margin-bottom: 0px; margin-right: 5px"),
                      actionButton(
                        inputId = "down",
                        label = "+1 field [S]",
                        style = "margin-bottom: 0px; margin-right: 5px")
                  )
                ),
                column(
                  width = 4,
                  div(style = "display: flex; justify-content: center;",
                      actionButton(
                        inputId = "back",
                        label = "-1 year [A]",
                        style = "margin-bottom: 0px; margin-right: 5px"
                      ),
                      actionButton(
                        inputId = "forward",
                        label = "+1 year [D]",
                        style = "margin-bottom: 0px; margin-right: 5px"
                      )
                  )
                )
              )
            ),
            fluidRow( # SECOND ROW --------------------
              style = "margin-top: 5px",
              column( # LEFT PANNEL ---------------------
                width = 3,
                selectInput(
                  inputId = "select_field",
                  label = HTML(
                    'What to display? (<a id="show" href="#"
                    style="text-decoration: underline; color: blue;">filter list</a>)'),
                  choices = c('LULC_A', 'LULC_B')
                ),
                # actionButton("show", "Select fields to display"),
                sliderInput(
                  inputId = "year",
                  label = "What year?",
                  min = 2020,
                  max = 2059,
                  value = 2020,
                  step = 1,
                  ticks = FALSE,
                  sep = ""
                ),
                pickerInput(
                  inputId = "fields_aux",
                  label = "Secondary fields",
                  choices = LETTERS,
                  multiple = TRUE,
                  options = pickerOptions(container = "body", actionsBox = TRUE, liveSearch = TRUE),
                  width = "100%"
                ),
                radioButtons(
                  inputId = "select_run",
                  label = "Which run?",
                  choices = c('A','B','C','D'),
                  selected = 'A',
                  inline = F),
                shinyjs::hidden(
                  checkboxInput(
                    inputId = "toggle_query",
                    label = "Show query tool (WIP)"),
                  conditionalPanel(
                    condition = "input.toggle_query",
                    style="padding:5px",
                    textAreaInput(
                      inputId = "queryA",
                      label = "Query A",
                      value = "",
                      rows = 1,
                      cols = 50),
                    textAreaInput(
                      inputId = "queryB",
                      label = "Query B",
                      value = "",
                      rows = 1,
                      cols = 50),
                    checkboxInput(
                      inputId = "count_query",
                      label = "Count query")
                  )
                )
              ),
              column( # MIDDLE PANEL ----------------------
                width = 6,
                fluidRow(
                  div(
                    plotOutput(
                      outputId = "map",
                      height = "500px",
                      width = "600px",
                      click = "plot_click",
                      dblclick = "plot_dblclick",
                      brush = brushOpts(id = "plot_brush", resetOnNew = TRUE)
                    ),
                    div(style = 'display:flex; justify-content: center;',
                        actionButton(
                          inputId = 'zoom_button',
                          label = 'Zoom',
                          style = "margin-right:10px; float:right"),
                        actionButton(
                          inputId = 'reset_button',
                          label = 'Reset',
                          style = "margin-right:10px; float:right"),
                        downloadButton(
                          'downloadPlot',
                          label = 'Save image',
                          style = "margin-right:10px; float:right"),
                        downloadButton(
                          'downloadData_fixedTime',
                          label =  'Save csv',
                          style = "margin-right:10px; float:right")
                    ),
                    verbatimTextOutput('selected_point_info')
                  ),
                ),
                # fluidRow(
                #   column(11,
                #          helpText("B. Time slice"),
                #          tableOutput("time_summary"),
                #          verbatimTextOutput('time_tables')
                #   )
                # )
              ),
              column( # RIGHT PANEL ----------------------
                width = 3,
                checkboxInput(
                  inputId = "toggle_options",
                  label = "Show plot options"),
                conditionalPanel(
                  condition = "input.toggle_options",
                  checkboxGroupInput(
                    inputId = "plot_options",
                    label = "Plot elements",
                    inline = FALSE,
                    choices = c(
                      'Roads',
                      'Zones',
                      'Legend',
                      'Places',
                      'Boundary',
                      'Axes'
                    ),
                    selected = c('Roads','Boundary','Places')
                  )
                ),
                checkboxInput(
                  inputId = "toggle_extent",
                  label = "Show extent box"),
                conditionalPanel(
                  condition = "input.toggle_extent",
                  textInput(
                    inputId = "extent",
                    label = "Extent (text)",
                    value = NULL,
                  )
                ),
                checkboxInput(
                  inputId = "toggle_legend",
                  label = "Show legend"),
                conditionalPanel(
                  condition = 'input.toggle_legend',
                  strong("Legend"),
                  plotOutput(
                    outputId = "legend",
                    height = "500px",
                    width = "200px"
                )
                )
              )
            )
          )
        ), # END RUN PANE
        tabPanel("Load", # LOAD RUNS TAB -------------
            fluidPage(

             column(width = 6,
               # Directory selection
               shinyFiles::shinyDirButton("dir", "Select Directory", "Please select a directory"),
               verbatimTextOutput("dirPath"),

               # File dropdowns
               selectInput("file1", "Run A", choices = NULL, width = '100%'),
               selectInput("file2", "Run B", choices = NULL, width = '100%'),
               selectInput("file3", "Run C", choices = NULL, width = '100%'),
               selectInput("file4", "Run D", choices = NULL, width = '100%'),

               # Load Runs button
               actionButton("load_runs", "Load Runs"),
             )
           ),

           column(width = 6,
                verbatimTextOutput("load_status") # Display load status
           )

        ),
        # tabPanel("Fields", # SELECT FIELDS TAB -------------
        #   fluidPage(
        #     column(width = 6,
        #      shinyWidgets::checkboxGroupButtons(
        #        inputId = "fields_aux",
        #        label = 'Available fields',
        #        choices = c('A','B','C'),
        #        individual = TRUE
        #       )
        #     )
        #   )
        # )
    )
  )
  )
}
