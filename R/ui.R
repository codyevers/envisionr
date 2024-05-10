
app_ui <- function(request) {
  fluidPage(
    titlePanel(uiOutput("title")),
    tags$head(
      tags$script("
        $(document).on('keydown', function(e) {
          if(e.key === 'a') {
            $('#back').click();
          } else if (e.key === 'd') {
            $('#forward').click();
          } else if (e.key === 'w') {
            $('#up').click();
          } else if (e.key === 's') {
            $('#down').click();
          } else if (e.key === 'q') {
            $('#left').click();
          } else if (e.key === 'e') {
            $('#right').click();
          }
        });
      ")
    ),
    fluidRow(
      column(
        width = 3,
        radioButtons(
          inputId = "select_run",
          label = "Select run",
          choices = c('A','B','C','D'),
          selected = 'A',
          inline = T),
        div(style = "display: flex; justify-content: center;",
            actionButton(
              inputId = "left",
              label = "-1 run",
              style = "margin-bottom: 10px; margin-right: 5px"),
            actionButton(
              inputId = "right",
              label = "+1 run",
              style = "margin-bottom: 10px; margin-right: 5px")
        ),
        selectInput(
          inputId = "filter",
          label = 'Mapping field',
          choices = c('LULC_A', 'LULC_B')
        ),
        div(style = "display: flex; justify-content: center;",
            actionButton(
              inputId = "up",
              label = "-1 field",
              style = "margin-bottom: 10px; margin-right: 5px"),
            actionButton(
              inputId = "down",
              label = "+1 field",
              style = "margin-bottom: 10px; margin-right: 5px")
        ),
        sliderInput(
          inputId = "year",
          label = NULL,
          min = 2020,
          max = 2059,
          value = 2020,
          step = 1,
          ticks = FALSE,
          sep = ""
        ),
        div(style = "display: flex; justify-content: center;",
            actionButton(
              inputId = "back",
              label = "-1 year",
              style = "margin-bottom: 10px; margin-right: 5px"),
            actionButton(
              inputId = "forward",
              label = "+1 year",
              style = "margin-bottom: 10px; margin-right: 5px")
        ),
        checkboxInput(
          inputId = "toggle_query",
          label = "Toggle query tool"),
        conditionalPanel(
          condition = "input.toggle_query",
          style="padding:5px; border:solid; border-radius:5px",
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
        ),
        checkboxInput(
          inputId = "toggle_options",
          label = "Toggle plot options"),
        conditionalPanel(
          condition = "input.toggle_options",
          sliderInput(
            inputId = "time_window",
            label = "Time window",
            min = 1,
            max = 25,
            value = 5,
            step = 1,
            ticks = FALSE,
            sep = ""
          ),
          checkboxGroupInput(
            inputId = "plot_options",
            label = "Customize plot",
            inline = TRUE,
            choices = c(
              'Roads',
              'Zones',
              'Legend',
              'Places',
              'Boundary',
              'Axes'
            ),
            selected = c('Roads','Boundary','Places')
          ),
          textInput(
            inputId = "extent",
            label = "Extent",
            value = NULL,
          )
        ),
        checkboxInput(
          inputId = "toggle_download",
          label = "Toggle download tools"),
        conditionalPanel(
          condition = "input.toggle_download",
          downloadButton('downloadPlot', 'Save map'),
          actionButton(inputId = "makeAnimation", label = "Make animation"),
          uiOutput("downloadData_fixedSpace_UI"),
          downloadButton('downloadData_fixedTime', 'Attributes at selected time')
        ),
        plotOutput(
          outputId = "legend",
          height = "500px",
          width = "200px"
        ),
      ),
      column(
        width = 7,
        fluidRow(
          div(
            plotOutput(
              outputId = "map",
              height = "420px",
              width = "560px",
              click = "plot_click",
              dblclick = "plot_dblclick",
              brush = brushOpts(id = "plot_brush", resetOnNew = TRUE)
            ),
            div(style = 'display:flex; justify-content: center;',
                actionButton(
                  inputId = 'zoom_button',
                  label = 'zoom',
                  style = "margin-right:10px; float:right"),
                actionButton(
                  inputId = 'reset_button',
                  label = 'reset',
                  style = "margin-right:10px; float:right")
            ),
            verbatimTextOutput('selected_point_info')
          ),
        ),
        fluidRow(
          column(11,
                 helpText("B. Time slice"),
                 tableOutput("time_summary"),
                 verbatimTextOutput('time_tables')
          )
        )
      ),
      column(
        width = 2,
        shinyWidgets::checkboxGroupButtons(
          inputId = "filter_aux",
          label = 'Available fields',
          choices = c('A','B','C'),
          individual = TRUE
        ),
      )
    )
  )
}
