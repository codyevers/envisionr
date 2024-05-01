# ..........................
# UI
# ...........................

#' Title
#'
#' @param datacubes
#'
#' @return
#' @export
#'
#' @import shiny
#' @import shinyWidgets

run_envisionr <- function(...) {

  datacubes <- list(...)
  idu_rast <- ref_data$idu_rast
  idu_vect <- ref_data$idu_vect
  roads <- ref_data$ref_roads
  places <- ref_data$ref_places
  zones <- ref_data$ref_zones
  boundary <- ref_data$ref_boundary

  if(is.null(datacubes)){
    stop("Datacube(s) required")
  }

  if(length(datacubes) > 4){
    stop("Maximum of 4 runs")
  }

  ui <- fluidPage(
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

  # ....................................
  # SERVER
  # ....................................

  server_wrapper <- function(...){
  # server <- function(input, output, clientData, session) {
    function(input, output, clientData, session) {

    # Render the dynamic title in the UI
    output$title <- renderUI({
      h3(paste0('EnvisionR - ', data()$run))
    })

    output$history_text <- renderUI({
      h3(paste0('EnvisionR - ', data()$run))
    })

    # data --------------

    # object to store limits from brush
    ranges <- reactiveValues(x = NULL, y = NULL)

    # reactive Value to save last selected point
    selectedPoint <- reactiveValues(IDU_INDEX = NULL, point = NULL)

    output$downloadData_fixedSpace_UI <- renderUI({
      req(selectedPoint$IDU_INDEX)
      downloadButton('downloadData_fixedSpace', 'History at selected point')
    })

    # reactive object to store loaded arrays
    dc_library <- reactiveValues(dataList = datacubes)

    # year index
    year_index <- reactive({
      input$year - 2018
    })

    extent <- reactive({
      if(input$extent == ''){
        txt = '438483.2, 540570.0, 1061211.4, 1149023.9'
        ext <- eval(parse(text = paste0('terra::ext(', txt, ')')))
      } else {
        ext <- eval(parse(text = paste0('terra::ext(', input$extent, ')')))
      }
      return(ext)
    })

    # current data cube based on input$select_run
    data <- reactive({

      sel <- which(c('A','B','C','D') == input$select_run)
      dc <- dc_library$dataList[[sel]]
      run_name <- attr(dc, 'run')

      y <- year_index()
      f <- input$filter
      q1 <- input$queryA
      q2 <- input$queryB
      e <- extent()

      # set year to 0 if count query selected
      if(input$count_query == TRUE) y <- NULL

      idu_rast <- update_raster(
        raster = idu_rast,
        datacube = dc,
        field = f,
        year = y,
        extent = e,
        queryA = q1,
        queryB = q2)

      pal_df <- get_pal('data/idu.xml', input$filter)

      levels <- terra::unique(idu_rast)

      # update UI inputs
      updateSelectInput(
        session = session,
        inputId = 'filter',
        choices = names(dc),
        selected = input$filter)

      shinyWidgets::updateCheckboxGroupButtons(
        session = session,
        inputId = 'filter_aux',
        choices = names(dc),
        selected = c(input$filter, input$filter_aux))

      return(list(
        run = run_name,
        datacube = dc,
        idu_rast = idu_rast,
        pal_df = pal_df,
        levels = levels))
    })

    # observers ------------

    # observe map for double clicks
    observe({
      req(input$plot_dblclick)
      x <- input$plot_dblclick$x
      y <- input$plot_dblclick$y
      point <- stars::st_extract(
        x = data()$idu_rast,
        at = matrix(c(x, y), ncol=2))
      selectedPoint$IDU_INDEX <- point$IDU_INDEX
      xy <- data.frame(x = x, y = y) |>
        st_as_sf(coords = c('x','y')) |>
        st_set_crs(st_crs(zone))
      selectedPoint$point <- xy
    })

    # observe for year back button
    observeEvent(input$back, {
      current_year <- input$year
      if (current_year > 2020) {
        updated_year <- current_year - 1
        updateSliderInput(session, "year", value = updated_year)
      }
    })

    # observe for year forward button
    observeEvent(input$forward, {
      current_year <- input$year
      if (current_year < 2059) {
        updated_year <- current_year + 1
        updateSliderInput(session, "year", value = updated_year)
      }
    })

    observeEvent(input$up, {
      choices = input$filter_aux
      current_index <- match(input$filter, choices)
      if (current_index > 1) {
        updated_choice <- choices[current_index - 1]
      } else {
        updated_choice <- choices[length(choices)] # Wrap to the last option
      }
      updateSelectInput(session, "filter", selected = updated_choice)
    })

    observeEvent(input$down, {
      choices = input$filter_aux
      current_index <- match(input$filter, choices)
      if (current_index < length(choices)) {
        updated_choice <- choices[current_index + 1]
      } else {
        updated_choice <- choices[1] # Wrap to the first option
      }
      updateSelectInput(session, "filter", selected = updated_choice)
    })

    # run select left
    observeEvent(input$left, {
      choices = c('A','B','C','D')
      current_index <- match(input$select_run, choices)
      if (current_index < length(choices)) {
        updated_choice <- choices[current_index + 1]
      } else {
        updated_choice <- choices[1]
      }
      updateSelectInput(session, "select_run", selected = updated_choice)
    })

    # run select right
    observeEvent(input$right, {
      choices = c('A','B','C','D')
      current_index <- match(input$select_run, choices)
      if (current_index < length(choices)) {
        updated_choice <- choices[current_index + 1]
      } else {
        updated_choice <- choices[1]
      }
      updateSelectInput(session, "select_run", selected = updated_choice)
    })


    # observe for zoom button press
    observeEvent(input$zoom_button, {
      brush <- input$plot_brush
      if (!is.null(brush)) {
        ranges$x <- c(brush$xmin, brush$xmax)
        ranges$y <- c(brush$ymin, brush$ymax)
        ranges$x[2] <- ranges$x[1] + diff(ranges$y) / 0.75
        updateTextAreaInput(session, inputId = 'extent', value=paste(
          paste(round(ranges$x,0), collapse=', '),
          paste(round(ranges$y,0), collapse=', '),
          sep=', '))
      }
    })

    # observe zoom reset button
    observeEvent(input$reset_button, {
      updateTextAreaInput(session, inputId = 'extent', value='')
    })

    # output ---------------------

    output$legend <- renderPlot({
      req(data()$levels)
      par(mar=rep(0,4))
      pal_df <- data()$pal_df |> filter(value %in% data()$levels$VALUE)
      plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
      legend('topleft', legend = pal_df$label, ncol = 1, fill = pal_df$hex, box.lwd = NA)
    })

    # draw map
    output$map <- renderPlot({

      req(input$filter)
      if(is.null(data()$datacube)) return(NULL)

      pal_field <- input$filter
      if(input$count_query) {
        pal_field <- NULL
      }

      if(input$queryA != '' | input$queryB != ''){
        pal_df <- data.frame(
          value = c(0,1,10,11),
          hex = c('#FFFFFF','#7AFF62','#847FFF','#FF0000'))
        pal_df <- data.frame(
          value = c(0,1,10,11),
          hex = c('#FFFFFF','purple','purple','black'))
      } else {
        pal_df <- NULL
      }

      plot_raster(
        raster = data()$idu_rast,
        pal_lookup = pal_field,
        pal_table = pal_df,
        extent = extent(),
        focus = selectedPoint$point,
        options = input$plot_options)
    })

    # attribute panel summary
    output$selected_point_info <- renderPrint({
      req(selectedPoint$IDU_INDEX)
      data_sel <- data()$datacube[
        input$filter_aux,selectedPoint$IDU_INDEX,year_index()] |>
        as.data.frame()
      labels <- input$filter_aux |> map_dfr(function(x){
        try({
          out <- lookup_val(data_sel[,x], x)
          if(length(out$label) == 0){
            data.frame(
              field = out$field,
              value = prettyNum(out$value, digits = 3),
              label = '',
              col = '')
          } else {
            data.frame(
              field = out$field,
              value = prettyNum(out$value, digits = 3),
              label = out$label,
              col = out$hex)
          }
        })
      })
      print(labels)
    })

    # time slice summary
    output$time_summary <- renderTable({

      req(selectedPoint$IDU_INDEX)

      indx = selectedPoint$IDU_INDEX
      fields <- unique(c(input$filter, input$filter_aux))

      out <- data.frame(data()$datacube[fields,indx+1,]) |>
        dplyr::select(-idu) |>
        dplyr::filter(year >= input$year - input$time_window) |>
        dplyr::filter(year <= input$year + input$time_window) |>
        dplyr::mutate(year = ifelse(
          year == input$year,
          paste0('**',year, '**'),
          as.character(year)))
    })

    output$downloadPlot <- downloadHandler(
      filename = function() {
        paste(input$filter,'_','yr',
              input$yr,'_',Sys.time(),
              input$scn.run,'.png', sep='') },
      content = function(file) {
        png(filename = file, width=640, height=480, units = 'px')
        if(is.null(data()$datacube)) return(NULL)

        pal_field <- input$filter
        if(input$count_query) {
          pal_field <- NULL
        }

        plot_raster(
          raster = data()$idu_rast,
          pal_lookup = pal_field,
          extent = input$extent,
          focus = selectedPoint$point,
          options = input$plot_options)
        dev.off()
      }
    )

    # observe for year forward button
    observeEvent(input$makeAnimation, {

      f <- input$filter
      q1 <- input$queryA
      q2 <- input$queryB

      library(animation)
      saveGIF({
        for(y in 2:40){
          idu_rast <- update_raster(
            raster = idu_rast,
            datacube = data()$datacube,
            extent = input$extent,
            field = f,
            year = y,
            queryA = q1,
            queryB = q2)

          plot_raster(
            raster = idu_rast,
            pal_lookup = input$filter,
            extent = input$extent,
            focus = selectedPoint$point,
            options = input$plot_options)

          title(main = paste('Year: ', y + 2021))
        }

      }, movie.name = paste0(input$filter, '.gif'), interval = .5)
    })

    output$downloadData_fixedSpace <- downloadHandler(
      filename = function() { 'idu_history_output.csv' },
      content = function(file) {
        idu_index <- selectedPoint$IDU_INDEX
        dataset <- data()$datacube[input$filter_aux,idu_index,] |> as.data.frame()
        write.csv(dataset, file, row.names=F)
      }
    )

    output$downloadData_fixedTime <- downloadHandler(
      filename = function() { 'attribute_map_output.csv' },
      content = function(file) {
        dataset <- data()$datacube[input$filter_aux,,year_index()] |> as.data.frame()
        write.csv(dataset, file, row.names=F)
      }
    )
  }
  }

  server <- server_wrapper(ref_data)
  shinyApp(ui, server)
}
