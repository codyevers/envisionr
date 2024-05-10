server_wrapper <- function(ref_data, datacubes){

  function(input, output, clientData, session) {

    # extract spatial reference data
    idu_rast <- ref_data$idu_rast
    idu_vect <- ref_data$idu_vect
    roads <- ref_data$ref_roads
    places <- ref_data$ref_places
    zones <- ref_data$ref_zones
    boundary <- ref_data$ref_boundary

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

      levels <- terra::unique(idu_rast)

      pal_df <- get_pal(input$filter)

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
    # observe({
    #   req(input$plot_dblclick)
    #   x <- input$plot_dblclick$x
    #   y <- input$plot_dblclick$y
    #   point <- stars::st_extract(
    #     x = data()$idu_rast,
    #     at = matrix(c(x, y), ncol=2))
    #   selectedPoint$IDU_INDEX <- point$IDU_INDEX
    #   xy <- data.frame(x = x, y = y) |>
    #     st_as_sf(coords = c('x','y')) |>
    #     st_set_crs(st_crs(zone))
    #   selectedPoint$point <- xy
    # })

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
      if (current_index > 1) {
        updated_choice <- choices[current_index - 1]
      } else {
        updated_choice <- choices[length(choices)]
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
