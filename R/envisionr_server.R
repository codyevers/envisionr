# ....................................
# Shiny SERVER function for EnvisionR
# ....................................

envisionr_server <- function(input, output, clientData, session) {
  
  # Return the requested dataset
  datasetInput <- reactive({
    cat('input change:',input$filter,'\n')
    cat(input$scn.run)
    scn.to.load <- getDeltaBundleToLoad(paste0(input$scn.run,'.Rdata'))
    # load new scenario if selected is different from loaded
    if(scn.loaded != scn.to.load){
      load(scn.to.load)
      scn.loaded <<- scn.to.load
      db.base <<- db
    }
    da <<- db.base$state.array
    if(is.null(da)){ da <<- db.base$state_array }
    slice <<- pullSlice(state.array=da, year=input$yr, filter=input$filter)    
    return(slice)    
  })
  
  # export animation
  ntext <- eventReactive(input$goButton, {
    input$n
    saveGIF({
      ani.options(nmax = 30)
      for(i in c(1:50)){
        slice <<- pullSlice(state.array=da, year=i, filter=input$filter)    
        mapSlice(data.slice=slice, filter=input$filter, use.color.scheme=TRUE, extent=c(ranges$x,ranges$y), plot.options = input$plot_option)
      }
    }, interval = 0.1, movie.name = "bm_demo.gif", ani.width = 360, ani.height = 480)
  })
  
  output$nText <- renderText({
    ntext()
  })
  
  # object to store limits from brush
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  # draw map
  output$distPlot <- renderPlot({
    dataset <- datasetInput()
    cat('updating map\n',input$filter,'\n',scn.loaded,'\n')
    if(is.null(input$palette)) pal = NULL else pal = input$palette
    if(input$extent == '' | input$extent == 'null'){
      mapSlice(data.slice=slice, filter=input$filter, use.color.scheme=TRUE, extent=NULL, custom.pal = pal, plot.options = input$plot_options)
    } else {
      mapSlice(data.slice=slice, filter=input$filter, use.color.scheme=TRUE, extent=input$extent, custom.pal = pal, plot.options = input$plot_options)
    }
  })
  # , height=360, width=480)
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$plot_dblclick, {
    brush <- input$plot_brush
    print(paste(brush$xmin, brush$xmax, brush$ymin, brush$ymax))
    # browser()
    if (!is.null(brush)) { 
      # double click zooms to brushed area
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      ranges$x[2] <- ranges$x[1] + diff(ranges$y) / 0.75
      updateTextAreaInput(session, inputId = 'extent', value=paste(
                                                                   paste(round(ranges$x,0), collapse=', '), 
                                                                   paste(round(ranges$y,0), collapse=', '), 
                                                                   sep=', '))
      } else { 
      # double click resets zoom
      ranges$x <- NULL
      ranges$y <- NULL
      updateTextAreaInput(session, inputId = 'extent', value='null')
    }
  })
  
  output$summary <- renderText({
      if(!is.null(input$plot_click)){
        cellNum <- cellFromXY(idu.grid.ref, xy = c(input$plot_click$x, input$plot_click$y))
        if(!is.na(cellNum)){
          iduNum <- idu.grid.ref[cellNum]
          iduVal <- da[iduNum,input$filter,input$yr]
          valLabel <- pal.df$label[which(iduVal==pal.df$value)]
          print(paste('SELECTED IDU:', iduNum, '\n', input$filter, iduVal, '\n',valLabel))
        }
      } else {
        # print('')
      }

  })

  output$downloadData <- downloadHandler(
    filename = function() { 'output.csv' },
    content = function(file) {
      dataset <- datasetInput()
      dataset <- data.frame(idu=c(1:length(dataset)), value=dataset)
      write.csv(dataset, file, row.names=F)
    }
  )
  
  output$downloadPlot <- downloadHandler(
    filename = function() { paste(input$filter,'_','yr',input$yr,'_',Sys.time(),input$scn.run,'.png', sep='') },
    content = function(file) {
      png(filename = file, width=640, height=480, units = 'px')
      if(is.null(input$palette)) pal = NULL else pal = input$palette
      if(input$extent == '' | input$extent == 'null'){
        mapSlice(data.slice=slice, filter=input$filter, use.color.scheme=TRUE, extent=NULL, custom.pal = pal, plot.options = input$plot_options)
      } else {
        mapSlice(data.slice=slice, filter=input$filter, use.color.scheme=TRUE, extent=input$extent, custom.pal = pal, plot.options = input$plot_options)
      }
      dev.off()
    }
  )
}
