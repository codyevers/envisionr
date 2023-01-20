# ..........................
# SHINY UI for EnvisionR
# ...........................


# Define UI for application that plots random distributions 
envisionr_ui <- pageWithSidebar(
  
  # Application title
  headerPanel("EnvisionR"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    
    selectInput("scn.run","Run", choices=gsub('.Rdata','',scn.run.df$file)),
    
    textInput(inputId = "filter", label = "Field ", value = "vegclass"),
    
    helpText("Enter field name (flamelen, n_du, vegclass, disturb, actor, wui, fireid, lulc_a, policy,
             defense, manage, prevtrt, zone, in_ugb) or boolean statement
             (vegclass %in% 200:210 & manage == 10)"),
    
    textInput(inputId = "extent", label = "Zoom extent ", value = 'null'),
    
    helpText("Zoom map by dragging extent then double-clicking within the box.
             Double-click map w/o dragging to reset Manually enter extent above. Type 'null' to reset."),
    
    helpText("Year"),
    
    sliderInput(inputId = "yr",
                label = NULL, 1, 50, 1, step = 1, ticks=TRUE, 
                animate=animationOptions(interval=1000, loop=T)),
    
    # textInput(inputId = "palette", label = "Custom color palette:", value = 'null'),
    
    checkboxGroupInput("plot_options", "Customize plot", inline = TRUE,
                       c('Axis','Legend','Box','UGB','Custom palette','Transparent white'), selected = c('Legend','Box','UGB')),
    
    downloadButton('downloadData', 'CSV'),
    
    downloadButton('downloadPlot', 'PNG'),
    
    actionButton("goButton", "Animate")
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("distPlot", height="420px", width="560px", 
               click = "plot_click", dblclick = "plot_dblclick", 
               brush = brushOpts(
                 id = "plot_brush",
                 resetOnNew = TRUE
               )),
    verbatimTextOutput("summary"),
    verbatimTextOutput("nText")
  ))
