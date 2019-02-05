
ui <- fixedPage(
  titlePanel("WormObserver analysis."),
  selectInput(inputId = "selected_dataset",
              label="Which dataset?",
              choices=unique(data$dataset_ID)),
  h4(textOutput("timepoint_number")),
  h4(textOutput("frame_number")),
  fluidRow(
    column(8,
           plotOutput(outputId = "pathPlot")
    )
  ),
  br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
  textOutput("click_info"),
  br(),
  fluidRow(
    column(6,
           helpText("Select the timepoint you want to have a look at (in minutes)."),
           br(),
           uiOutput("timepoint_slider"),
           br(),
           helpText("Select offset for angle calculation."),
           br(),
           numericInput(inputId = "selected_offset",
                        label = "Offset",
                        min = 1,
                        max = 50,
                        value = 25),
           br(),
           radioButtons("path_color", "Color the path by:",
                        c("Track ID" = "ID",
                          "Mean angle" = "mean_angle",
                          "SD angle" = "sd_local_angle",
                          "Local angle" = "angle",
                          "Mean velocity" = "velocity",
                          "SD velocity" = "sd_local_velocity",
                          "Local velocity" = "local_velocity",
                          "Track displacement" = "displacement",
                          "Track length (s)" = "length",
                          "Frame"= "frame"
                        ))
    ),
    column(6,
           helpText("Select the frame number."),
           br(),
           br(),
           sliderInput(inputId = "selected_frame",
                       label = "Frames",
                       min = 1,
                       max = max(data$frame),
                       value = 10,
                       ticks=FALSE),
           helpText("Chose the minimal duration for paths to be shown (frames)."),
           numericInput(inputId = "selected_duration",
                        label = "Duration",
                        min = 1,
                        max = max(data$frame),
                        value = 50),
           br(),
           numericInput(inputId = "max_number_gaps",
                        label="Max. gaps in track",
                        min=0,
                        max=10,
                        value=5)
    )
  )
)



server <- function(input, output,session) {
  
  #the timepoint slider is dynmaically render as it's maximum value is dependent on the chosen dataset
  output$timepoint_slider <- renderUI({
    data.sub <- data %>% filter(dataset_ID == input$selected_dataset) 
    sliderTextInput(inputId = "selected_minute",
                    label = "Timepoint (minutes)",
                    choices= sort(unique(data.sub$minutes))
    )
  })
  output$pathPlot <- renderPlot({
    selected_dataset <- input$selected_dataset
    max_number_gaps <- input$max_number_gaps
    path_color <- input$path_color
    selected_minute <- input$selected_minute
    selected_frame <-input$selected_frame
    selected_offset <- input$selected_offset
    selected_duration <- input$selected_duration
    plot_image_with_path(selected_offset,selected_minute,selected_dataset,max_number_gaps, selected_duration,selected_frame,path_color,"with_image")
  },height = 800, width = 800)
  
  #output$click_info <- renderPrint({
  # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
  # were a base graphics plot, we'd need those.
  #nearPoints(data_for_plotting,xvar="location_x",yvar="location_y", input$plot_click, threshold = 100, maxpoints = 1,addDist = TRUE)
  #})
  
  
  # output$timepoint_number <- renderText({
  #   data.sub <- data %>% filter(dataset_ID == input$selected_dataset)
  #   selected_minute <- input$selected_minute
  #   all_tps_in_minutes <- unique(data.sub$minutes)
  #   corresponding_minute <- all_tps_in_minutes[which(abs(all_tps_in_minutes-selected_minute)==min(abs(all_tps_in_minutes-selected_minute)))]
  #   paste("Minute shown:", corresponding_minute)
  #   
  # })
  
  output$frame_number <- renderText({
    selected_frame <- input$selected_frame
    paste("Frame number shown:", selected_frame)
    
  })
  
}

shinyApp(ui, server)

