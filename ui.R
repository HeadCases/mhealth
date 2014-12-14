shinyUI(fluidPage(
  
  # Copy the line below to make a number input box into the UI.
  # AGE MINS_SINCE_INJURY GCS_EYE GCS_MOTOR GCS_VERBAL PUPIL_REACT_LEFT PUPIL_REACT_RIGHT
  numericInput("age", label = h5("AGE"), value = 18),
  sliderInput("mins", label = h5("MINS_SINCE_INJURY"), min = 0, max = 1000, value = 60),
  sliderInput("eye", label = h5("GCS_EYE"), min = 1, max = 4, value = 1),
  sliderInput("motor", label = h5("GCS_MOTOR"), min = 1, max = 6, value = 1),
  sliderInput("verbal", label = h5("GCS_VERBAL"), min = 1, max = 5, value = 1),
  sliderInput("left", label = h5("PUPIL_REACT_LEFT"), min = 1, max = 3, value = 1),
  sliderInput("right", label = h5("PUPIL_REACT_RIGHT"), min = 1, max = 3, value = 1),
  actionButton("action", label = "Queue Patient"),
  
  hr(),
  fluidRow(column(3, verbatimTextOutput("value"))),
  fluidRow(dataTableOutput(outputId="table"))  
))
