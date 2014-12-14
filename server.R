shinyServer(function(input, output) {
  
  # You can access the value of the widget with input$num, e.g.
  output$value <- renderPrint({ 
    # input$num 
    # AGE MINS_SINCE_INJURY GCS_EYE GCS_MOTOR GCS_VERBAL PUPIL_REACT_LEFT PUPIL_REACT_RIGHT
    nd <- data.frame(AGE=c(input$age),
                     MINS_SINCE_INJURY=c(input$mins),
                     GCS_EYE=c(input$eye),
                     GCS_MOTOR=c(input$motor),
                     GCS_VERBAL=c(input$verbal),
                     PUPIL_REACT_LEFT=c(input$left),
                     PUPIL_REACT_RIGHT=c(input$right))
    predict(r.glm2,newdata=nd,type="response")[[1]]
    })
  
    output$table <- renderDataTable({
      nd <- data.frame(AGE=c(input$age),
                       MINS_SINCE_INJURY=c(input$mins),
                       GCS_EYE=c(input$eye),
                       GCS_MOTOR=c(input$motor),
                       GCS_VERBAL=c(input$verbal),
                       PUPIL_REACT_LEFT=c(input$left),
                       PUPIL_REACT_RIGHT=c(input$right))
      knn(nd)
    })
  
})