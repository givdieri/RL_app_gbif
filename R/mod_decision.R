mod_decision_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::textAreaInput(ns('remarks'), 'Expert remarks', rows = 6, placeholder = 'Add expert review notes here.'),
    shiny::verbatimTextOutput(ns('decision'))
  )
}

mod_decision_server <- function(id, criterion_result_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    output$decision <- shiny::renderPrint({
      req(criterion_result_reactive())
      res <- criterion_result_reactive()
      cat(sprintf('Provisional: %s\nReason: %s\nExpert remarks: %s\n',
                  res$provisional_category,
                  res$reason,
                  ifelse(nzchar(input$remarks), input$remarks, '[none]')))
    })
  })
}
