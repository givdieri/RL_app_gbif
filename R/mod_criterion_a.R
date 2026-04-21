mod_criterion_a_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::verbatimTextOutput(ns('summary')),
    DT::DTOutput(ns('detail'))
  )
}

mod_criterion_a_server <- function(id, analysis_reactive, species_reactive, settings_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    result <- shiny::reactive({
      req(analysis_reactive(), species_reactive(), settings_reactive())
      compute_criterion_a(analysis_reactive(), species_reactive(), settings_reactive())
    })

    output$summary <- shiny::renderPrint({
      res <- result()
      cat(sprintf('Provisional category: %s\nReason: %s\nNote: %s\n', res$provisional_category, res$reason, res$note))
    })

    output$detail <- DT::renderDT({
      as.data.frame(result()) |> DT::datatable(options = list(dom = 't'))
    })

    result
  })
}
