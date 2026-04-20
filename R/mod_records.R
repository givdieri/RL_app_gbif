mod_records_ui <- function(id) {
  ns <- shiny::NS(id)
  DT::DTOutput(ns('tbl'))
}

mod_records_server <- function(id, records_reactive, species_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    output$tbl <- DT::renderDT({
      req(records_reactive(), species_reactive())
      records_reactive() |>
        dplyr::filter(species_working == species_reactive()) |>
        dplyr::select(record_id, species_working, observation_date, year, decimalLongitude, decimalLatitude, source_dataset, quality_flag, exclusion_reason) |>
        DT::datatable(options = list(pageLength = 10))
    })
  })
}
