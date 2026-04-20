mod_maps_ui <- function(id) {
  ns <- shiny::NS(id)
  DT::DTOutput(ns('map_tbl'))
}

mod_maps_server <- function(id, analysis_reactive, species_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    output$map_tbl <- DT::renderDT({
      req(analysis_reactive(), species_reactive())
      analysis_reactive() |>
        dplyr::filter(species_working == species_reactive()) |>
        dplyr::distinct(grid_id, decimalLongitude, decimalLatitude, year) |>
        DT::datatable(options = list(pageLength = 10))
    })
  })
}
