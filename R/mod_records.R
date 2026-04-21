mod_records_ui <- function(id) {
  ns <- shiny::NS(id)
  DT::DTOutput(ns('tbl'))
}

mod_records_server <- function(id, records_reactive, analysis_reactive, species_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    records_view <- shiny::reactive({
      req(records_reactive(), analysis_reactive(), species_reactive())

      ifbl_lookup <- analysis_reactive() |>
        dplyr::select(record_id, ifbl_grid_id, ifbl_uurhok, ifbl_kwartier, grid_source, grid_id) |>
        dplyr::distinct()

      records_reactive() |>
        dplyr::left_join(ifbl_lookup, by = 'record_id') |>
        dplyr::filter(species_working == species_reactive())
    })

    output$tbl <- DT::renderDT({
      req(records_view())
      records_view() |>
        dplyr::select(
          record_id,
          species_working,
          observation_date,
          year,
          decimalLongitude,
          decimalLatitude,
          ifbl_grid_id,
          ifbl_uurhok,
          ifbl_kwartier,
          grid_id,
          grid_source,
          source_dataset,
          quality_flag,
          exclusion_reason
        ) |>
        DT::datatable(options = list(pageLength = 10))
    })
  })
}
