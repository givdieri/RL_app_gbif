mod_maps_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::plotOutput(ns('map_plot'), height = '450px'),
    DT::DTOutput(ns('map_tbl'))
  )
}

mod_maps_server <- function(id, analysis_reactive, species_reactive, settings_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    map_df <- shiny::reactive({
      req(analysis_reactive(), species_reactive(), settings_reactive())
      split_year <- as.integer(settings_reactive()$split_year %||% '2000')

      analysis_reactive() |>
        dplyr::filter(species_working == species_reactive()) |>
        dplyr::mutate(period = dplyr::if_else(year < split_year, 'historical', 'current')) |>
        dplyr::group_by(grid_id) |>
        dplyr::summarise(
          has_historical = any(period == 'historical'),
          has_current = any(period == 'current'),
          decimalLongitude = mean(decimalLongitude, na.rm = TRUE),
          decimalLatitude = mean(decimalLatitude, na.rm = TRUE),
          .groups = 'drop'
        ) |>
        dplyr::mutate(
          occurrence_period = dplyr::case_when(
            has_historical & has_current ~ 'both_periods',
            has_historical ~ 'historical_only',
            has_current ~ 'current_only',
            TRUE ~ 'unknown'
          )
        )
    })

    output$map_plot <- shiny::renderPlot({
      req(map_df())
      ggplot2::ggplot(
        map_df(),
        ggplot2::aes(x = decimalLongitude, y = decimalLatitude, color = occurrence_period)
      ) +
        ggplot2::geom_point(size = 3, alpha = 0.9) +
        ggplot2::scale_color_manual(
          values = c(
            historical_only = '#3B82F6',
            current_only = '#EF4444',
            both_periods = '#8B5CF6',
            unknown = 'grey50'
          ),
          name = 'Occurrence period'
        ) +
        ggplot2::labs(
          x = 'Longitude',
          y = 'Latitude',
          title = paste('Occurrence timing map:', species_reactive())
        ) +
        ggplot2::theme_minimal()
    })

    output$map_tbl <- DT::renderDT({
      req(map_df())
      map_df() |>
        dplyr::select(grid_id, decimalLongitude, decimalLatitude, occurrence_period) |>
        DT::datatable(options = list(pageLength = 10))
    })
  })
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
