mod_maps_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::plotOutput(ns('map_plot'), height = '520px'),
    DT::DTOutput(ns('map_tbl'))
  )
}

mod_maps_server <- function(id, analysis_reactive, species_reactive, settings_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    discover_grid_file <- function() {
      env_path <- Sys.getenv('IFBL_GRID_PATH', unset = '')
      if (nzchar(env_path) && file.exists(env_path)) return(env_path)

      candidates <- c(
        file.path('spatial', 'ifbl_grid.shp'),
        file.path('spatial', 'ifbl_grid.kml'),
        file.path('data_aux', 'ifbl', 'ifbl_grid.shp')
      )
      existing <- candidates[file.exists(candidates)]
      if (length(existing) > 0) return(existing[[1]])

      dynamic <- c(
        list.files('spatial', pattern = '\\.shp$', full.names = TRUE),
        list.files('spatial', pattern = '\\.kml$', full.names = TRUE)
      )
      if (length(dynamic) > 0) return(dynamic[[1]])

      ''
    }

    map_df <- shiny::reactive({
      req(analysis_reactive(), species_reactive(), settings_reactive())
      split_year <- as.integer(settings_reactive()$split_year %||% '2000')

      analysis_reactive() |>
        dplyr::filter(species_working == species_reactive()) |>
        dplyr::mutate(period = dplyr::if_else(year < split_year, 'historical', 'current')) |>
        dplyr::group_by(grid_id, ifbl_grid_id, grid_source) |>
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

    ifbl_grid <- shiny::reactive({
      grid_path <- discover_grid_file()
      if (!nzchar(grid_path) || !file.exists(grid_path)) return(NULL)

      g <- tryCatch(sf::st_read(grid_path, quiet = TRUE), error = function(e) NULL)
      if (is.null(g) || nrow(g) == 0) return(NULL)

      id_candidates <- c('IFBL', 'ifbl_id', 'ifbl', 'CODE', 'code', 'GRID_ID', 'grid_id', 'Name', 'name')
      id_col <- id_candidates[id_candidates %in% names(g)][1]
      if (is.na(id_col) || !nzchar(id_col)) return(NULL)

      g |>
        dplyr::mutate(join_ifbl_id = as.character(.data[[id_col]]))
    })

    output$map_plot <- shiny::renderPlot({
      req(map_df())
      status_palette <- c(
        historical_only = '#3B82F6',
        current_only = '#EF4444',
        both_periods = '#8B5CF6',
        unknown = 'grey70'
      )

      g <- ifbl_grid()
      if (!is.null(g)) {
        joined <- g |>
          dplyr::left_join(map_df(), by = c('join_ifbl_id' = 'ifbl_grid_id'))

        flanders_bbox <- sf::st_bbox(c(xmin = 2.45, ymin = 50.65, xmax = 5.95, ymax = 51.55), crs = sf::st_crs(4326))
        flanders_sf <- sf::st_as_sfc(flanders_bbox)

        ggplot2::ggplot() +
          ggplot2::geom_sf(data = sf::st_transform(flanders_sf, sf::st_crs(g)), fill = '#F8FAFC', color = '#CBD5E1') +
          ggplot2::geom_sf(
            data = joined,
            ggplot2::aes(fill = occurrence_period),
            color = '#64748B',
            linewidth = 0.1,
            alpha = 0.75
          ) +
          ggplot2::scale_fill_manual(values = status_palette, drop = FALSE, na.value = 'white', name = 'Occurrence period') +
          ggplot2::labs(title = paste('IFBL occupancy map:', species_reactive())) +
          ggplot2::theme_minimal()
      } else {
        ggplot2::ggplot(
          map_df(),
          ggplot2::aes(x = decimalLongitude, y = decimalLatitude, color = occurrence_period)
        ) +
          ggplot2::borders('world', regions = 'Belgium', colour = '#CBD5E1', fill = '#F8FAFC') +
          ggplot2::geom_point(size = 3, alpha = 0.9) +
          ggplot2::coord_quickmap(xlim = c(2.45, 5.95), ylim = c(50.65, 51.55)) +
          ggplot2::scale_color_manual(values = status_palette, name = 'Occurrence period') +
          ggplot2::labs(
            x = 'Longitude',
            y = 'Latitude',
            title = paste('Occurrence timing map (point fallback):', species_reactive())
          ) +
          ggplot2::theme_minimal()
      }
    })

    output$map_tbl <- DT::renderDT({
      req(map_df())
      map_df() |>
        dplyr::select(grid_id, ifbl_grid_id, grid_source, decimalLongitude, decimalLatitude, occurrence_period) |>
        DT::datatable(options = list(pageLength = 10))
    })
  })
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
