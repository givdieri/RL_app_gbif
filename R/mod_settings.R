mod_settings_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns('settings_ui'))
}

mod_settings_server <- function(id, defaults_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$settings_ui <- shiny::renderUI({
      purrr::map(seq_len(nrow(defaults_df)), function(i) {
        nm <- defaults_df$setting[i]
        val <- defaults_df$value[i]
        shiny::textInput(ns(nm), nm, value = val)
      })
    })

    shiny::reactive({
      vals <- purrr::map_chr(defaults_df$setting, ~ input[[.x]] %||% defaults_df$value[defaults_df$setting == .x])
      stats::setNames(as.list(vals), defaults_df$setting)
    })
  })
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
