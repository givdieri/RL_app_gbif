suppressPackageStartupMessages({
  library(shiny)
  library(DT)
  library(dplyr)
  library(purrr)
  library(ggplot2)
})

source('R/helpers_data.R')
source('R/helpers_redlist.R')
source('R/mod_species.R')
source('R/mod_records.R')
source('R/mod_maps.R')
source('R/mod_criterion_a.R')
source('R/mod_settings.R')
source('R/mod_decision.R')

app_data <- load_app_data('app_data')

ui <- fluidPage(
  titlePanel('Flanders Fungal Red List Support (GBIF-only v1)'),
  sidebarLayout(
    sidebarPanel(
      mod_species_ui('species', app_data$species_master$species_working)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('records', mod_records_ui('records')),
        tabPanel('map', mod_maps_ui('maps')),
        tabPanel('criterion_a', mod_criterion_a_ui('criterion_a')),
        tabPanel('settings', mod_settings_ui('settings')),
        tabPanel('decision', mod_decision_ui('decision')),
        tabPanel('readme', includeMarkdown('README.md'))
      )
    )
  )
)

server <- function(input, output, session) {
  selected_species <- mod_species_server('species')

  settings <- mod_settings_server('settings', app_data$settings_defaults)

  mod_records_server('records', reactive(app_data$records_clean), reactive(app_data$records_analysis), selected_species)
  mod_maps_server('maps', reactive(app_data$records_analysis), selected_species, settings)

  criterion_result <- mod_criterion_a_server(
    'criterion_a',
    reactive(app_data$records_analysis),
    selected_species,
    settings
  )

  mod_decision_server('decision', criterion_result)
}

shinyApp(ui, server)
