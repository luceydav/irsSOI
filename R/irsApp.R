
#' Run Shiny App with available data
#'
#' @description
#' Takes available IRS data and runs Shiny App for exploration
#' (https://www.nber.org/research/data/individual-income-tax-statistics-zip-code-data-soi)
#'
#' @param data A data.table of IRS SOI data
#'
#' @examples
#' \dontrun{
#' irsApp()}
#'
#' @export
irsApp <- function(data = irs_app_data) {

  # If loading .fst from disc instead of built-in data, convert to data.table
  if( !identical(data, irs_app_data) ) {
    data <-  setDT(data, key = "zipcode")
  }

  header <- shinydashboard::dashboardHeader(title = "IRS Tax Dashboard")

  sidebar <- shinydashboard::dashboardSidebar(

    ## Sidebar content
    shinydashboard::dashboardSidebar(shinydashboard::sidebarMenu(
      shinydashboard::menuItem("Table", tabName = "table", icon = shiny::icon("table")),
      shinydashboard::menuItem("Charts", tabName = "charts", icon = shiny::icon("bar-chart-o")),
      shinyWidgets::selectizeGroupUI(
        id = "my-filters",
        inline = FALSE,
        params = list(
          var_one = list(
            inputId = "state",
            title = "Specify States",
            placeholder = 'select'
          ),
          var_two = list(
            inputId = "county",
            title = "Specify Counties",
            placeholder = 'select'
          ),
          var_three = list(
            inputId = "post_office_city",
            title = "Specify Cities",
            placeholder = 'select'
          ),
          var_four = list(
            inputId = "zipcode",
            title = "Specify Zipcodes",
            placeholder = 'select'
          ),
          var_five = list(
            inputId = "agi_level",
            title = "Specify Income Levels",
            placeholder = 'select'
          )
        )
      )
    ))
  )

  body <- shinydashboard::dashboardBody(

    shinydashboard::tabItems(
      # First tab content
      shinydashboard::tabItem(tabName = "table",
              shiny::fluidRow(shinydashboard::box(DT::dataTableOutput("Table"), width = 12))),
      shinydashboard::tabItem(
        tabName = "charts",
        shiny::fluidRow(shinydashboard::box(plotly::plotlyOutput("AGI_Level"), width = 12)),
        shiny::fluidRow(shinydashboard::box(plotly::plotlyOutput("TaxRate"), width = 12))
      )
    ))

  server <- function(input, output, session) {

    res_mod <- shiny::callModule(
      module = shinyWidgets::selectizeGroupServer,
      id = "my-filters",
      data = data,
      vars = c("state", "county", "post_office_city", "zipcode", "agi_level")
    )

    output$Table <- DT::renderDT({

      make_summary_DT(res_mod())

    })

    output$AGI_Level <- plotly::renderPlotly({

      make_agi_graph(res_mod())

    })

    output$TaxRate <- plotly::renderPlotly({

      make_tax_graph(res_mod())

    })
  }

  ui <- shinydashboard::dashboardPage(header, sidebar, body)

  shiny::shinyApp(ui, server)

}
