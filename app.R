library(shiny)
library(shinydashboard)
library(data.table)
library(magrittr)
library(DT)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(fst)
#library(shinycssloaders)

# Data
irs_app_data <- read_fst("irs_app.fst")
irs_app_data <- setDT(irs_app_data, key = "zipcode")

header <- dashboardHeader(title = "IRS Tax Dashboard")

sidebar <- dashboardSidebar(
  
  ## Sidebar content
  dashboardSidebar(sidebarMenu(
    menuItem("Table", tabName = "table", icon = icon("table")),
    menuItem("Charts", tabName = "charts", icon = icon("bar-chart-o")),
    selectizeGroupUI(
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

body <- dashboardBody(
  
  tabItems(
    # First tab content
    tabItem(tabName = "table",
            fluidRow(shinydashboard::box(dataTableOutput("Table"), width = 12))),
    tabItem(
      tabName = "charts",
      fluidRow(shinydashboard::box(plotlyOutput("AGI_Level"), width = 12)),
      fluidRow(shinydashboard::box(plotlyOutput("TaxRate"), width = 12))
      )
  ))

server <- function(input, output, session) {
  
  res_mod <- callModule(
    module = selectizeGroupServer,
    id = "my-filters",
    data = irs_app_data,
    vars = c("state", "county", "post_office_city", "zipcode", "agi_level")
  )
  
  output$Table <- renderDT({
    
    make_summary_DT(res_mod())

    })
  
  output$AGI_Level <- renderPlotly({
    
    make_agi_graph(res_mod())
    
  })
  
  output$TaxRate <- renderPlotly({
    
    make_tax_graph(res_mod())
    
  })
}

ui <- dashboardPage(header, sidebar, body)

shinyApp(ui, server)