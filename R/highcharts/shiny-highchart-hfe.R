library(shiny)
library(highcharter)

allnew1 <- all %>%
  filter(entity_type == "State (FIR)") %>% 
  mutate(var = ((dist_flown_km / dist_achieved_km) -1)*100) %>%
  mutate(perc = 100 - var) %>%
  filter(date >= "2016-01-01" & date <= "2019-03-31") %>%
  select(date, type_model, perc, entity_name)

dataset1 <- spread(allnew1, type_model, perc)

ui <- fluidPage(
  fluidRow(
    column(width = 3,
           uiOutput("entity")
           )
    ),
    column(width = 8,
           highchartOutput("hcontainer",height = "500px")
    )
  )


server <- function(input, output) {
  
  output$entity <- renderUI({
    selectInput(
      inputId = "entity",
      label = "Choose entity",
      choices = sort(unique(dataset1$entity_name)))
  })
  
  filtered <- reactive({
    dataset1 %>%
      filter(entity_name == input$entity)
  })
  
  output$hcontainer <- renderHighchart({
    
    validate(
      need(input$entity != "", "Please select entity"))
    
   hc <- highchart(type = "stock") %>% 
      hc_title(text = "Horizontal en-route flight efficiency") %>%
      hc_add_series(name = "Actual trajectory",
                    data = filtered(),
                    hcaes(x = datetime_to_timestamp(lubridate::ymd(date)), y = CPF),
                    type = "line",
                    color = hex_to_rgba("red", 0.5)) %>%
      hc_add_series(name = "Flight plan",
                    data = filtered(),
                    hcaes(x = datetime_to_timestamp(lubridate::ymd(date)), y = FTFM), 
                    type = "line",
                    color = hex_to_rgba("blue", 0.5),
                    showInNavigator = TRUE) %>%
      hc_add_series(name = "Shortest constraint route",
                    data = filtered(),
                    hcaes(x = datetime_to_timestamp(lubridate::ymd(date)), y = SCR), 
                    type = "line",
                    color = hex_to_rgba("green", 0.5),
                    showInNavigator = TRUE) %>%
      hc_yAxis(min = 90, title = list(text = "efficiency (%)")) %>%
      hc_xAxis(type = 'datetime', labels = list(format = '{value:%b %d}')) %>%
      hc_tooltip(valueDecimals = 2)
   hc
   
  })
  
}

shinyApp(ui = ui, server = server)
