library(shiny)
library(highcharter)

all_details1 <- all_details %>%
  filter(!(entity_name == "ANA LUX") & !(entity_name == "SES Area (RP1)"))

entity_details_mm12 <- all_details1 %>%
  #filter(date >= "2015-01-01" & date <= "2019-01-31") %>%
  group_by(yyyy, mm, entity_name, delay_group) %>%
  summarise(delay = sum(delay))

entity_summaries_mm12 <- all_summaries %>%
  #filter(date >= "2015-01-01" & date <= "2019-01-31") %>%
  group_by(yyyy, mm, entity_name) %>%
  summarise(flt_tot = sum(flights), flt_dly = sum(flights_delayed), 
            flt_dly_gt15 = sum(flights_delayed_gt15))

entity_mm12 <- inner_join(entity_details_mm12, entity_summaries_mm12, 
                          by = c("yyyy", "mm", "entity_name")) %>%
  mutate(date = ymd(str_c(yyyy, mm, "01", sep = "-")))

levels(entity_mm12$delay_group)[levels(entity_mm12$delay_group)=="W"] <- "Weather [W, D]"
levels(entity_mm12$delay_group)[levels(entity_mm12$delay_group)=="Da"] <- "ATC Disruptions [I, T]"
levels(entity_mm12$delay_group)[levels(entity_mm12$delay_group)=="Sa"] <- "ATC Staffing [S]"
levels(entity_mm12$delay_group)[levels(entity_mm12$delay_group)=="Ca"] <- "ATC Capacity [C]"
levels(entity_mm12$delay_group)[levels(entity_mm12$delay_group)=="Other"] <- "Other [all other codes]"


ui <- fluidPage(
  fluidRow(
    column(width = 3,
           uiOutput("entity"),
           uiOutput("delay_grp")
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
      choices = sort(unique(entity_mm12$entity_name)))
  })
  
  output$delay_grp <- renderUI({
    selectInput(
      inputId = "delay_grp",
      label = "Choose group of delay",
      choices = sort(unique(entity_mm12$delay_group)))
  })
  
  filtered <- reactive({
    entity_mm12 %>%
      filter(entity_name == input$entity & delay_group == input$delay_grp)
  })
  
  output$hcontainer <- renderHighchart({
    
    validate(
      need(input$entity != "", "Please select entity"),
      need(input$delay_grp != "", "Please select group of delay"))
    
    hc <- highchart(type = "stock") %>% 
      hc_title(text = "En-route ATFM delay") %>%
      hc_add_series(name = "En-route ATFM delay",
                    data = filtered(),
                    hcaes(x = datetime_to_timestamp(lubridate::ymd(date)), y = delay),
                    type = "line",
                    color = hex_to_rgba("red", 0.8)) %>%
      hc_yAxis(title = list(text = "En-route ATFM delay")) %>%
      hc_xAxis(type = 'datetime', labels = list(format = '{value:%b %Y}'))
    hc
    
  })
  
}

shinyApp(ui = ui, server = server)
