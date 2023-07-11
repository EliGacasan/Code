
library(shiny)
library(shiny.fluent)
library(tidyverse)
library(leaflet)
library(plotly)

import::from(glue, "glue")
ui <- fluentPage(
  Text(variant = "xxLarge", "Hello world!")
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)

fluentPeople %>% glimpse
fluentSalesDeals %>% glimpse()


details_list_columns <- tibble(
  fieldName = c("rep_name", "date", "deal_amount", "client_name", "city", "is_closed"),
  name = c("Sales rep", "Close date", "Amount", "Client", "City", "Is closed?"),
  key = fieldName
)

ui <- fluentPage(
  uiOutput("analysis")
)

server <- function(input, output, session) {
  filtered_deals <- reactive({
    filtered_deals <- fluentSalesDeals %>% filter(is_closed > 0)
  })
  
  output$analysis <- renderUI({
    items_list <- if(nrow(filtered_deals()) > 0) {
      DetailsList(items = filtered_deals(), columns = details_list_columns)
    } else {
      p("No matching transactions.")
    }
    Stack(
      tokens = list(childrenGap = 5),
      Text(variant = "large", "Sales deals details", block = T),
      div(style = "max-height: 500px; overflow: auto", items_list)
    )
  })
    
}
shinyApp(ui, server)

filters <- tagList(
  DatePicker.shinyInput(inputId = "fromDate", value = as.Date("2020/01/01"), label = "From date"),
  DatePicker.shinyInput(inputId = "toDate", value = as.Date("2020/12/31"), label = "To date")
)

ui <- fluentPage(
  filters,
  uiOutput("analysis")
)

server <- function(input, output, session) {
  filtered_deals <- reactive({
    req(input$fromDate)
    filtered_deals <- fluentSalesDeals %>% filter(
      date >= input$fromDate,
      date <= input$toDate,
      is_closed > 0 
    )
  })
  
  output$analysis <- renderUI({
    items_list <- if(nrow(filtered_deals()) > 0) {
      DetailsList(items = filtered_deals(), columns = details_list_columns)
    } else {
      p("No matching transactions.")
    }
    Stack(
      tokens = list(childrenGap = 5),
      Text(variant = "large", "Sales deals details", block = T),
      div(style = "max-height: 500px; overflow: auto", items_list)
    )
  })
  
}

shinyApp(ui, server)

# Filter options, using stack and label to visually arrange filter controls 
filters <- Stack(
  tokens = list(childrenGap = 10),
  Stack(
    horizontal = T,
    tokens = list(childrenGap = 10),
    DatePicker.shinyInput(inputId = "fromDate", value = as.Date("2020/01/01"), label = "From date"),
    DatePicker.shinyInput(inputId = "toDate", value = as.Date("2020/12/31"), label = "To date")
  ),
  Label("Filter by sales reps", className = "my_class"),
  NormalPeoplePicker.shinyInput(
    "selectedPeople",
    class = "my_class",
    options = fluentPeople,
    pickerSuggestionsProps = list(
      suggestionsHeaderText = "Matching people",
      mostRecentlyUsedHeaderText = "Sales reps",
      noResultsFoundtext = "No results found",
      showRemoveButtons = T
    )
  ),
  Slider.shinyInput(
    inputId = "slider",
    value = 0, min = 0, max = 10000000, step = 1000000,
    label = "Minimum amount",
    valueFormat = JS("function(x) {return '$' + x}"),
    snapToStep = T
  ),
  Toggle.shinyInput(inputId = "closedOnly", value = T, label = "Include closed deals only?")
)



# Helper function to create Fluent UI cards 
makeCard <- function(title, content, size = 12, style = "") {
  div(
    class = glue("card ms-depth-8 ms-sm{size} ms-xl{size}"),
    style = style,
    Stack(
      tokens = list(childrenGap = 5),
      Text(variant = "large", title, block = T),
      content
    )
  )
}



ui <- fluentPage(
  tags$style(".card { padding: 28px; margin-bottom: 28px; }"),
  Stack(
    tokens = list(childrenGap = 10), horizontal = T,
    makeCard("Filters", filters, size = 4, style = "max-height: 320px;"),
    makeCard("Deals count", plotlyOutput("plot"), size = 8, style = "max-height: 320px")
  ),
  uiOutput("analysis")
)


server <- function(input, output, session) {
  
  # Data
  filtered_deals <- reactive({
    req(input$fromDate)
    selectedPeople <- (
      if (length(input$selectedPeople) > 0) input$selectedPeople 
      else fluentPeople$key
    )
    minClosedVal <- if (isTRUE(input$closedOnly)) 1 else 0 
    
    filtered_deals <- fluentSalesDeals %>%
      filter(
        rep_id %in% selectedPeople,
        date >= input$fromDate,
        date <= input$toDate,
        deal_amount >= input$slider,
        is_closed >= minClosedVal
      ) %>% 
      mutate(is_closed = ifelse(is_closed == 1, "Yes", "No"))
    
    filtered_deals
  })
  
  # Render Table
  output$analysis <- renderUI({
    items_list <- if(nrow(filtered_deals()) > 0) {
      DetailsList(items = filtered_deals(), columns = details_list_columns)
    } else {
      p("No matching transactions.")
    }
    Stack(
      tokens = list(childrenGap = 10), horizontal = T,
      makeCard("Top results", div(style="max-height: 500px; overflow: auto", items_list)),
      makeCard("Map", leafletOutput("map"))
    )
    
  })
  
  output$map <- renderLeaflet({
    points <- cbind(filtered_deals()$LONGITUDE, filtered_deals()$LATITUDE)
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(data = points)
  })
  
  output$plot <- renderPlotly({
    p <- ggplot(filtered_deals(), aes(x = rep_name)) +
      geom_bar(fill = unique(filtered_deals()$color)) +
      ylab("Number of deals") +
      xlab("Sales rep") +
      theme_light()
    ggplotly(p, height = 300)
  })
  
  router_server()
}

shinyApp(ui, server)


library(dplyr)
library(ggplot2)
library(glue)
library(leaflet)
library(plotly)
library(sass)
library(shiny)
library(shiny.fluent)
library(shiny.router)



makePage <- function (title, subtitle, contents) {
  tagList(div(
    class = "page-title",
    span(title, class = "ms-fontSize-32 ms-fontWeight-semibold", style =
           "color: #323130"),
    span(subtitle, class = "ms-fontSize-14 ms-fontWeight-regular", style =
           "color: #605E5C; margin: 14px;")
  ),
  contents)
}

analysis_page <- makePage(
  "Sales representatives",
  "Best performing reps",
  div(
    Stack(
      horizontal = T,
      tokens = list(childrenGap = 10),
      makeCard("Filters", filters, size = 4, style = "max-height: 320px"),
      makeCard("Deals count", plotlyOutput("plot"), size = 8, style = "max-height: 320px")
    ), 
    uiOutput("analysis")
  )
)

ui <- fluentPage(
  tags$style(".card { padding: 28px; margin-bottom: 28px; }"),
  analysis_page
)

header <- "header"
navigation = "navigation"
footer <- "footer"

layout <- function(mainUI) {
  div(class = "grid-container",
      div(class = "header", header),
      div(class = "sidenav", navigation),
      div(class = "main", mainUI),
      div(class = "footer", footer)
  )
}

ui <- fluentPage(
  layout(analysis_page),
  tags$head(
    tags$link(href = "style.css", rel = "stylesheet", type = "text/css")
  )
)

header <- tagList(
  img(src = "appsilon-logo.png", class = "logo"),
  div(Text(variant = "xLarge", "Sales Reps Analysis"), class = "title"),
  CommandBar(
    items = list(
      CommandbarItem("New", "Add", subitems = list(
        CommandBarItem("Email message", "Mail", key = "emailMessage", href = "mailto:me@example.com"),
        CommandBarItem("Calendar event", "Calendar", key = "calendarEvent")
      )),
      CommandBarItem("Upload sales plan", "Upload"),
      CommandBarItem("Share analysis", "Share"),
      CommandBarItem("Download report", "Download")
    ),
    farItems = list(
      CommandBarItem("Grid view", "Tiles", iconOnly = T),
      CommandBarItem("Info", "Info", iconOnly = T)
    ),
    style = list(width = "100%")
  )
)

navigation <- Nav(
  groups = list(
    list(links = list(
      list(name = 'Home', url = '#!/', key = 'home', icon = 'Home'),
      list(name = 'Analysis', url = '#!/other', key = 'analysis', icon = 'AnalyticsReport'),
      list(name = 'shiny.fluent', url = 'http://github.com/Appsilon/shiny.fluent', key = 'repo', icon = 'GitGraph'),
      list(name = 'shiny.react', url = 'http://github.com/Appsilon/shiny.react', key = 'shinyreact', icon = 'GitGraph'),
      list(name = 'Appsilon', url = 'http://appsilon.com', key = 'appsilon', icon = 'WebAppBuilderFragment')
    ))
  ),
  initialSelectedKey = 'home',
  styles = list(
    root = list(
      height = '100%',
      boxSizing = 'border-box',
      overflowY = 'auto'
    )
  )
)

footer <- Stack(
  horizontal = TRUE,
  horizontalAlign = 'space-between',
  tokens = list(childrenGap = 20),
  Text(variant = "medium", "Built with ❤ by Appsilon", block=TRUE),
  Text(variant = "medium", nowrap = FALSE, "If you'd like to learn more, reach out to us at hello@appsilon.com"),
  Text(variant = "medium", nowrap = FALSE, "All rights reserved.")
)

layout <- function(mainUI){
  div(class = "grid-container",
      div(class = "header", header),
      div(class = "sidenav", navigation),
      div(class = "main", mainUI),
      div(class = "footer", footer)
  )
}


ui <- fluentPage(
  layout(analysis_page),
  tags$head(
    tags$link(href = "style.css", rel = "stylesheet", type = "text/css")
  ))


card1 <- makeCard(
  "Welcome to shiny.fluent demo!",
  div(
    Text("shiny.fluent is a package that allows you to build Shiny apps using Microsoft's Fluent UI."),
    Text("Use the menu on the left to explore live demos of all available components.")
  ))

card2 <- makeCard(
  "shiny.react makes it easy to use React libraries in Shiny apps.",
  div(
    Text("To make a React library convenient to use from Shiny, we need to write an R package that wraps it - for example, a shiny.fluent package for Microsoft's Fluent UI, or shiny.blueprint for Palantir's Blueprint.js."),
    Text("Communication and other issues in integrating Shiny and React are solved and standardized in shiny.react package."),
    Text("shiny.react strives to do as much as possible automatically, but there's no free lunch here, so in all cases except trivial ones you'll need to do some amount of manual work. The more work you put into a wrapper package, the less work your users will have to do while using it.")
  ))

home_page <- makePage(
  "This is a Fluent UI app built in Shiny",
  "shiny.react + Fluent UI = shiny.fluent",
  div(card1, card2)
)

router <- router_ui(
  route("/", home_page),
  route("other", analysis_page)
)

ui <- fluentPage(
  layout(router),
  tags$head(
    tags$link(href = "style.css", rel = "stylesheet", type = "text/css")
  ))

shinyApp(ui, server)













# --------------------------------------------------------------

library(shiny)
library(shiny.fluent)
library(tibble)
library(plotly)
library(tidyverse)

Card <- function(..., title = NULL) {
  Stack(
    class = "ms-depth-8",
    tokens = list(padding = 20, childrenGap = 5),
    if (!is.null(title)) Text(title, variant = "large"),
    ...
  )
}

Grid <- function(...) {
  div(
    class = "ms-Grid", dir = "ltr",
    style = "padding: 0px",
    ...
  )
}

GridItem <- function(..., class = "ms-sm12") {
  div(
    style = paste("ms-Grid-col", class),
    style = "padding: 10px",
    ...
  )
}



columns <- tibble(
  fieldName = c("rep_name", "date", "deal_amount", "city", "is_closed"),
  name = c("Sales rep", "Close date", "Amount", "City", "Is closed?")
)

filters <- tagList(
  div(
    Label("Sales representative"),
    NormalPeoplePicker.shinyInput("people", options = fluentPeople) 
  ),
  Toggle.shinyInput("includeOpen", label = "Include open deals")
)


ui <- fluentPage(
  Grid(
    GridItem(class = "ms-sm6",
             Card(
               title = "Filters", filters
             )
    ),
    GridItem(class = "ms-sm6",
             Card(title = "Deals count",
                  plotlyOutput("plot")) 
    ),
    GridItem(
      Card(
        title = "Sales deals details",
        div(style = "height: 500px; overflow:auto;", uiOutput("table"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  filteredDeals <- reactive({
    fluentSalesDeals %>% filter(
      is_closed | input$includeOpen,
      rep_id %in% input$people | length(input$people) == 0
    )
  })
  
  output$plot <- renderPlotly({
    ggplot(filteredDeals(), aes(x = rep_name)) + 
      geom_bar(fill = unique(filteredDeals()$color)) + 
      xlab("Sales rep") + r
    ylab("Number of deals") + 
      theme_light()
  })
  
  output$table <- renderUI({
    DetailsList(items = filteredDeals(), columns = columns, 
                checkboxVisibility = 2)
  })
  
}

shinyApp(ui, server)

# --------------------------------------------------------------



















































































































































































