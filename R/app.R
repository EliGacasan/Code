# Shiny Fluent App 

# I'm new to microsoft fluentUI using R, a framework which is more flexible than other
# frameworks for creating dashboards, essentially, there is much more control over UI
# components, although, a higher learning curve.

# https://developer.microsoft.com/en-us/fluentui#/controls/web/toggle#feedback
# https://appsilon.github.io/shiny.fluent/

# Dataset Information link
# https://data-openjustice.doj.ca.gov/sites/default/files/dataset/2022-08/Arrests%20Context_081122.pdf

# Dashboard/Shiny libraries
library(shiny)
library(bs4Dash)
library(shinydashboardPlus)
library(shinyWidgets)
# Plotting libraries
library(highcharter)
library(DT)

offenses <- c("VIOLENT", "PROPERTY", "F_DRUGOFF", "F_SEXOFF", "F_ALLOTHER", "F_TOTAL", "M_TOTAL")
names(offenses) <- c("Violent Offenses", "Property Offenses", "Felony Drug Offenses",
                     "Felony Sex Offenses", "Felony Other Offenses", "Total Felonies", "Total Misdimeanors")
ages <- as.character(unique(data$AGE_GROUP))
stringr::str_to_lower(ages)

ui <- bs4DashPage(
  dark = T,
  header = bs4DashNavbar(
    title = "California Arrests Dashboard",
    compact = F,
    fixed = F
  ),
  sidebar = bs4DashSidebar(
    id = "sidebar",
    minified = F,
    width = "300px",
    skin = "light",
    bs4SidebarMenu(
      bs4SidebarMenuItem("Home", tabName = "homePage", icon = icon("house")),
      bs4SidebarMenuItem("Crime Dashboard", tabName = "dashboardPage", icon = icon("chart-line")),
      bs4SidebarMenuItem("Statistical Analysis", tabName = "statisticsPage", icon = icon("bolt"))
    )
  ),
  
  body = bs4DashBody(
    bs4TabItems(
      bs4TabItem(
        tabName = "homePage",
        bs4Dash::column(
          width = 12,
          div(tags$iframe(style="height:800px; width: 1000px; scrolling=yes", 
                          src="https://data-openjustice.doj.ca.gov/sites/default/files/dataset/2022-08/Arrests%20Context_081122.pdf"))
        )
      ),
      bs4TabItem(
        tabName = "dashboardPage",
        fluidRow(
          bs4Dash::column(
            width = 4,
            bs4Card(
              title = "Data Filtration",
              width = NULL,
              headerBorder = T,
              background = "navy",
              solidHeader = T,
              collapsible = F,
              gradient = T,
              airYearpickerInput(inputId = "selectYear", label = "Select Years:", view = "years", range = F,
                                 dateFormat = "yyyy", minDate = "1981-01-01", maxDate = "2020-01-01",
                                 value = c("1981-01-01", "2010-01-01"), multiple = 2),
              pickerInput(inputId = "selectCounty", label = "Select County", multiple = T,
                          choices = unique(as.character(arrests$COUNTY)), selected = c("Los Angeles", "San Francisco"), 
                          options = list(`actions-box` = T)),
              pickerInput(inputId = "selectGender", label = "Select Gender", choices = c("Male", "Female"),
                          multiple = T, selected = "Male"),
              pickerInput(inputId = "selectRace", label = "Select Race", multiple = T,
                          choices = c("Black", "Hispanic", "Other","White"), 
                          selected = c("Black", "Hispanic", "Other","White")),
              pickerInput(inputId = "selectAgeGroup", label = "Select Age Group", multiple = T,
                          choices = c("Under 18", "18 to 19", "20 to 29", "30 to 39", "40 to 69", "70 and over"),
                          selected = c("Under 18", "18 to 19", "20 to 29", "30 to 39", "40 to 69", "70 and over"),
                          options = list(`actions-box` = T)),
              pickerInput(inputId = "selectOffenseCategory", label = "Select Offense", multiple = F,
                          choices = offenses),
              switchInput(inputId = "switch", value = F, onLabel = "Rates", offLabel = "Totals", label = "Type")
            )
          ),
          bs4Dash::column(
            width = 8,
            bs4Card(
              title = "Crime Map",
              headerBorder = T,
              background = "navy",
              solidHeader = T,
              collapsible = F,
              width = NULL,
              gradient = T,
              height = "672.5px",
              highchartOutput("map", height = "645px")
            )
          ),
          bs4Dash::column(
            width = 12,
            bs4Card(
              title = "Time Plot",
              headerBorder = T,
              background = "navy",
              solidHeader = T,
              collapsible = F,
              width = NULL,
              gradient = T,
              highchartOutput("timePlot")
            )
          )
        )
        
      ),
      bs4TabItem(
        tabName = "statisticsPage",
        fluidRow(
          bs4Dash::column(
            width = 6, 
            bs4Card(
              title = "Loglinear Model Options",
              headerBorder = T,
              background = "navy",
              solidHeader = T,
              collapsible = F,
              width = NULL,
              gradient = T,
              airYearpickerInput(inputId = "selectYear2", label = "Select Years:", view = "years", range = F,
                                 dateFormat = "yyyy", minDate = "1981-01-01", maxDate = "2020-01-01",
                                 value = c("1981-01-01", "2010-01-01"), multiple = 2),
              pickerInput(inputId = "selectCounty2", label = "Select County", multiple = T,
                          choices = unique(as.character(arrests$COUNTY)), selected = c("Los Angeles", "San Francisco"), 
                          options = list(`actions-box` = T)),
              pickerInput(inputId = "selectOffenseCategory2", label = "Select Offense", multiple = F,
                          choices = offenses),
              pickerInput(inputId = "refCatRACE", label = "Race Reference Category", choices = unique(as.character(data$RACE)), multiple = F),
              pickerInput(inputId = "refCatAGE_GROUP", label = "Age Reference Category", choices = unique(as.character(data$AGE_GROUP)), multiple = F),
              height = "500px"
            )
          ),
          bs4Dash::column(
            width = 6, 
            bs4Card(
              title = "Model Estimates",
              headerBorder = T,
              background = "navy",
              solidHeader = T,
              collapsible = F,
              width = NULL,
              gradient = T,
              height = "500px", 
              tags$head(tags$style("#UIglmOutput{ 
                                   font-size: 25px;
                                   }")),
              div(uiOutput("UIglmOutput", fill = T, inline = T), style = "padding-bottom: 50px; margin-bottom: 1px")
            )
          ),
          bs4Dash::bs4InfoBoxOutput("box1"),
          bs4Dash::bs4InfoBoxOutput("box2"),
          bs4Dash::bs4InfoBoxOutput("box3"),
          bs4Dash::bs4InfoBoxOutput("box4"),
          bs4Dash::bs4InfoBoxOutput("box5"),
          bs4Dash::bs4InfoBoxOutput("box6"),
          bs4Dash::bs4InfoBoxOutput("box7"),
          bs4Dash::bs4InfoBoxOutput("box8"),
          bs4Dash::bs4InfoBoxOutput("box9")
          
        )
      )
      
      
    )
  ),
  
  controlbar = bs4DashControlbar(
    width = "1450px",
    bs4Card(
      title = "Data Tables",
      headerBorder = T,
      background = "navy",
      solidHeader = F,
      width = 12,
      collapsible = F,
      gradient = T,
      height = "800px",
      dataTableOutput("table", height = "700px")
    )
  ),
  
  footer = bs4DashFooter(
    
  )
  
)

server <- function(input, output, session) {
  
  # Reactive Data
  #-------------- Time Plot Stuff
  
  # Time Plot Data
  data.react <- reactive({
    req(input$selectYear)
    if (length(input$selectYear) == 2) {
      if (as.numeric(substr(as.character(input$selectYear[1]), 1, 4)) < as.numeric(substr(as.character(input$selectYear[2]), 1, 4))) {
        data %>% dplyr::filter(YEAR >= as.numeric(substr(as.character(input$selectYear[1]), 1, 4)),
                                  YEAR <= as.numeric(substr(as.character(input$selectYear[2]), 1, 4)),
                                  COUNTY %in% input$selectCounty, GENDER %in% input$selectGender,
                                  RACE %in% input$selectRace, AGE_GROUP %in% input$selectAgeGroup)
      } else if (as.numeric(substr(as.character(input$selectYear[1]), 1, 4)) > as.numeric(substr(as.character(input$selectYear[2]), 1, 4))) {
        data %>% dplyr::filter(YEAR <= as.numeric(substr(as.character(input$selectYear[1]), 1, 4)),
                                  YEAR >= as.numeric(substr(as.character(input$selectYear[2]), 1, 4)),
                                  COUNTY %in% input$selectCounty, GENDER %in% input$selectGender,
                                  RACE %in% input$selectRace, AGE_GROUP %in% input$selectAgeGroup)
      }
    } else {
      data %>% dplyr::filter(YEAR == as.numeric(substr(as.character(input$selectYear[1]), 1, 4)),
                                COUNTY %in% input$selectCounty, GENDER %in% input$selectGender,
                                RACE %in% input$selectRace, AGE_GROUP %in% input$selectAgeGroup)
    }
   
  })
  
  data.react.count <- reactive({
    data.react() %>% plyr::count(vars = c("YEAR", "COUNTY", "Population"), wt_var = input$selectOffenseCategory) %>%
      group_by(YEAR, COUNTY) %>% summarize(Rate = freq/Population * 100000, freq, Population)
  })
  
  # Time Plot
  output$timePlot <- renderHighchart({
    if (input$switch == F) {
      hchart(data.react.count(), hcaes(x = YEAR, y = freq, group = COUNTY), type = "column") %>% 
        hc_add_theme(hc_theme_flatdark()) %>%
        hc_xAxis(title = list(text = "Year")) %>%
        hc_yAxis(title = list(text = paste("# of", names(which(offenses == input$selectOffenseCategory)))))
    } else if (input$switch == T) {
      hchart(data.react.count(), hcaes(x = YEAR, y = Rate, group = COUNTY), type = "column") %>% 
        hc_add_theme(hc_theme_flatdark()) %>%
        hc_xAxis(title = list(text = "Year")) %>%
        hc_yAxis(title = list(text = paste("Rate of", names(which(offenses == input$selectOffenseCategory)), "per 100,000 persons")))
    }
    
  }) 
 
  #--------------
  
  #-------------- Map 
  
  # Map Data
  data.react.map <- reactive({
    req(input$selectYear)
    if (length(input$selectYear) == 2) {
      if (as.numeric(substr(as.character(input$selectYear[1]), 1, 4)) < as.numeric(substr(as.character(input$selectYear[2]), 1, 4))) {
        data %>% dplyr::filter(YEAR >= as.numeric(substr(as.character(input$selectYear[1]), 1, 4)),
                                  YEAR <= as.numeric(substr(as.character(input$selectYear[2]), 1, 4)), GENDER %in% input$selectGender,
                                  RACE %in% input$selectRace, AGE_GROUP %in% input$selectAgeGroup)
      } else if (as.numeric(substr(as.character(input$selectYear[1]), 1, 4)) > as.numeric(substr(as.character(input$selectYear[2]), 1, 4))) {
        data %>% dplyr::filter(YEAR <= as.numeric(substr(as.character(input$selectYear[1]), 1, 4)),
                                  YEAR >= as.numeric(substr(as.character(input$selectYear[2]), 1, 4)), GENDER %in% input$selectGender,
                                  RACE %in% input$selectRace, AGE_GROUP %in% input$selectAgeGroup)
      }
    } else {
      data %>% dplyr::filter(YEAR == as.numeric(substr(as.character(input$selectYear[1]), 1, 4)), GENDER %in% input$selectGender,
                                RACE %in% input$selectRace, AGE_GROUP %in% input$selectAgeGroup)
    }
  })
  
  data.react.count.map <- reactive({
    data.react.map() %>% plyr::count(vars = c("COUNTY"), wt_var = input$selectOffenseCategory)
  })
  
  data.react.map.rate <- reactive({
    data.react.map() %>% plyr::count(vars = c("COUNTY", "Population"), wt_var = input$selectOffenseCategory) %>%
      group_by(COUNTY, Population) %>% summarize(Rate = freq/Population * 100000, freq, Population) %>%
      group_by(COUNTY) %>% summarize(AverageRate = round(mean(Rate), digits = 1))
  })
  
  # Map
  output$map <- renderHighchart({
    if (input$switch == F) {
      hcmap(map = "countries/us/us-ca-all.js",
            data = data.react.count.map(),
            value = "freq",
            name = paste("# of", names(which(offenses == input$selectOffenseCategory))),
            joinBy = c("name", "COUNTY"),
            borderColor = "#2c5269",
            borderWidth = 1) 
    } else if (input$switch == T) {
      hcmap(map = "countries/us/us-ca-all.js",
            data = data.react.map.rate(),
            value = "AverageRate",
            name = paste("Rate of", names(which(offenses == input$selectOffenseCategory))),
            joinBy = c("name", "COUNTY"),
            borderColor = "#2c5269",
            borderWidth = 1) 
    }
    
    
  })
  
  #--------------
  
  
  
  #-------------- Home
  output$test <- renderText({
    paste("Class:", class(input$selectYear), "  Format:", input$selectYear,"\n", 
          "Turned Class:", class(as.numeric(substr(as.character(input$selectYear), 1, 4))), " Format:", as.numeric(substr(as.character(input$selectYear), 1, 4))) 
  })
  #--------------
  
  #-------------- Tables
  output$table <- renderDataTable({
    datatable(data.react(), fillContainer = T, 
              options = list(
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': 'grey', 'color': 'white'});",
                  "}")
              )) %>%  formatStyle(colnames(data.react()),  color = 'black', backgroundColor = 'white', fontWeight = 'bold')
  })
  
 
  #--------------
  
  #-------------- Statistical Analysis Page
  
  stats.data <- reactive({
    req(input$selectYear2)
    req(input$selectCounty2)
    
    data$RACE <- relevel(data$RACE, ref = input$refCatRACE)
    data$AGE_GROUP <- relevel(data$AGE_GROUP, ref = input$refCatAGE_GROUP)
    
    if (length(input$selectYear2) == 2) {
      if (as.numeric(substr(as.character(input$selectYear2[1]), 1, 4)) < as.numeric(substr(as.character(input$selectYear2[2]), 1, 4))) {
        data %>% dplyr::filter(YEAR >= as.numeric(substr(as.character(input$selectYear2[1]), 1, 4)),
                               YEAR <= as.numeric(substr(as.character(input$selectYear2[2]), 1, 4)),
                               COUNTY %in% input$selectCounty2) 
      } else if (as.numeric(substr(as.character(input$selectYear2[1]), 1, 4)) > as.numeric(substr(as.character(input$selectYear2[2]), 1, 4))) {
        data %>% dplyr::filter(YEAR <= as.numeric(substr(as.character(input$selectYear2[1]), 1, 4)),
                               YEAR >= as.numeric(substr(as.character(input$selectYear2[2]), 1, 4)),
                               COUNTY %in% input$selectCounty2)
      }
    } else {
      data %>% dplyr::filter(YEAR == as.numeric(substr(as.character(input$selectYear2[1]), 1, 4)),
                             COUNTY %in% input$selectCounty2)
    }
  })
  
  glmModel <- reactive({
    glm(formula = paste(input$selectOffenseCategory2, "~ GENDER + RACE + AGE_GROUP"), 
        family = poisson(link = "log"), offset = log(Population), data = stats.data())
  }) %>% bindCache(input$selectOffenseCategory2, stats.data())
  
  
  glmPrint <- reactive({
    round(Confint(glmModel(), exponentiate = T), digits = 3) 
  }) %>% bindCache(glmModel(), input$selectOffenseCategory2)
  
  
  
  
  output$glmOutput <- renderPrint(
    expr = {
      print(glmPrint())
    }
  )
  
  output$UIglmOutput <- renderUI({
    verbatimTextOutput("glmOutput")
  })
  
  
  #--------------
 
  # BS4 Info Boxes
  #--------------
  output$box1 <- renderbs4InfoBox({
    
    
    bs4InfoBox(
      title = paste0("The rate of ", names(which(offenses == input$selectOffenseCategory2)),
                     " for ", levels(stats.data()$RACE)[2], "s is"),
      value = glmPrint()[3,1],
      subtitle = paste0("times that for ", input$refCatRACE, "s."),
      color = "olive",
      fill = T,
      gradient = T
    )
  })
  
  output$box2 <- renderbs4InfoBox({
    bs4InfoBox(
      title = paste0("The rate of ", names(which(offenses == input$selectOffenseCategory2)),
                     " for ", levels(stats.data()$RACE)[3], "s is"),
      value = glmPrint()[4,1],
      subtitle = paste0("times that for ", input$refCatRACE, "s."),
      color = "olive",
      fill = T,
      gradient = T
    )
  })
  
  output$box3 <- renderbs4InfoBox({
    bs4InfoBox(
      title = paste0("The rate of ", names(which(offenses == input$selectOffenseCategory2)),
                     " for ", levels(stats.data()$RACE)[4], "s is"),
      value = glmPrint()[5,1],
      subtitle = paste0("times that for ", input$refCatRACE, "s."),
      color = "olive",
      fill = T,
      gradient = T
    )
  })
  
  output$box4 <- renderbs4InfoBox({
    bs4InfoBox(
      title = paste0("The rate of ", names(which(offenses == input$selectOffenseCategory2)),
                     " for people ", levels(stats.data()$AGE_GROUP)[2], " is "),
      value = glmPrint()[6,1],
      subtitle = paste0("times that of people ", input$refCatAGE_GROUP),
      color = "lightblue",
      fill = T,
      gradient = T
    )
  })
  
  output$box5 <- renderbs4InfoBox({
    bs4InfoBox(
      title = paste0("The rate of ", names(which(offenses == input$selectOffenseCategory2)),
                     " for people ", levels(stats.data()$AGE_GROUP)[3], " is "),
      value = glmPrint()[7,1],
      subtitle = paste0("times that of people ", input$refCatAGE_GROUP),
      color = "lightblue",
      fill = T,
      gradient = T
    )
  })
  
  output$box6 <- renderbs4InfoBox({
    bs4InfoBox(
      title = paste0("The rate of ", names(which(offenses == input$selectOffenseCategory2)),
                     " for people ", levels(stats.data()$AGE_GROUP)[4], " is "),
      value = glmPrint()[8,1],
      subtitle = paste0("times that of people ", input$refCatAGE_GROUP),
      color = "lightblue",
      fill = T,
      gradient = T
    )
  })
  
  output$box7 <- renderbs4InfoBox({
    bs4InfoBox(
      title = paste0("The rate of ", names(which(offenses == input$selectOffenseCategory2)),
                     " for people ", levels(stats.data()$AGE_GROUP)[5], " is "),
      value = glmPrint()[9,1],
      subtitle = paste0("times that of people ", input$refCatAGE_GROUP),
      color = "lightblue",
      fill = T,
      gradient = T
    )
  })
  
  output$box8 <- renderbs4InfoBox({
    bs4InfoBox(
      title = paste0("The rate of ", names(which(offenses == input$selectOffenseCategory2)),
                     " for people ", levels(stats.data()$AGE_GROUP)[6], " is "),
      value = glmPrint()[10,1],
      subtitle = paste0("times that of people ", input$refCatAGE_GROUP),
      color = "lightblue",
      fill = T,
      gradient = T
    )
  })
  
  output$box9 <- renderbs4InfoBox({
    bs4InfoBox(
      title = paste0("The rate of ", 
                     names(which(offenses == input$selectOffenseCategory2)),
                     " for ", "Male", "s is "),
      value = glmPrint()[2,1],
      subtitle = paste0("times that for ", as.character(unique(stats.data()$GENDER)[2]), "s."),
      color = "maroon",
      fill = T,
      gradient = T
    )
  })
  
  
}



shinyApp(ui, server)












































































