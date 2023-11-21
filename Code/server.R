library(shiny)
library(shinydashboard)
library(shinyalert)
library(tidyverse)
library(sf)
library(leaflet)
library(ggthemes)
library(plotly)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  options(scipen = 999)
  # Reading Data Files
  

  death_toll <- read_csv("../data/DeathRate(World2019).csv")
  cod_5to14 <- read_csv("../data/CauseOfDeath(5-14).csv")
  cod_15to49 <- read_csv("../data/CauseOfDeath(15-49).csv")
  cod_50to69 <- read_csv("../data/CauseOfDeath(50-69).csv")
  cod_above70 <- read_csv("../data/CauseOfDeath(above70).csv")
  world_shape_file <- st_read("../data/WorldBoundaries.csv/world-administrative-boundaries.shp")

  
  # Data Preprocessing
  
  death_toll <- death_toll[complete.cases(death_toll), ]
  
  cod_5to14 <- cod_5to14 %>% filter(`Year` == "2019")
  cod_5to14 <- cod_5to14[complete.cases(cod_5to14), ]
  
  cod_15to49 <- cod_15to49 %>% filter(`Year` == "2019")
  cod_15to49 <- cod_15to49[complete.cases(cod_15to49), ]
  
  cod_50to69 <- cod_50to69 %>% filter(`Year` == "2019")
  cod_50to69 <- cod_50to69[complete.cases(cod_50to69), ]
  
  cod_above70 <- cod_above70 %>% filter(`Year` == "2019")
  cod_above70 <- cod_above70[complete.cases(cod_above70), ]
  
  cause_of_death <- rbind(cod_5to14, cod_15to49, cod_50to69, cod_above70)
  
  cause_of_death <- cause_of_death %>% 
    pivot_longer(cols = 5:ncol(cause_of_death), 
                 names_to = "Cause of Death(COD)", 
                 values_to = "Total Deaths")
  
  
  # Merging Data Files
  
  world_shape_file <- merge(world_shape_file, death_toll, by.x = "iso3", by.y = "Country Code", all.x = TRUE)
  world_file <- world_shape_file %>% st_as_sf()
  
  # Age Selector Choices
  
  updateSelectInput(
    session = session,
    "age_selector",
    choices = c('All',unique(cause_of_death$`Age Group`)),
    selected = "All"
  )
  
  # Country selector Choices
  
  updateSelectInput(
    session = session,
    "country_selector",
    choices = c('All',unique(cause_of_death$Entity)),
    selected = "All"
  )
  
  
  # Add Plot_ly Box Plot
  
  output$cod <- renderPlotly({
    plot_data <- cause_of_death
    if (input$age_selector == 'All' & input$country_selector != 'All'){
      plot_data <- plot_data %>% 
        filter(Entity == input$country_selector) %>% 
        group_by(`Cause of Death(COD)`) %>% 
        summarise(`Total Deaths` = sum(`Total Deaths`))
    }else if (input$country_selector == 'All' & input$age_selector != 'All'){
      plot_data <- plot_data %>% 
        filter(`Age Group` == input$age_selector) %>% 
        group_by(`Cause of Death(COD)`) %>% 
        summarise(`Total Deaths` = sum(`Total Deaths`))
    }else if (input$age_selector == 'All' & input$country_selector == 'All'){
      plot_data <- plot_data %>% 
        group_by(`Cause of Death(COD)`) %>% 
        summarise(`Total Deaths` = sum(`Total Deaths`))
    } else {
      plot_data <- plot_data %>% 
        filter(`Age Group` == input$age_selector & Entity == input$country_selector)
    }
    
    plot <- plot_data %>% 
      mutate(`Cause of Death(COD)` = reorder(`Cause of Death(COD)`, `Total Deaths`)) %>%
      ggplot(aes(x = `Cause of Death(COD)`, y = `Total Deaths`)) +
      scale_y_continuous(trans = "sqrt") +
      geom_col(aes(text = prettyNum(`Total Deaths`, big.mark = ",", scientific = FALSE)), fill = "#0099f9") +
      theme_minimal() +
      xlab("Cause of Deaths") +
      ylab("Total Deaths") +
      coord_flip() +
      theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1))
    
    ggplotly(plot, tooltip = c("text"))
    })

  # Add Leaflet Choropleth Map
  
  output$death_map <- renderLeaflet({
    
    # Creating a Palette
    
    pal <- colorNumeric(palette = "YlOrBr", domain = world_file$`2019`, na.color=rgb(0,0,0,0))
    
    # Creating a Label
    
    mytext <- paste(
      "Country: ", world_file$`Country Name`,"<br/>",
      "Death Rate: ", world_file$`2019`,"<br/>",
      sep="") %>%
      lapply(htmltools::HTML)
    
    # Leaflet Plot
    
    leaflet() %>% 
      addProviderTiles("CartoDB.Voyager") %>% 
      addPolygons(
        data = world_file,
        fillColor = ~pal(`2019`), 
        stroke = TRUE, 
        fillOpacity = 7, 
        color = "white",
        weight = 0.3, 
        label = mytext, 
        labelOptions = labelOptions( 
          style = list("font-weight" = "bold", 
                       padding = "3px 8px",
                       "color" = "black",
                       "font-family" = "Sans-Serif"),
          textsize = "16px", 
          direction = "auto"),
        highlightOptions = highlightOptions(
          weight = 1,
          color = "black",
          fillOpacity = 0.5,
          bringToFront = TRUE)
      ) %>% addLegend("bottomleft", 
                      pal = pal, 
                      values = ~`2019`,
                      title = "Death Rate(%)",
                      labFormat = labelFormat(suffix = "%"),
                      opacity = 1,
                      data = world_file) %>% 
      setView(lng = 0, lat = 0, zoom = 1)
    })
  
  observeEvent(input$alert_button, {
    shinyalert(
      title = "References",
      text = "1. World Bank. (2017). Death rate, crude (per 1,000 people) | Data. Worldbank.org. https://data.worldbank.org/indicator/SP.DYN.CDRT.IN\n
      2. Causes of deaths for children between 5 and 14. (n.d.). Our World in Data. https://ourworldindata.org/grapher/causes-of-death-in-5-14-year-olds\n
      3. Causes of deaths for 15 to 49 year olds. (n.d.). Our World in Data. https://ourworldindata.org/grapher/causes-of-death-in-15-49-year-olds\n
      4. Causes of deaths for 50- to 69-year-olds. (n.d.). Our World in Data. Retrieved June 6, 2023, from https://ourworldindata.org/grapher/causes-of-death-in-50-69-year-olds\n
      5. Causes of deaths for people who were 70 years and older. (n.d.). Our World in Data. Retrieved June 6, 2023, from https://ourworldindata.org/grapher/causes-of-death-in-70-year-olds\n",
      size = "l",
      confirmButtonText = "Return to App",
      confirmButtonCol = "#AEDEF4",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      type = "info"
    )
  })
  
  # Value Box
  output$averageDeathRate <- renderValueBox({
    valueBox(
      paste0(round(mean(world_file$`2019`, na.rm = TRUE), 2), "%"),
      "Avg. Death Rate",
      icon = icon("chart-simple"),
      color = "yellow"
    )
  })
  
  output$Highestdeath <- renderValueBox({
    valueBox(
      world_file %>% 
        filter(`2019` == max(world_file$`2019`, na.rm = TRUE)) %>% 
        as.data.frame() %>% 
        select(`Country Name`),
      "Highest Deaths",
      icon = icon("arrow-up"),
      color = "blue"
    )
  })
  
  output$topreason <- renderValueBox({
    valueBox(
      HTML("<h3 style='font-size: 14.5px;'>Cardiovascular<br>Disease</h3>"),
      "Top Cause of Deaths",
      icon = icon("circle-info"),
      color = "red"
    )
  })
  
}


