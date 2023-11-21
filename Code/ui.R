library(shiny)
library(shinydashboard)
library(shinyalert)
library(tidyverse)
library(sf)
library(leaflet)
library(plotly)

dashboardPage(
  dashboardHeader(title = "Death Rate Tracker (2019)", 
                  titleWidth = 300),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      box(title = "Death Rate Across Countries",
          div(id = "map-container",
              style = "margin-top: 8px;",
              leafletOutput("death_map", height =500)
          ),
          height = 600),
      box(title = "Breakdown of 2019 Death Rate (Age and Country)",
          selectInput("age_selector", choices = c(), label = "Select Age Group"),
          selectInput("country_selector", choices = c(), label = "Select Country"),
          plotlyOutput("cod", height = 550),
          height = 775,
          width = 6)
    ),
    fluidRow(
      tags$style("
          .above-box {
            margin-top: -50px;
            margin-left: 305px;
          }
          .view-box {
            margin-top: -180px;
            margin-left: -15px;
          }
          .high-box {
            margin-top: -180px;
            margin-left: 190px;
          }
          .reason-box {
            margin-top: -180px;
            margin-left: 435px;
          }
        "),
      column(width = 3, class = "above-box", 
             actionButton("alert_button", "References")),
      column(width = 5, class = "view-box",
             valueBoxOutput("averageDeathRate")),
      column(width = 6, class = "high-box",
             valueBoxOutput("Highestdeath")),
      column(width = 8, class = "reason-box",
             valueBoxOutput("topreason"))
    )
  )
)

