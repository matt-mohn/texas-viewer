library(tidyverse)
library(readxl)
library(dplyr)
library(tigris)
library(shinycssloaders)
library(sf)
library(viridis)
library(shiny)
source("plotcode.R")

office_choices <- read.csv("tags.csv")
#loading_options <- read.csv("loadbar.csv")

calculation_choices <- c("Map by precinct",
                     "Map by county",
                     "Map by US Congress district",
                     "Map by TX Senate district",
                     "Map by TX House district",
                     "Map by TX SBOE district",
                     "Map by region")
graphic_choices <- c("Default",
                         "Overlay county",
                         "Overlay US Congress districts",
                         "Overlay TX Senate districts",
                         "Overlay TX House districts",
                         "Overlay TX SBOE districts",
                         "Hide state border")


fluidPage(
  tags$img(
    src = "site_logo.png",
    width = 300,
    height = 134
  ),
  fluidRow(
    column(
      3,
      HTML("<br><br>"),
      h4("Race 1"),
      selectInput("year1", "Year", choices = c("2002", "2004", "2006", "2008", "2010", "2012", "2014", "2016", "2018", "2020", "2022")),
      selectInput("office1", "Office", choices = office_choices),
      actionButton(
        "runplot",
        "Build map"
      ),
      HTML("<br><br>"),
      h4("Race 2 (optional)"),
      selectInput("year2", "Year", choices = c("2002", "2004", "2006", "2008", "2010", "2012", "2014", "2016", "2018", "2020", "2022")),
      selectInput("office2", "Office", choices = office_choices),
      actionButton(
        "runswingplot",
        "Build swing map"
      ),
      HTML("<br><br>"),
      h4("Options"),
      selectInput("calculation_method", "Calculation region", choices = calculation_choices),
      selectInput("graphic_method", "Overlay options", choices = graphic_choices),
      checkboxInput("swing_alternate", "Plot swings in one line"),
      checkboxInput("use_percentiles", "Plot income, etc. via percentiles"),
      HTML("<br><br>"),
      h4("Custom labelling"),
      textInput("title", "Custom map title"),
      textInput("subtitle", "Custom map subtitle"),
      textInput("annotations", "Custom map annotation"),
      textInput("chart_title", "Custom demographic plot title"),
      textInput("chart_subtitle", "Custom demographic plot subtitle"),
      textInput("chart_yaxis", "Custom demographic plot y-axis label"),
      tags$a(href="https://github.com/matt-mohn/texas-viewer", "GitHub")
    ),
    column(1),
    column(
      8,
      shinycssloaders::withSpinner(plotOutput("Map", inline = TRUE)),
      br(),
      shinycssloaders::withSpinner(plotOutput("DemographyPlot", inline = TRUE)),
      br(),
      shinycssloaders::withSpinner(plotOutput("CensusPlot"))
    )
  )
)
