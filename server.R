source("plotcode.R")
library(tidyverse)
library(readxl)
library(dplyr)
library(tigris)
library(sf)
library(viridis)
library(shiny)

# Define server logic required to draw a histogram


function(input, output, session) {
  n <- data.frame(cbind(c(0, 2), c(0, 2)))
  colnames(n) <- c("a", "b")
  geoid <- ggplot() +
    geom_point(data = n, mapping = aes(x = a, y = b), color = "white") +
    theme_void()

  output$Map <- renderPlot(geoid, res = 2, width = 10, height = 10)
  output$DemographyPlot <- renderPlot(geoid, res = 2, width = 10, height = 10)
  output$CensusPlot <- renderPlot(geoid, res = 2, width = 10, height = 10)

  observeEvent(
    input$runplot,
    {
      year_one <- input$year1
      office_one <- input$office1

      calculation_choice <- "Map by precinct"
      graphic_choice <- "Default"

      calculation_choice <- input$calculation_method
      graphics_choice <- input$graphic_method
      
      compress_swing <- F
      compress_swing <- input$swing_alternate


      use_county <- F
      use_county <- if_else(calculation_choice == "Map by county", T, F)
      use_cd <- F
      use_cd <- if_else(calculation_choice == "Map by US Congress district", T, F)
      use_hd <- F
      use_hd <- if_else(calculation_choice == "Map by TX House district", T, F)
      use_sd <- F
      use_sd <- if_else(calculation_choice == "Map by TX Senate district", T, F)
      use_ed <- F
      use_ed <- if_else(calculation_choice == "Map by TX SBOE district", T, F)
      use_rg <- F
      use_rg <- if_else(calculation_choice == "Map by region", T, F)
      map_subtitle <- "NA"
      map_title <- "NA"
      map_annotations <- "NA"
      overlay_cty <- F
      overlay_cty <- if_else(graphics_choice == "Overlay county", T, F)
      overlay_cd <- F
      overlay_cd <- if_else(graphics_choice == "Overlay US Congress districts", T, F)
      hide_state_overlay <- F
      hide_state_overlay <- if_else(graphics_choice == "Hide state border", T, F)
      overlay_hd <- F
      overlay_hd <- if_else(graphics_choice == "Overlay TX House districts", T, F)
      overlay_sd <- F
      overlay_sd <- if_else(graphics_choice == "Overlay TX Senate districts", T, F)
      overlay_ed <- F
      overlay_ed <- if_else(graphics_choice == "Overlay TX SBOE districts", T, F)




      overlay_geom_type <- geom_type_builder(c(
        overlay_cty,
        overlay_cd,
        overlay_ed,
        overlay_hd,
        overlay_sd,
        F,
        hide_state_overlay
      ))



      geom_type <- geom_type_builder(c(use_county, use_cd, use_ed, use_hd, use_sd, use_rg, F))

      map_title <- binder(year_one, office_one)
      map_subtitle <- subtitle_handler(geom_type)

      if (input$title != "") {
        map_title <- input$title
      }
      if (input$subtitle != "") {
        map_subtitle <- input$subtitle
      }
      if (input$annotations != "") {
        map_annotations <- input$annotations
      }

      map_labels <- c(map_title, map_subtitle, map_annotations)

      output_storage <- versatile_plot_builder(
        year_1 = year_one,
        office_1 = office_one,
        geomtype = geom_type,
        geomtype_overlay = overlay_geom_type,
        strings = map_labels
      )


      map_render <- output_storage$map
      map_data <- output_storage$dataslot

      # Handling For Plots

      plot_title <- "NA"
      plot_subtitle <- "NA"
      plot_yaxis <- "NA"
      plot_title <- input$chart_title
      plot_subtitle <- input$chart_subtitle
      plot_yaxis <- input$chart_yaxis
      
      force_percentile <- F
      force_percentile <- input$use_percentiles

      if (plot_yaxis == "") {
        plot_yaxis <- "NA"
      }

      if (plot_subtitle == "" | plot_subtitle == "NA") {
        plot_subtitle <- subtitle_handler(geom_type)
      }

      if (plot_title == "") {
        plot_title <- binder(year_one, office_one)
      }

      plot_labels <- c(plot_yaxis, plot_subtitle, plot_title)
      demographic_plot <- demography_plots(map_data, geom_type, plot_labels, compress_swing)
      census_plot <- extra_plots(map_data, geom_type, plot_labels, compress_swing, force_percentile)

      # Render All

      output$Map <- renderPlot(map_render, res = 96, width = 800, height = 800)
      output$DemographyPlot <- renderPlot(demographic_plot, res = 96, width = 800, height = 800)
      output$CensusPlot <- renderPlot(census_plot, res = 96, width = 800, height = 800)
    }
  )

  observeEvent(
    input$runswingplot,
    {
      year_one <- input$year1
      office_one <- input$office1
      year_two <- input$year2
      office_two <- input$office2

      calculation_choice <- "Map by precinct"
      graphic_choice <- "Default"

      calculation_choice <- input$calculation_method
      graphics_choice <- input$graphic_method
      
      compress_swing <- F
      compress_swing <- input$swing_alternate
      
      force_percentile <- F
      force_percentile <- input$use_percentiles


      use_county <- F
      use_county <- if_else(calculation_choice == "Map by county", T, F)
      use_cd <- F
      use_cd <- if_else(calculation_choice == "Map by US Congress district", T, F)
      use_hd <- F
      use_hd <- if_else(calculation_choice == "Map by TX House district", T, F)
      use_sd <- F
      use_sd <- if_else(calculation_choice == "Map by TX Senate district", T, F)
      use_ed <- F
      use_ed <- if_else(calculation_choice == "Map by TX SBOE district", T, F)
      use_rg <- F
      use_rg <- if_else(calculation_choice == "Map by region", T, F)
      map_subtitle <- "NA"
      map_title <- "NA"
      map_annotations <- "NA"
      overlay_cty <- F
      overlay_cty <- if_else(graphics_choice == "Overlay county", T, F)
      overlay_cd <- F
      overlay_cd <- if_else(graphics_choice == "Overlay US Congress districts", T, F)
      hide_state_overlay <- F
      hide_state_overlay <- if_else(graphics_choice == "Hide state border", T, F)
      overlay_hd <- F
      overlay_hd <- if_else(graphics_choice == "Overlay TX House districts", T, F)
      overlay_sd <- F
      overlay_sd <- if_else(graphics_choice == "Overlay TX Senate districts", T, F)
      overlay_ed <- F
      overlay_ed <- if_else(graphics_choice == "Overlay TX SBOE districts", T, F)

      overlay_geom_type <- geom_type_builder(c(
        overlay_cty,
        overlay_cd,
        overlay_ed,
        overlay_hd,
        overlay_sd,
        F,
        hide_state_overlay
      ))



      geom_type <- geom_type_builder(c(use_county, use_cd, use_ed, use_hd, use_sd, use_rg, F))
      map_title <- binder_vs(year_one, office_one, year_two, office_two)
      map_subtitle <- subtitle_handler(geom_type)

      if (input$title != "") {
        map_title <- input$title
      }
      if (input$subtitle != "") {
        map_subtitle <- input$subtitle
      }
      if (input$annotations != "") {
        map_annotations <- input$annotations
      }

      map_labels <- c(map_title, map_subtitle, map_annotations)

      p <- versatile_plot_builder(
        year_1 = year_two,
        office_1 = office_two,
        year_2 = year_one,
        office_2 = office_one,
        geomtype = geom_type,
        geomtype_overlay = overlay_geom_type,
        strings = map_labels
      )

      map_render <- p$map
      map_data <- p$dataslot

      # Handling for Plots

      plot_title <- "NA"
      plot_subtitle <- "NA"
      plot_yaxis <- "NA"
      plot_title <- input$chart_title
      plot_subtitle <- input$chart_subtitle
      plot_yaxis <- input$chart_yaxis

      if (plot_yaxis == "") {
        plot_yaxis <- "NA"
      }

      if (plot_subtitle == "" | plot_subtitle == "NA") {
        plot_subtitle <- subtitle_handler(geom_type)
      }
      plot_subtitle <- paste("\n", plot_subtitle, sep = "")

      if (plot_title == "") {
        plot_title <- binder_vs(year_one, office_one, year_two, office_two)
      }

      plot_election1 <- binder(year_one, office_one)
      plot_election2 <- binder(year_two, office_two)

      plot_labels <- c(plot_yaxis, plot_subtitle, plot_title, plot_election1, plot_election2)
      demographic_plot <- demography_plots(map_data, geom_type, plot_labels, compress_swing)
      census_plot <- extra_plots(map_data, geom_type, plot_labels, compress_swing, force_percentile)

      # All Render

      output$Map <- renderPlot(map_render, res = 96, width = 800, height = 800)
      output$DemographyPlot <- renderPlot(demographic_plot, res = 96, width = 800, height = 800)
      output$CensusPlot <- renderPlot(census_plot, res = 96, width = 800, height = 800)
    }
  )
}
