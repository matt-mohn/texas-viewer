# load election results file (MOVE TO PERMANENT)-------------------------------------
candidate_names <- read.csv("names.csv")
race_id_numbers <- read.csv("election_to_id.csv")
library(sf)

masterkey_handler <- function(master_key) {
  for (i in 1:length(master_key))
  {
    master_key[i] <- paste("n",
      substr(str_to_upper(master_key[i]), 2, nchar(master_key[i])),
      sep = ""
    )
  }
  return(master_key)
}

race_id_numbers <- race_id_numbers |>
  mutate(tag = paste(office, year, sep = "x"))

candidate_names <- candidate_names |>
  mutate(tag = paste(office, year, sep = "x"))

candidate_names_D <- candidate_names |>
  filter(party == "D")

candidate_names_R <- candidate_names |>
  filter(party == "R")

county_ID_data <- read.csv("geography/master_shp_to_county_lookup_table.csv") |>
  data.frame() |>
  mutate(master_key = masterkey_handler(master_key)) |>
  rename("DIST" = "county")

cd_2021_data <- read.csv("geography/master_shp_to_2021_cd_lookup_table.csv") |>
  data.frame() |>
  mutate(master_key = masterkey_handler(master_key))

sd_2021_data <- read.csv("geography/master_shp_to_2021_sd_lookup_table.csv") |>
  data.frame() |>
  mutate(master_key = masterkey_handler(master_key))

hd_2021_data <- read.csv("geography/master_shp_to_2021_hd_lookup_table.csv") |>
  data.frame() |>
  mutate(master_key = masterkey_handler(master_key))

ed_2021_data <- read.csv("geography/master_shp_to_2021_ed_lookup_table.csv") |>
  data.frame() |>
  mutate(master_key = masterkey_handler(master_key))

rg_2021_data <- read.csv("geography/master_shp_to_2021_rg_lookup_table.csv") |>
  data.frame() |>
  mutate(master_key = masterkey_handler(master_key))

census_data <- read.csv("demography/2020_census_for_master_shp.csv") |>
  data.frame() |>
  mutate(master_key = masterkey_handler(master_key))

extra_data <- read.csv("demography/master_shp_2020_extra_lookup_table.csv") |>
  data.frame() |>
  mutate(master_key = masterkey_handler(master_key))

versatile_plot_builder <- function(year_1,
                                   office_1,
                                   year_2 = "NA",
                                   office_2 = "NA",
                                   geomtype = "NA",
                                   geomtype_overlay = "",
                                   inset = "NA",
                                   strings = c("", "", "")) {
  # Establish error handling mechanism
  quit <- F
  quitcode <- ""

  two_track <- F
  dem_name_1 <- ""
  dem_name_2 <- ""
  gop_name_1 <- ""
  gop_name_2 <- ""
  verbatim_dem_name_1 <- ""
  verbatim_gop_name_1 <- ""
  verbatim_dem_name_2 <- ""
  verbatim_gop_name_2 <- ""

  if (office_2 != "NA") {
    two_track <- T
  }


  # Identify the original race (FOR RACE ONE)
  lookup_code_1 <- paste(office_1, year_1, sep = "x")

  race_ID_1 <- race_id_numbers[match(
    lookup_code_1,
    race_id_numbers$tag
  ), 3]

  dem_name_1 <- match(lookup_code_1, candidate_names_D$tag, nomatch = "999")
  if (dem_name_1 == "999") {
    quit <- T
  }


  if (quit == F) {
    verbatim_dem_name_1 <- candidate_names_D$name[dem_name_1]
  }




  gop_name_1 <- match(lookup_code_1, candidate_names_R$tag, nomatch = "999")
  if (gop_name_1 == "999") {
    quit <- T
  }

  if (quit == F) {
    verbatim_gop_name_1 <- candidate_names_R$name[gop_name_1]
  }

  if (quit == F) {
    electoral_data_1 <- read.csv(paste(year_1, "_election_results.csv", sep = "")) |>
      data.frame() |>
      filter(raceID == race_ID_1)
  }

  if (quit == F) {
    # Query original race
    electoral_data_1 <- electoral_data_1 |>
      select(-any_of(c("raceID"))) |>
      mutate(master_key = masterkey_handler(master_key)) # Unused value tabs
  }

  if (quit == F) {
    if (nrow(electoral_data_1) < 2) {
      quit <- T
    }
  }

  # If running successfully, complete pivot
  if (quit == F) {
    electoral_data_1 <- electoral_data_1 |>
      pivot_wider(
        values_from = "votes",
        names_from = "party"
      )
  }

  # Query secondary race (optional)
  if (two_track == T & quit == F) {
    lookup_code_2 <- paste(office_2, year_2, sep = "x")
    race_ID_2 <- race_id_numbers[match(
      lookup_code_2,
      race_id_numbers$tag
    ), 3]

    dem_name_2 <- match(lookup_code_2, candidate_names_D$tag, nomatch = "999")
    if (dem_name_2 == "999") {
      quit <- T
    }

    if (quit == F) {
      verbatim_dem_name_2 <- candidate_names_D$name[dem_name_2]
    }



    gop_name_2 <- match(lookup_code_2, candidate_names_R$tag, nomatch = "999")
    if (gop_name_2 == "999") {
      quit <- T
    }

    if (quit == F) {
      verbatim_gop_name_2 <- candidate_names_R$name[gop_name_2]
    }


    if (quit == F) {
      electoral_data_2 <- read.csv(paste(year_2, "_election_results.csv", sep = "")) |>
        data.frame() |>
        filter(raceID == race_ID_2)
    }

    if (quit == F) {
      electoral_data_2 <- electoral_data_2 |>
        select(-any_of(c("raceID"))) |>
        mutate(master_key = masterkey_handler(master_key)) # Unused value tabs
    }

    if (quit == F) {
      if (nrow(electoral_data_2) < 2) {
        quit <- T
        quitcode <- "Second race not found"
      }
    }

    # If running successful, complete second pivot and merge
    if (quit == F) {
      electoral_data_2 <- electoral_data_2 |>
        pivot_wider(
          values_from = "votes",
          names_from = "party"
        )
    }
  }

  # Add geometry attribute
  if (quit == F) {
    if (geomtype == "NA") {
      # Only do precinct
    }
    if (geomtype != "NA") {
      lookup_table <- lookup_table_lookup(geomtype)
      electoral_data_1 <- electoral_data_1 |>
        left_join(lookup_table,
          by = c("master_key" = "master_key")
        )
      if (two_track == T) {
        electoral_data_2 <- electoral_data_2 |>
          left_join(lookup_table,
            by = c("master_key" = "master_key")
          )
      }
    }
  }

  # Calculate grouped geometries depending on user selection
  if (quit == F) {
    if (geomtype == "NA") {
      key <- "master_key"
    }
    if (geomtype != "NA") {
      key <- "DIST"
    }

    electoral_data_1 <- electoral_data_1 |>
      filter(is.na(D) == F) |>
      filter(is.na(R) == F) |>
      mutate(D = ifelse(D == 0, 0.01, D)) |>
      mutate(R = ifelse(R == 0, 0.01, R))

    if (two_track == T) {
      electoral_data_2 <- electoral_data_2 |>
        filter(is.na(D) == F) |>
        filter(is.na(R) == F) |>
        mutate(D = ifelse(D == 0, 0.01, D)) |>
        mutate(R = ifelse(R == 0, 0.01, R))
    }

    if (geomtype != "NA") {
      electoral_data_1 <- electoral_data_1 |>
        group_by(DIST) |>
        mutate(D = sum(D), R = sum(R)) |>
        distinct(DIST, .keep_all = T)
      if (two_track == T) {
        electoral_data_2 <- electoral_data_2 |>
          group_by(DIST) |>
          mutate(D = sum(D), R = sum(R)) |>
          distinct(DIST, .keep_all = T)
      }
    }

    electoral_data_1 <- electoral_data_1 |>
      mutate("Dem_Share" = D / (D + R))

    if (two_track == T) {
      electoral_data_2 <- electoral_data_2 |>
        mutate("Dem_Share" = D / (D + R))
    }
  }

  # Share merger in two-track model
  if (quit == F & two_track == T) {
    electoral_data_2 <- electoral_data_2 |>
      mutate("Dem_Share_2" = Dem_Share) |>
      select(master_key, Dem_Share_2)

    electoral_data_1 <- electoral_data_1 |>
      left_join(electoral_data_2,
        by = c(key),
        suffix = c("", ".y")
      )

    slot_2 <- electoral_data_1
    electoral_data_1 <- electoral_data_1 |>
      mutate(Dem_Share = Dem_Share - Dem_Share_2)
  }

  if (quit == F & two_track == F) {
    slot_2 <- electoral_data_1
  }

  # Actually add the geography
  if (quit == F) {
    base_shapefile <- query_a_shapefile(geomtype)
  }

  # Compute factors
  if (quit == F) {
    base_shapefile <- base_shapefile |>
      left_join(electoral_data_1,
        by = c(key)
      )

    if (two_track == T) {
      base_shapefile$Dem_Factor <- margin_handler_swing(base_shapefile$Dem_Share)
    }
    if (two_track == F) {
      base_shapefile$Dem_Factor <- margin_handler(base_shapefile$Dem_Share)
    }
  }

  if (quit == F) {
    # First Panel - Add Base Shapefile, if Precinct, Use Special Colour
    if (geomtype == "NA") {
      outputplot <- ggplot() +
        geom_sf(
          data = base_shapefile,
          mapping = aes(
            fill = factor(Dem_Factor,
              levels = margin_levels
            ),
            color = factor(Dem_Factor,
              levels = margin_levels
            )
          ),
          linewidth = 0.25
        ) +
        scale_color_manual(
          name = "",
          breaks = margin_levels,
          values = margin_levels_color,
          drop = F
        )
    }

    if (geomtype != "NA") {
      outputplot <- ggplot() +
        geom_sf(
          data = base_shapefile,
          mapping = aes(
            fill = factor(Dem_Factor,
              levels = margin_levels
            )
          ),
          color = "black",
          linewidth = 0.25
        )
    }

    outputplot <- outputplot +
      scale_fill_manual(
        name = "",
        breaks = margin_levels,
        values = margin_levels_color,
        drop = F
      ) +
      annotate(
        geom = "text",
        label = strings[1],
        x = x_bound(0.75),
        y = y_bound(0.95),
        vjust = 0.5,
        hjust = 0.5,
        fontface = 2,
        size = 4
      ) +
      annotate(
        geom = "text",
        label = strings[2],
        x = x_bound(0.75),
        y = y_bound(0.91),
        vjust = 0.5,
        hjust = 0.5,
        size = 3
      ) +
      annotate(
        geom = "text",
        label = if_else(strings[3] == "NA",
          if_else(two_track == T,
            default_annotation(
              dem_name_one = verbatim_dem_name_1,
              gop_name_one = verbatim_gop_name_1,
              dem_name_two = verbatim_dem_name_2,
              gop_name_two = verbatim_gop_name_2
            ),
            default_annotation(
              dem_name_one = verbatim_dem_name_1,
              gop_name_one = verbatim_gop_name_1
            )
          ),
          strings[3]
        ),
        x = x_bound(0.1),
        y = y_bound(0.87),
        vjust = 0.5,
        hjust = 0.5,
        size = 3
      ) +
      annotate(
        geom = "text",
        label = "TexasElectionViewer",
        x = x_bound(0.95),
        y = y_bound(0.0),
        vjust = 0.5,
        hjust = 0.5,
        size = 3
      ) +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.direction = "horizontal",
        legend.position = c(0, 0),
        legend.justification = c("left", "bottom"),
        plot.background = element_rect(
          fill = "white",
          color = NA
        ),
        panel.background = element_rect(
          fill = "white",
          color = NA
        )
      ) +
      annotate(
        geom = "text",
        label = if_else(two_track == T, "Two-party margin swing", "Two party margin"),
        x = if_else(two_track == T, x_bound(0.08), x_bound(0.05)),
        y = y_bound(0.18),
        vjust = 0.5,
        hjust = 0.5,
        size = 4
      )

    outputplot <- overlay_handler(outputplot, geomtype_overlay, geomtype)

    outputplot <- outputplot +
      coord_sf(
        xlim = c(x_bound(0), x_bound(1)),
        ylim = c(y_bound(0), y_bound(1))
      )
  }

  if (quit == T) {
    slot_2 <- data.frame(cbind(c(0, 1000), c(0, 1000)))
    outputplot <- empty_plot()
    n <- data.frame(cbind(c(0, 1000), c(0, 1000)))
    colnames(n) <- c("a", "b")
    outputplot <- ggplot("Race cannot be plotted.\nOne of the offices selected was either not up for election that year, or\nit was uncontested by Democrats and/or Republicans.\nMost 2004 data is unavailable.")
  }

  output_storage <- list(
    "map" = outputplot,
    "dataslot" = slot_2
  )

  return(output_storage)
}

# Generate demography plots
demography_plots <- function(datum, geomtype, calls, compress_swing) {
  instant_failure_condition <- F
  if (nrow(datum) == 2) {
    instant_failure_condition <- T
  }

  if (instant_failure_condition == F) {
    # Compress and group for Census
    if (geomtype == "NA") # Precinct
      {
        census_data_local <- census_data |>
          select(master_key, vap, anglovap, nanglovap, asianvap, blackvap, hispvap) |>
          mutate(
            anglo_pct = anglovap / vap,
            non_anglo_pct = nanglovap / vap,
            asian_pct = asianvap / vap,
            black_pct = blackvap / vap,
            hispanic_pct = hispvap / vap
          )

        datum <- datum |>
          filter(is.na("master_key") == F) |>
          select(starts_with("Dem_Share") | starts_with("master_key")) |>
          left_join(census_data_local, by = "master_key") |>
          filter(vap != 0)
      }
    if (geomtype != "NA") # All districts
      {
        district_panel <- lookup_table_lookup(geomtype)
        census_data_local <- census_data |>
          left_join(district_panel, by = "master_key") |>
          filter(is.na("DIST") == F) |>
          select(DIST, vap, anglovap, nanglovap, asianvap, blackvap, hispvap) |>
          group_by(DIST) |>
          mutate(
            vap = sum(vap),
            anglovap = sum(anglovap),
            nanglovap = sum(nanglovap),
            asianvap = sum(asianvap),
            blackvap = sum(blackvap),
            hispvap = sum(hispvap)
          ) |>
          mutate(
            anglo_pct = anglovap / vap,
            non_anglo_pct = nanglovap / vap,
            asian_pct = asianvap / vap,
            black_pct = blackvap / vap,
            hispanic_pct = hispvap / vap
          )

        census_data_local <- census_data_local |>
          distinct(DIST, .keep_all = T)

        datum <- datum |>
          filter(is.na("DIST") == F) |>
          select(starts_with("Dem_Share") | starts_with("DIST")) |>
          left_join(census_data_local, by = "DIST")
      }

    instant_quit_flag <- F

    if (compress_swing == T) {
      datum <- datum |>
        filter(is.na("Dem_Share") == F) |>
        filter(is.na("Dem_Share_2") == F) |>
        mutate(Dem_Share = Dem_Share - Dem_Share_2) |>
        select(-Dem_Share_2)

      if (calls[1] == "NA") {
        calls[1] <- "Change in Democratic two-party margin between races"
      }
    }

    if ("Dem_Share_2" %in% colnames(datum) == T) {
      instant_quit_flag <- T
      datum <- datum |>
        rename(
          race_2 = Dem_Share,
          race_1 = Dem_Share_2
        )

      count <- nrow(datum)
      datum <- datum |>
        pivot_longer(
          cols = starts_with("race"),
          names_to = "Race",
          values_to = "Share"
        ) |>
        pivot_longer(
          cols = ends_with("pct"),
          names_to = "Ethnicity",
          values_to = "Race_Share"
        )

      datum <- transform_mapping_plot_2(datum, calls[4], calls[5])

      demography_plot <- ggplot(
        data = datum,
        mapping = aes(x = Race_Share, y = Share, color = Race)
      ) +
        geom_hline(yintercept = 0, linewidth = 0.35, color = if_else(compress_swing == T, "black", NA)) +
        geom_point(alpha = alpha_handler(count)) +
        geom_smooth() +
        facet_wrap("Ethnicity", nrow = 3, scales = "free_x") +
        scale_x_continuous(labels = scales::percent) +
        scale_y_continuous(labels = scales::percent) +
        labs(
          x = "Share of voting-age population in 2020 census",
          y = if_else(calls[1] == "NA", "Democratic share of two-party vote", calls[1]),
          subtitle = calls[2],
          title = calls[3],
          caption = "TexasElectionViewer"
        )

      demography_plot <- plot_wrapper(demography_plot)
    }

    if ("Dem_Share_2" %in% colnames(datum) == F & instant_quit_flag == F) {
      count <- nrow(datum)

      datum <- datum |>
        pivot_longer(
          cols = ends_with("pct"),
          names_to = "Ethnicity",
          values_to = "Race_Share"
        )

      datum <- datum |> transform_mapping_plot_1()

      range <- quantile(datum$Dem_Share, .98)[[1]] - quantile(datum$Dem_Share, .02)[[1]]
      if (range < 0.5) {
        datum <- datum |>
          filter(Dem_Share > quantile(datum$Dem_Share, .02)[[1]]) |>
          filter(Dem_Share < quantile(datum$Dem_Share, .98)[[1]])
      }

      demography_plot <- ggplot(
        data = datum,
        mapping = aes(x = Race_Share, y = Dem_Share)
      ) +
        geom_hline(yintercept = 0, linewidth = 0.35, color = if_else(compress_swing == T, "black", NA)) +
        geom_point(alpha = alpha_handler(count)) +
        geom_smooth() +
        facet_wrap("Ethnicity", nrow = 3, scales = "free_x") +
        scale_x_continuous(labels = scales::percent) +
        scale_y_continuous(labels = scales::percent) +
        labs(
          x = "Share of voting-age population in 2020 census",
          y = if_else(calls[1] == "NA", "Democratic share of two-party vote", calls[1]),
          subtitle = calls[2],
          title = calls[3],
          caption = "TexasElectionViewer"
        )

      demography_plot <- plot_wrapper(demography_plot)
    }
  }

  if (instant_failure_condition == T) {
    demography_plot <- empty_plot("Missing info.")
  }
  return(demography_plot)
}

# Generate demography plots
extra_plots <- function(datum, geomtype, calls, compress_swing, force_percentile) {
  instant_failure_condition <- F
  if (nrow(datum) == 2) {
    instant_failure_condition <- T
  }

  if (instant_failure_condition == F) {
    # Compress and group for Census
    if (geomtype == "NA") # Precinct
      {
        extra_data_local <- extra_data |>
          select(master_key, pop_above_25, adv_degree, median_income, density) |> # Now apply weights
          mutate(
            adv_degree = adv_degree / pop_above_25
          )

        extra_data_local <- extra_data_local |>
          na.omit()

        datum <- datum |>
          filter(is.na("master_key") == F) |>
          select(starts_with("Dem_Share") | starts_with("master_key")) |>
          left_join(extra_data_local, by = "master_key") |>
          na.omit()
      }

    if (geomtype != "NA") # All districts
      {
        district_panel <- lookup_table_lookup(geomtype)

        extra_data_local <- extra_data |>
          left_join(district_panel, by = "master_key") |>
          filter(is.na("DIST") == F) |>
          select(DIST, pop_above_25, adv_degree, median_income, density) |> # Now apply weights
          mutate(
            median_income_wt = median_income * pop_above_25,
            density_wt = density * pop_above_25
          ) |>
          group_by(DIST) |>
          mutate(
            pop_above_25 = sum(pop_above_25),
            adv_degree = sum(adv_degree),
            median_income_wt = sum(median_income_wt),
            density_wt = sum(density_wt)
          ) |>
          mutate(
            median_income = median_income_wt / pop_above_25,
            density = density_wt / pop_above_25,
            adv_degree = adv_degree / pop_above_25
          )

        extra_data_local <- extra_data_local |>
          distinct(DIST, .keep_all = T) |>
          na.omit()

        datum <- datum |>
          filter(is.na("DIST") == F) |>
          select(starts_with("Dem_Share") | starts_with("DIST")) |>
          left_join(extra_data_local, by = "DIST") |>
          na.omit()
      }

    instant_quit_flag <- F

    if (compress_swing == T) {
      datum <- datum |>
        filter(is.na("Dem_Share") == F) |>
        filter(is.na("Dem_Share_2") == F) |>
        mutate(Dem_Share = Dem_Share - Dem_Share_2) |>
        select(-Dem_Share_2)

      if (calls[1] == "NA") {
        calls[1] <- "Change in Democratic two-party margin between races"
      }
    }

    if ("Dem_Share_2" %in% colnames(datum) == T) {
      instant_quit_flag <- T
      datum <- datum |>
        rename(
          race_2 = Dem_Share,
          race_1 = Dem_Share_2
        )

      count <- nrow(datum)

      datum <- datum |>
        filter(is.na(density) == F)

      if (force_percentile == T) {
        datum$density <- weighted_percentile(datum$density, 
                                             datum$pop_above_25)
        datum$median_income <- weighted_percentile(datum$median_income, 
                                                   datum$pop_above_25)
        datum$adv_degree <- weighted_percentile(datum$adv_degree, 
                                                datum$pop_above_25)
      }

      datum <- datum |>
        pivot_longer(
          cols = starts_with("race"),
          names_to = "Race",
          values_to = "Share"
        ) |>
        pivot_longer(
          cols = c("median_income", "density", "adv_degree"),
          names_to = "Class",
          values_to = "Value"
        ) |>
        filter(is.na("Value") == F)

      datum <- na.omit(datum)

      datum <- transform_mapping_plot_4(datum, calls[4], calls[5])

      if (force_percentile == T) {
        datum$Class <- paste(datum$Class, " - Population-weighted percentile", sep = "")
      }

      census_plot <- ggplot(
        data = datum,
        mapping = aes(x = Value, y = Share, color = Race)
      ) +
        geom_hline(yintercept = 0, linewidth = 0.35, 
                   color = if_else(compress_swing == T, "black", NA)) +
        geom_point(alpha = alpha_handler(count)) +
        geom_smooth() +
        facet_wrap("Class", nrow = 3, scales = "free_x") +
        scale_x_continuous() +
        scale_y_continuous(labels = scales::percent) +
        labs(
          x = "2020 Census data",
          y = if_else(calls[1] == "NA", "Democratic share of two-party vote", calls[1]),
          subtitle = calls[2],
          title = calls[3],
          caption = "TexasElectionViewer"
        )

      census_plot <- plot_wrapper(census_plot)
    }

    if ("Dem_Share_2" %in% colnames(datum) == F & instant_quit_flag == F) {
      count <- nrow(datum)

      if (force_percentile == T) {
        datum$density <- weighted_percentile(datum$density, 
                                             datum$pop_above_25)
        datum$median_income <- weighted_percentile(datum$median_income, 
                                                   datum$pop_above_25)
        datum$adv_degree <- weighted_percentile(datum$adv_degree, 
                                                datum$pop_above_25)
      }

      datum <- datum |>
        pivot_longer(
          cols = c("median_income", "density", "adv_degree"),
          names_to = "Class",
          values_to = "Value"
        ) |>
        filter(is.na("Value") == F)

      datum <- na.omit(datum)

      datum <- datum |> transform_mapping_plot_3()

      range <- quantile(datum$Dem_Share, .98)[[1]] - quantile(datum$Dem_Share, .02)[[1]]
      if (range < 0.5) {
        datum <- datum |>
          filter(Dem_Share > quantile(datum$Dem_Share, .02)[[1]]) |>
          filter(Dem_Share < quantile(datum$Dem_Share, .98)[[1]])
      }

      if (force_percentile == T) {
        datum$Class <- paste(datum$Class, " - Population-weighted percentile", sep = "")
      }

      census_plot <- ggplot(
        data = datum,
        mapping = aes(x = Value, y = Dem_Share)
      ) +
        geom_hline(yintercept = 0, linewidth = 0.35, 
                   color = if_else(compress_swing == T, "black", NA)) +
        geom_point(alpha = alpha_handler(count)) +
        geom_smooth() +
        facet_wrap("Class", nrow = 3, scales = "free_x") +
        scale_x_continuous() +
        scale_y_continuous(labels = scales::percent) +
        labs(
          x = "2020 Census data",
          y = if_else(calls[1] == "NA", "Democratic share of two-party vote", calls[1]),
          subtitle = calls[2],
          title = calls[3],
          caption = "TexasElectionViewer"
        )

      census_plot <- plot_wrapper(census_plot)
    }
  }
  if (instant_failure_condition == T) {
    census_plot <- empty_plot("Missing info.")
  }
  return(census_plot)
}

transform_mapping_plot_2 <- function(datum, election1, election2) {
  # Change race share
  # Change race values
  datum <- datum |>
    mutate(Ethnicity = if_else(Ethnicity == "anglo_pct",
                               "White", Ethnicity)) |>
    mutate(Ethnicity = if_else(Ethnicity == "asian_pct",
                               "Asian American / Pacific Islander", Ethnicity)) |>
    mutate(Ethnicity = if_else(Ethnicity == "black_pct",
                               "African American", Ethnicity)) |>
    mutate(Ethnicity = if_else(Ethnicity == "hispanic_pct",
                               "Hispanic", Ethnicity)) |>
    mutate(Ethnicity = if_else(Ethnicity == "non_anglo_pct",
                               "Non-White", Ethnicity)) |>
    mutate(Race = if_else(Race == "race_1", election1, Race)) |>
    mutate(Race = if_else(Race == "race_2", election2, Race))

  return(datum)
}

transform_mapping_plot_1 <- function(datum) {
  # Change race share
  # Change race values
  datum <- datum |>
    mutate(Ethnicity = if_else(Ethnicity == "anglo_pct",
                               "White", Ethnicity)) |>
    mutate(Ethnicity = if_else(Ethnicity == "asian_pct",
                               "Asian American / Pacific Islander", Ethnicity)) |>
    mutate(Ethnicity = if_else(Ethnicity == "black_pct",
                               "African American", Ethnicity)) |>
    mutate(Ethnicity = if_else(Ethnicity == "hispanic_pct",
                               "Hispanic", Ethnicity)) |>
    mutate(Ethnicity = if_else(Ethnicity == "non_anglo_pct",
                               "Non-White", Ethnicity))

  return(datum)
}

transform_mapping_plot_3 <- function(datum) {
  # Change race share
  # Change race values
  datum <- datum |>
    mutate(Class = if_else(Class == "adv_degree",
                           "Pop. over 25 with bachelor's degree or higher", Class)) |>
    mutate(Class = if_else(Class == "median_income",
                           "Median household income ($)", Class)) |>
    mutate(Class = if_else(Class == "density",
                           "Density", Class))

  return(datum)
}

transform_mapping_plot_4 <- function(datum, election1, election2) {
  # Change race share
  # Change race values
  datum <- datum |>
    mutate(Class = if_else(Class == "adv_degree",
                           "Pop. over 25 with bachelor's degree or higher", Class)) |>
    mutate(Class = if_else(Class == "median_income",
                           "Median household income ($)", Class)) |>
    mutate(Class = if_else(Class == "density",
                           "Density", Class)) |>
    mutate(Race = if_else(Race == "race_1", election1, Race)) |>
    mutate(Race = if_else(Race == "race_2", election2, Race))

  return(datum)
}


alpha_handler <- function(count) {
  a <- 1

  if (count > 1) {
    a <- 0.3
  }
  if (count > 100) {
    a <- 0.15
  }
  if (count > 200) {
    a <- 0.1
  }
  if (count > 300) {
    a <- 0.05
  }

  return(a)
}



margin_handler <- function(value_vector) {
  return_vector <- c()
  for (i in 1:length(value_vector))
  {
    m <- value_vector[i]
    n <- NA
    # m should be D vote share
    if (is.na(m)) {
      m <- -1.5
    }

    if (m > -1.4) {
      m <- 2 * m - 1
    }

    if (m > -1.6) {
      n <- "NA"
    }
    if (m >= -1) {
      n <- "R >80%"
    }
    if (m > -.8) {
      n <- "R >60%"
    }
    if (m > -.6) {
      n <- "R >40%"
    }
    if (m > -.4) {
      n <- "R >30%"
    }
    if (m > -.3) {
      n <- "R >25%"
    }
    if (m > -.25) {
      n <- "R >20%"
    }
    if (m > -.2) {
      n <- "R >15%"
    }
    if (m > -.15) {
      n <- "R >10%"
    }
    if (m > -.1) {
      n <- "R >5%"
    }
    if (m > -.05) {
      n <- "R >0%"
    }
    if (m > 0) {
      n <- "D >0%"
    }
    if (m > 0.05) {
      n <- "D >5%"
    }
    if (m > 0.1) {
      n <- "D >10%"
    }
    if (m > 0.15) {
      n <- "D >15%"
    }
    if (m > 0.2) {
      n <- "D >20%"
    }
    if (m > 0.25) {
      n <- "D >25%"
    }
    if (m > 0.3) {
      n <- "D >30%"
    }
    if (m > 0.4) {
      n <- "D >40%"
    }
    if (m > 0.6) {
      n <- "D >60%"
    }
    if (m > 0.8) {
      n <- "D >80%"
    }
    return_vector[i] <- n
  }
  return(return_vector)
}


margin_handler_swing <- function(value_vector) {
  return_vector <- c()
  for (i in 1:length(value_vector))
  {
    m <- value_vector[i]
    n <- NA
    # m should be D vote share
    if (is.na(m)) {
      m <- -2.5
    }

    m <- m * 2

    if (m > -2.4) {
      m <- m
    }

    if (m > -2.4) {
      n <- "NA"
    }
    if (m >= -1) {
      n <- "R >80%"
    }
    if (m > -.8) {
      n <- "R >60%"
    }
    if (m > -.6) {
      n <- "R >40%"
    }
    if (m > -.4) {
      n <- "R >30%"
    }
    if (m > -.3) {
      n <- "R >25%"
    }
    if (m > -.25) {
      n <- "R >20%"
    }
    if (m > -.2) {
      n <- "R >15%"
    }
    if (m > -.15) {
      n <- "R >10%"
    }
    if (m > -.1) {
      n <- "R >5%"
    }
    if (m > -.05) {
      n <- "R >0%"
    }
    if (m > 0) {
      n <- "D >0%"
    }
    if (m > 0.05) {
      n <- "D >5%"
    }
    if (m > 0.1) {
      n <- "D >10%"
    }
    if (m > 0.15) {
      n <- "D >15%"
    }
    if (m > 0.2) {
      n <- "D >20%"
    }
    if (m > 0.25) {
      n <- "D >25%"
    }
    if (m > 0.3) {
      n <- "D >30%"
    }
    if (m > 0.4) {
      n <- "D >40%"
    }
    if (m > 0.6) {
      n <- "D >60%"
    }
    if (m > 0.8) {
      n <- "D >80%"
    }
    return_vector[i] <- n
  }
  return(return_vector)
}

margin_levels <- c(
  "D >80%", "D >60%", "D >40%",
  "D >30%", "D >25%", "D >20%",
  "D >15%", "D >10%", "D >5%",
  "D >0%", "R >0%", "R >5%",
  "R >10%", "R >15%", "R >20%",
  "R >25%", "R >30%", "R >40%",
  "R >60%", "R >80%", "NA"
)

margin_levels_color <- c(
  "#111C3B", "#152452", "#172C6B",
  "#1C378A", "#2949AB", "#3359CC",
  "#4971EB", "#678CFA", "#ABC0FF",
  "#D6E4FF", "#FFD6E1", "#FFABBD",
  "#FA677F", "#EB4964", "#CC334D",
  "#AB293F", "#8A1C2E", "#6B1725",
  "#52151F", "#3B1118", "#C4C4C4"
)

y_bound <- function(x) {
  return((x * (1594958 - 412842)) + 412842)
}

x_bound <- function(x) {
  return((x * (1618132 - 372991)) + 372991)
}

subtitle_handler <- function(geom_type) {
  if (geom_type == "NA") {
    latter_string <- "by Texas precinct"
  }

  if (geom_type == "CD2021") {
    latter_string <- "by Texas 2021 congressional district"
  }

  if (geom_type == "SD2021") {
    latter_string <- "by Texas 2021 State Senate district"
  }

  if (geom_type == "HD2021") {
    latter_string <- "by Texas 2021 State House district"
  }

  if (geom_type == "ED2021") {
    latter_string <- "by Texas 2021 SBOE district"
  }

  if (geom_type == "COUNTY") {
    latter_string <- "by Texas county"
  }

  if (geom_type == "RG2021") {
    latter_string <- "by Texas region"
  }
  return(latter_string)
}

geom_type_builder <- function(flags) {
  geom_type <- "NA"
  if (flags[1] == T) {
    geom_type <- "COUNTY"
  }
  if (flags[2] == T) {
    geom_type <- "CD2021"
  }
  if (flags[3] == T) {
    geom_type <- "ED2021"
  }
  if (flags[4] == T) {
    geom_type <- "HD2021"
  }
  if (flags[5] == T) {
    geom_type <- "SD2021"
  }
  if (flags[6] == T) {
    geom_type <- "RG2021"
  }
  if (flags[7] == T) {
    geom_type <- "HIDESTATE"
  }

  return(geom_type)
}

lookup_table_lookup <- function(geomtype) {
  if (geomtype == "CD2021") {
    data_fetch <- cd_2021_data
  }
  if (geomtype == "SD2021") {
    data_fetch <- sd_2021_data
  }
  if (geomtype == "RG2021") {
    data_fetch <- rg_2021_data
  }
  if (geomtype == "COUNTY") {
    data_fetch <- county_ID_data
  }
  if (geomtype == "HD2021") {
    data_fetch <- hd_2021_data
  }
  if (geomtype == "ED2021") {
    data_fetch <- ed_2021_data
  }

  return(data_fetch)
}

binder <- function(a, b) {
  c <- paste(a, " ", b, sep = "")
  return(c)
}

binder_vs <- function(a, b, c, d) {
  e <- paste(a, " ", b, " v.\n", c, " ", d, sep = "")
  return(e)
}

query_a_shapefile <- function(geomtype) {
  if (geomtype == "NA") # Precinct
    {
      base_shapefile <- st_read("geography/coastal/master_shapefile_coastal_2022/master_shapefile_coastal_2022.shp",
        quiet = T
      )
    }
  if (geomtype == "CD2021") {
    base_shapefile <- cd_2021_polygon <- st_read("geography/coastal/congressional_districts_2021/congressional_districts_2021.shp",
      quiet = T
    )
  }
  if (geomtype == "HD2021") {
    base_shapefile <- st_read("geography/coastal/house_districts_2021/house_districts_2021.shp",
      quiet = T
    )
  }
  if (geomtype == "SD2021") {
    base_shapefile <- st_read("geography/coastal/senate_districts_2021/senate_districts_2021.shp",
      quiet = T
    )
  }
  if (geomtype == "ED2021") {
    base_shapefile <- st_read("geography/coastal/education_districts_2021/education_districts_2021.shp",
      quiet = T
    )
  }
  if (geomtype == "RG2021") {
    base_shapefile <- st_read("geography/coastal/texas_regions_2021/texas_regions_2021.shp",
      quiet = T
    )
  }
  if (geomtype == "COUNTY") {
    base_shapefile <- st_read("geography/coastal/county_coastal_2022/county_coastal_2022.shp",
      quiet = T
    )
    base_shapefile$DIST <- base_shapefile$county_nam
  }

  if (geomtype == "STATE") {
    base_shapefile <- st_read("geography/coastal/state_polygon_2022/state_polygon_coast.shp",
      quiet = T
    )
  }
  return(base_shapefile)
}

overlay_handler <- function(outputplot, geomtype_overlay, geomtype) {
  escape <- F
  if (geomtype_overlay == "NA") {
    escape <- T # NO OVERLAY, YES STATE OUTLINE

    state_polygon <- query_a_shapefile("STATE")

    outputplot <- outputplot + geom_sf(
      data = state_polygon,
      color = "black",
      fill = NA,
      linewidth = 0.5
    )
  }
  if (geomtype_overlay == "HIDESTATE") {
    escape <- T # NO OVERLAY, NO STATE OUTLINE
  }

  if (geomtype != geomtype_overlay & escape == F) {
    addnl_polygon <- query_a_shapefile(geomtype_overlay)
    state_polygon <- query_a_shapefile("STATE")

    outputplot <- outputplot + geom_sf(
      data = state_polygon,
      color = "black",
      fill = NA,
      linewidth = 0.5
    ) + geom_sf(
      data = addnl_polygon,
      color = "black",
      alpha = 0.5,
      fill = NA,
      linewidth = 0.5
    )
  }

  return(outputplot)
}

empty_plot <- function(label_plot) {
  n <- data.frame(cbind(c(0, 1000), c(0, 1000)))
  colnames(n) <- c("a", "b")
  ggplot() +
    geom_point(data = n, mapping = aes(x = a, y = b)) +
    annotate(
      geom = "text",
      label = label_plot,
      x = 500,
      y = 500,
      vjust = 0.5,
      hjust = 0.5,
      size = 6
    ) +
    theme_void()
}

plot_wrapper <- function(plot) {
  plot <- plot +
    theme_minimal() +
    theme(
      panel.background = element_rect(
        fill = "#F1F1F1",
        color = "#F1F1F1"
      ),
      panel.grid.major = element_line(color = "#CCCCCC"),
      panel.grid.minor = element_blank(),
      strip.background =
        element_rect(
          fill = "#E5E5E5",
          color = "#E5E5E5"
        ),
      plot.title = element_text(face = "bold")
    )
  return(plot)
}

default_annotation <- function(dem_name_one = "NA",
                               gop_name_one = "NA",
                               dem_name_two = "NA",
                               gop_name_two = "NA") {
  # One-way
  if (dem_name_two == "NA") {
    annotation <- paste(dem_name_one, " (D) v. ",
                        gop_name_one, " (R)", sep = "")
  }
  # Two-way
  if (dem_name_two != "NA") {
    annotation <- paste(dem_name_two, " (D) v. ",
                        gop_name_two, " (R)", "\n", "to →", "\n",
                        dem_name_one, " (D) v. ", gop_name_one, " (R)",
                        sep = ""
    )
  }
  return(annotation)
}

weighted_percentile <- function(target_vector, weight_vector) {
  return_vector <- target_vector
  tot_wt <- sum(weight_vector)

  for (i in 1:length(target_vector))
  {
    balance <- 0
    for (j in 1:length(target_vector))
    {
      if (target_vector[j] <= target_vector[i]) {
        balance <- balance + weight_vector[j]
      }
    }
    return_vector[i] <- balance
  }

  return_vector <- return_vector / tot_wt
  return(return_vector)
}
