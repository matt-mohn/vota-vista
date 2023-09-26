library(shiny)
library(readxl)
library(dplyr)
library(tidyverse)
library(tigris)
library(shinycssloaders)
library(sf)
library(viridis)
library(ggparliament)
library(DescTools)
library(scales)


national_percenter <- function(frame) {
  output_frame <- frame
  for (i in 1:nrow(output_frame))
  {
    output_frame[i, ncol(frame) + 1] <- sum(frame[i, 1:ncol(frame)])
  }
  for (i in 1:nrow(output_frame))
  {
    for (z in 1:ncol(frame))
    {
      output_frame[i, z] <- output_frame[i, z] / output_frame[i, ncol(output_frame)]
    }
  }
  output_frame <- output_frame[, 1:(ncol(output_frame) - 1)]


  return(output_frame)
}

precinct_percenter <- function(frame) {
  output_frame <- frame
  for (i in 1:nrow(output_frame))
  {
    output_frame[i, ncol(frame) + 1] <- sum(frame[i, 4:ncol(frame)])
  }
  for (i in 1:nrow(output_frame))
  {
    for (z in 4:ncol(frame))
    {
      output_frame[i, z] <- output_frame[i, z] / output_frame[i, ncol(output_frame)]
    }
  }
  output_frame <- output_frame[, 1:(ncol(output_frame) - 1)]
  output_frame <- output_frame |> select(-major)
  return(output_frame)
}

major_percenter <- function(frame) {
  output_frame <- frame |> data.frame()
  for (i in 1:nrow(output_frame))
  {
    output_frame[i, ncol(frame) + 1] <- sum(frame[i, 4:ncol(frame)])
  }
  for (i in 1:nrow(output_frame))
  {
    for (z in 4:ncol(frame))
    {
      output_frame[i, z] <- output_frame[i, z] / output_frame[i, ncol(output_frame)]
    }
  }
  output_frame <- output_frame[, 1:(ncol(output_frame) - 1)]
  return(output_frame)
}

winner_ID <- function(frame) {
  output_frame <- frame
  header <- colnames(output_frame)
  exporting_line <- header[1]
  orig_end <- ncol(output_frame)
  for (i in 1:nrow(output_frame))
  {
    max_val <- max(output_frame[i, 4:orig_end])
    index <- match(max_val, output_frame[i, 1:orig_end])
    title <- header[index]
    output_frame[i, orig_end + 1] <- max_val
    output_frame[i, orig_end + 2] <- title
  }
  output_frame <- output_frame[, c(1, orig_end + 1, orig_end + 2)]
  colnames(output_frame) <- c(exporting_line, "share", "winner")
  return(output_frame)
}

dhondt <- function(frame, seats, threshold) {
  frame <- data.frame(frame)
  colnames(frame) <- c("id", "value")
  seats <- as.integer(seats)
  threshold <- as.numeric(threshold)
  frame$value <- as.numeric(frame$value)
  total_value <- sum(frame$value)
  quota <- total_value * threshold

  frame <- frame |>
    filter(value >= quota)

  quit <- F
  runtime <- 0
  while (quit == F) {
    runtime <- runtime + 1
    divisor <- ncol(frame) - 2 + 1
    frame[, ncol(frame) + 1] <- frame[, 2] / divisor

    if (runtime >= seats) {
      quit <- T
    }
  }

  seat_count <- min(nrow(frame) * (ncol(frame) - 2), seats)
  minimum_vote <- Large(frame[, 3:ncol(frame)], seat_count)
  minimum_vote <- min(minimum_vote)

  # Sum Across Rows
  frame[, ncol(frame) + 1] <- 0
  for (i in 1:nrow(frame))
  {
    for (z in 3:(ncol(frame) - 1))
    {
      if (frame[i, z] >= minimum_vote) {
        frame[i, ncol(frame)] <- frame[i, ncol(frame)] + 1
      }
    }
  }

  frontend <- frame[, 1:2]
  backend <- frame[, ncol(frame)]
  frame <- cbind(frontend, backend)
  colnames(frame) <- c("id", "value", "seats")
  return(frame)
}

# Feed data through D'Hondt algorithm
nationwide <- function(frame) {
  frame <- frame |> pivot_longer(
    cols = -any_of(c("major", "minor", "seats")),
    values_to = "votes",
    names_to = "party"
  )

  # Build unique minor list
  constituencies <- unique(frame$minor)

  # Build a minor set
  for (i in 1:length(constituencies)) {
    name <- constituencies[i]

    sub_frame <- frame |> filter(minor == name)
    seats <- mean(sub_frame$seats)
    sub_frame <- sub_frame |> select(party, votes)
    distributed <- dhondt(sub_frame, seats, 0.03)
    distributed$const <- name
    distributed <- distributed |>
      select(id, seats, value, const)

    if (i == 1) {
      collector <- distributed
    }
    if (i != 1) {
      collector <- rbind(collector, distributed)
    }
  }
  colnames(collector) <- c("party", "seats", "value", "minor")
  return(collector)
}

# Returns national seat totals for hemisphere plot
collector_summary <- function(frame) {
  frame <- frame |>
    group_by(party) |>
    summarise(seats = sum(seats))
  return(frame)
}

compressor <- function(frame, parties, other_code) {
  winner_list <- frame$winner

  for (i in 1:length(winner_list))
  {
    if (winner_list[i] %in% parties == F) {
      winner_list[i] <- other_code
    }
  }

  frame$winner <- winner_list
  return(frame)
}

percent_vote_table <- function(pct_frame, vote_frame) {
  transform_seats <- t(pct_frame) |>
    data.frame() |>
    tibble::rownames_to_column("party")

  colnames(transform_seats) <- c("party", "pct")

  transform_seats2 <- t(vote_frame) |>
    data.frame() |>
    tibble::rownames_to_column("party")
  colnames(transform_seats2) <- c("party", "votes")

  output_frame <- left_join(transform_seats, transform_seats2, by = "party")

  return(output_frame)
}

national_manipulator <- function(frame, target_names, target_modifiers) {
  header <- colnames(frame)

  for (i in 1:length(target_names))
  {
    target_party <- target_names[i]
    target_share <- target_modifiers[i]
    if (target_party %in% header == T) {
      frame[target_party] <- frame[target_party] * target_share
    }
  }

  return(frame)
}

# ------------------------------------------------------------------------------------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(
    HTML('
         #sidebar {
            background-color: #ffffff;
        }

        body, label, input, button, select {
          font-family: "Arial";
        }')
  )),

  # Application title
  titlePanel(""),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      id = "sidebar",
      plotOutput("SEMI_Plot", inline = T),
      DT::dataTableOutput("DATA_TABLE"),
      h5(),
      sliderInput("obs_A", "Loading from language_config",
        min = 0, max = 200, value = 100, step = 0.1
      ),
      sliderInput("obs_B", "Loading from language_config",
        min = 0, max = 200, value = 100, step = 0.1
      ),
      sliderInput("obs_C", "Loading from language_config",
        min = 0, max = 200, value = 100, step = 0.1
      ),
      sliderInput("obs_D", "Loading from language_config",
        min = 0, max = 200, value = 100, step = 0.1
      ),
      sliderInput("obs_E", "Loading from language_config",
        min = 0, max = 200, value = 100, step = 0.1
      ),
      actionButton(
        "launch_year_1",
        "LISTO - 2019"
      ),
      actionButton(
        "launch_year_2",
        "LISTO - 2023"
      ),
      actionButton(
        "set_to_polls",
        "ENCUESTAS"
      ),
      actionButton(
        "reset",
        "RESTABLECER"
      ),
      actionButton(
        "info",
        "INFO"
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("major_plot", inline = T),
      plotOutput("minor_plot", inline = T),
      div(DT::dataTableOutput("minor_info_table"), style = "width: 60%"),
      h5(" "),
      h5(" ")
    )
  )
)

##############################################################################################################################################################################################

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  major_shapefile <- st_read("geography/major.shp",
    quiet = T
  )

  minor_shapefile <- st_read("geography/minor.shp",
    quiet = T
  )

  results_2019 <- read.csv("noviembre_2019.csv")

  party_db <- read.csv("party_list.csv")

  language <- read.csv("language_config.csv")
  LANG.party <- language[1, 2]
  LANG.seats <- language[2, 2]
  LANG.before <- language[3, 2]
  LANG.after <- language[4, 2]
  LANG.pct <- language[5, 2]
  LANG.changevoteof <- language[6, 2]
  LANG.go <- language[7, 2]
  LANG.polls <- language[8, 2]
  LANG.reset <- language[9, 2]
  LANG.info <- language[10, 2]
  LANG.minor <- language[11, 2]
  LANG.major <- language[12, 2]
  LANG.partymostvoted <- language[13, 2]
  LANG.year1 <- language[14, 2]
  LANG.year2 <- language[15, 2]
  LANG.relativetoitsvote <- language[16, 2]
  LANG.others <- language[17, 2]
  LANG.votes <- language[18, 2]
  LANG.search <- language[19, 2]
  LANG.message1 <- language[20, 2]
  LANG.message2 <- language[21, 2]
  LANG.nouse <- language[22, 2]
  print(LANG.others)

  slider_details <- read.csv("slider_config.csv")
  colnames(slider_details) <- c("label", "first_ID", "second_ID", "polling_avg")
  if (nrow(slider_details) > 5) {
    slider_details <- head(slider_details, 5)
  }
  orig_IDs_1 <- slider_details$first_ID
  orig_IDs_2 <- slider_details$second_ID
  if (nrow(slider_details) < 5) {
    diff_length <- 5 - nrow(slider_details)
    for (i in 1:diff_length) {
      slider_details <- add_row(slider_details, label = LANG.nouse, first_ID = " ", second_ID = " ", polling_avg = 100)
    }
  }
  return_names <- slider_details$label
  return_polls <- slider_details$polling_avg

  updateActionButton(
    session = session,
    inputId = "obs_A",
    label = paste0(LANG.changevoteof, " ", return_names[1], " ", LANG.relativetoitsvote, " ", LANG.year1)
  )

  updateActionButton(
    session = session,
    inputId = "obs_B",
    label = paste0("..", return_names[2])
  )

  updateActionButton(
    session = session,
    inputId = "obs_C",
    label = paste0("..", return_names[3])
  )

  updateActionButton(
    session = session,
    inputId = "obs_D",
    label = paste0("..", return_names[4])
  )

  updateActionButton(
    session = session,
    inputId = "obs_E",
    label = paste0("..", return_names[5])
  )

  updateActionButton(
    session = session,
    inputId = "launch_year_1",
    label = paste0(LANG.go, " → ", LANG.year1)
  )

  updateActionButton(
    session = session,
    inputId = "launch_year_2",
    label = paste0(LANG.go, " → ", LANG.year2)
  )

  updateActionButton(
    session = session,
    inputId = "reset",
    label = paste0(LANG.reset)
  )

  updateActionButton(
    session = session,
    inputId = "set_to_polls",
    label = paste0(LANG.polls)
  )

  updateActionButton(
    session = session,
    inputId = "info",
    label = paste0(LANG.info)
  )

  observeEvent(
    input$launch_year_1,
    {
      results_2019 <- read.csv("noviembre_2019.csv")

      A_change <- as.numeric(input$obs_A) / 100
      C_change <- as.numeric(input$obs_C) / 100
      B_change <- as.numeric(input$obs_B) / 100
      E_change <- as.numeric(input$obs_E) / 100
      D_change <- as.numeric(input$obs_D) / 100

      targets <- orig_IDs_1
      values <- c(A_change, B_change, C_change, D_change, E_change)
      results_2019 <- national_manipulator(results_2019, targets, values)

      results_by_major_2019 <- results_2019 %>%
        select(-minor) %>%
        group_by(major) %>%
        summarise(across(c(everything()), sum),
          .groups = "drop"
        )

      results_by_nation_2019 <- results_2019 %>%
        select(-c(minor, seats, major)) %>%
        summarise(across(c(everything()), sum),
          .groups = "drop"
        )

      results_2019_pct <- results_2019 |> precinct_percenter()
      results_by_major_2019_pct <- results_by_major_2019 |> major_percenter()
      results_by_nation_2019_pct <- results_by_nation_2019 |> national_percenter()

      results_2019_minor_winner <- results_2019_pct |> winner_ID()
      results_2019_major_winner <- results_by_major_2019_pct |> winner_ID()

      if (LANG.others %in% party_db$party == F) {
        party_db <- add_row(party_db, party = LANG.others, color = "#B8B8B8", leftmost = mean(party_db$leftmost) + 0.01)
      }

      party_color_levels <- party_db$color
      party_name_levels <- party_db$party

      results_2019_minor_winner <- compressor(
        results_2019_minor_winner,
        party_name_levels,
        LANG.others
      )

      results_2019_major_winner <- compressor(
        results_2019_major_winner,
        party_name_levels,
        LANG.others
      )

      minor_shapefile <- left_join(minor_shapefile, results_2019_minor_winner, by = "minor")

      major_shapefile <- left_join(major_shapefile, results_2019_major_winner, by = "major")

      outputplot <- ggplot() +
        geom_sf(
          data = major_shapefile,
          mapping = aes(fill = factor(winner), alpha = share, color = I("black")),
          linewidth = 0.25
        ) +
        labs(
          title = paste0(
            LANG.partymostvoted, " ",
            LANG.major, " (",
            LANG.year1, ")"
          )
        ) +
        theme(
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          plot.background = element_rect(
            fill = "white",
            color = NA
          ),
          panel.background = element_rect(
            fill = "white",
            color = NA
          )
        ) +
        scale_fill_manual(
          name = "",
          breaks = party_name_levels,
          values = party_color_levels,
          drop = T
        ) +
        scale_alpha(guide = "none")

      outputplot2 <- ggplot() +
        geom_sf(
          data = minor_shapefile,
          mapping = aes(fill = factor(winner), alpha = share, color = I("black")),
          linewidth = 0.25
        ) +
        labs(
          title = paste0(
            LANG.partymostvoted, " ",
            LANG.minor, " (",
            LANG.year1, ")"
          )
        ) +
        theme(
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          plot.background = element_rect(
            fill = "white",
            color = NA
          ),
          panel.background = element_rect(
            fill = "white",
            color = NA
          )
        ) +
        scale_fill_manual(
          name = "",
          breaks = party_name_levels,
          values = party_color_levels,
          drop = T
        ) +
        scale_alpha(guide = "none")

      test <- nationwide(results_2019)

      test_retain <- test |>
        group_by(minor) |>
        mutate(total = sum(value)) |>
        mutate(pp = value / total) |>
        mutate(votos = value) |>
        mutate(partido = party) |>
        mutate(escaños = seats) |>
        mutate(minor = minor) |>
        ungroup() |>
        select(minor, partido, escaños, pp) |>
        filter(pp > 0.03) |>
        arrange(minor, -pp)

      colnames(test_retain) <-
        c(LANG.minor, LANG.party, LANG.seats, LANG.pct)

      test <- test |>
        select(-value) |>
        filter(seats > 0)

      party_sums <- test |>
        group_by(party) |>
        summarise(seats = sum(seats))

      party_sums_retained <- party_sums |>
        arrange(-seats)

      transform_seats <- percent_vote_table(results_by_nation_2019_pct, results_by_nation_2019)

      merged_table <- left_join(transform_seats, party_sums, by = "party") |>
        mutate(seats = if_else(is.na(seats), 0, seats)) |>
        mutate(party = if_else(seats == 0, LANG.others, party)) |>
        group_by(party) |>
        summarise(seats = sum(seats), pct = sum(pct), votes = sum(votes)) |>
        arrange(-seats)

      party_sums$winner <- party_sums$party
      party_sums <- compressor(
        party_sums,
        party_name_levels,
        LANG.others
      )
      party_sums$party <- party_sums$winner
      party_sums <- party_sums |> select(-winner)

      party_sums <- left_join(party_sums,
        party_db,
        by = "party"
      )

      party_sums$party_f <- fct(party_sums$party,
        levels = unique(party_sums$party[order(party_sums$leftmost)])
      )

      party_sums <- party_sums |>
        arrange(party_f)

      hemisphere <- parliament_data(
        election_data = party_sums,
        type = "semicircle",
        parl_rows = 10,
        party_seats = party_sums$seats
      )

      semiplot <- ggplot(hemisphere, aes(x, y, colour = party_f)) +
        geom_point(size = 3) +
        theme_ggparliament() +
        labs(colour = NULL) +
        scale_colour_manual(
          values = party_color_levels[party_name_levels %in% unique(hemisphere$party)],
          limits = party_name_levels[party_name_levels %in% unique(hemisphere$party)]
        ) +
        theme(legend.position = "bottom")

      merged_table <- merged_table |>
        select(party, seats, votes, pct)

      colnames(merged_table) <- c(
        LANG.party,
        LANG.seats,
        LANG.votes,
        LANG.pct
      )

      output$major_plot <- renderPlot(outputplot, res = 96, width = 800, height = 500)
      output$minor_plot <- renderPlot(outputplot2, res = 96, width = 800, height = 500)
      output$SEMI_Plot <- renderPlot(semiplot, res = 96, width = 500, height = 300)
      output$DATA_TABLE <- DT::renderDataTable(DT::datatable(merged_table, options = list(language = list(
        paginate = list(
          previous = LANG.before,
          `next` = LANG.after
        ),
        `search` = LANG.search
      ), scrollY = TRUE, dom = "tp", pageLength = 5), rownames = FALSE) %>%
        DT::formatPercentage(c(LANG.pct), mark = " ", dec.mark = ",", 1) %>%
        DT::formatCurrency(c(LANG.votes), currency = "", mark = " ", dec.mark = ",", digits = 0))
      output$minor_info_table <- DT::renderDataTable(DT::datatable(test_retain, options = list(language = list(
        paginate = list(
          previous = LANG.before,
          `next` = LANG.after
        ),
        `search` = LANG.search
      ), scrollY = TRUE, dom = "tf", pageLength = 5), rownames = FALSE) %>%
        DT::formatPercentage(c(LANG.pct), mark = " ", dec.mark = ",", 1))
    }
  )


  observeEvent(
    input$launch_year_2,
    {
      results_2019 <- read.csv("julio_2023.csv")

      A_change <- as.numeric(input$obs_A) / 100
      C_change <- as.numeric(input$obs_C) / 100
      B_change <- as.numeric(input$obs_B) / 100
      D_change <- as.numeric(input$obs_D) / 100
      E_change <- as.numeric(input$obs_E) / 100

      targets <- orig_IDs_2
      values <- c(A_change, B_change, C_change, D_change, E_change)
      results_2019 <- national_manipulator(results_2019, targets, values)

      results_by_major_2019 <- results_2019 %>%
        select(-minor) %>%
        group_by(major) %>%
        summarise(across(c(everything()), sum),
          .groups = "drop"
        )

      results_by_nation_2019 <- results_2019 %>%
        select(-c(minor, seats, major)) %>%
        summarise(across(c(everything()), sum),
          .groups = "drop"
        )

      results_2019_pct <- results_2019 |> precinct_percenter()
      results_by_major_2019_pct <- results_by_major_2019 |> major_percenter()
      results_by_nation_2019_pct <- results_by_nation_2019 |> national_percenter()

      results_2019_minor_winner <- results_2019_pct |> winner_ID()
      results_2019_major_winner <- results_by_major_2019_pct |> winner_ID()

      party_color_levels <- party_db$color
      party_name_levels <- party_db$party

      results_2019_minor_winner <- compressor(
        results_2019_minor_winner,
        party_name_levels,
        LANG.others
      )

      results_2019_major_winner <- compressor(
        results_2019_major_winner,
        party_name_levels,
        LANG.others
      )

      minor_shapefile <- left_join(minor_shapefile, results_2019_minor_winner, by = "minor")

      major_shapefile <- left_join(major_shapefile, results_2019_major_winner, by = "major")

      outputplot <- ggplot() +
        geom_sf(
          data = major_shapefile,
          mapping = aes(fill = factor(winner), alpha = share, color = I("black")),
          linewidth = 0.25
        ) +
        labs(
          title = paste0(
            LANG.partymostvoted, " ",
            LANG.major, " (",
            LANG.year2, ")"
          )
        ) +
        theme(
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          plot.background = element_rect(
            fill = "white",
            color = NA
          ),
          panel.background = element_rect(
            fill = "white",
            color = NA
          )
        ) +
        scale_fill_manual(
          name = "",
          breaks = party_name_levels,
          values = party_color_levels,
          drop = T
        ) +
        scale_alpha(guide = "none")

      outputplot2 <- ggplot() +
        geom_sf(
          data = minor_shapefile,
          mapping = aes(fill = factor(winner), alpha = share, color = I("black")),
          linewidth = 0.25
        ) +
        labs(
          title = paste0(
            LANG.partymostvoted, " ",
            LANG.minor, " (",
            LANG.year2, ")"
          )
        ) +
        theme(
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          plot.background = element_rect(
            fill = "white",
            color = NA
          ),
          panel.background = element_rect(
            fill = "white",
            color = NA
          )
        ) +
        scale_fill_manual(
          name = "",
          breaks = party_name_levels,
          values = party_color_levels,
          drop = T
        ) +
        scale_alpha(guide = "none")

      test <- nationwide(results_2019)

      test_retain <- test |>
        group_by(minor) |>
        mutate(total = sum(value)) |>
        mutate(pp = value / total) |>
        mutate(votos = value) |>
        mutate(partido = party) |>
        mutate(escaños = seats) |>
        mutate(minor = minor) |>
        ungroup() |>
        select(minor, partido, escaños, pp) |>
        filter(pp > 0.03) |>
        arrange(minor, -pp)

      colnames(test_retain) <-
        c(LANG.minor, LANG.party, LANG.seats, LANG.pct)

      test <- test |>
        select(-value) |>
        filter(seats > 0)

      party_sums <- test |>
        group_by(party) |>
        summarise(seats = sum(seats))

      party_sums_retained <- party_sums |>
        arrange(-seats)

      transform_seats <- percent_vote_table(results_by_nation_2019_pct, results_by_nation_2019)


      merged_table <- left_join(transform_seats, party_sums, by = "party") |>
        mutate(seats = if_else(is.na(seats), 0, seats)) |>
        mutate(party = if_else(seats == 0, LANG.others, party)) |>
        group_by(party) |>
        summarise(seats = sum(seats), pct = sum(pct), votes = sum(votes)) |>
        arrange(-seats)


      # Name Handler
      party_sums$winner <- party_sums$party
      party_sums <- compressor(
        party_sums,
        party_name_levels,
        LANG.others
      )
      party_sums$party <- party_sums$winner
      party_sums <- party_sums |> select(-winner)


      party_sums <- left_join(party_sums,
        party_db,
        by = "party"
      )

      party_sums$party_f <- fct(party_sums$party,
        levels = unique(party_sums$party[order(party_sums$leftmost)])
      )

      party_sums <- party_sums |>
        arrange(party_f)

      hemisphere <- parliament_data(
        election_data = party_sums,
        type = "semicircle",
        parl_rows = 10,
        party_seats = party_sums$seats
      )


      semiplot <- ggplot(hemisphere, aes(x, y, colour = party_f)) +
        geom_point(size = 3) +
        theme_ggparliament() +
        labs(colour = NULL) +
        scale_colour_manual(
          values = party_color_levels[party_name_levels %in% unique(hemisphere$party)],
          limits = party_name_levels[party_name_levels %in% unique(hemisphere$party)]
        ) +
        theme(legend.position = "bottom")

      merged_table <- merged_table |>
        select(party, seats, votes, pct)

      colnames(merged_table) <- c(
        LANG.party,
        LANG.seats,
        LANG.votes,
        LANG.pct
      )


      output$major_plot <- renderPlot(outputplot, res = 96, width = 800, height = 500)
      output$minor_plot <- renderPlot(outputplot2, res = 96, width = 800, height = 500)
      output$SEMI_Plot <- renderPlot(semiplot, res = 96, width = 500, height = 300)
      output$DATA_TABLE <- DT::renderDataTable(DT::datatable(merged_table, options = list(language = list(
        paginate = list(
          previous = LANG.before,
          `next` = LANG.after
        ),
        `search` = LANG.search
      ), scrollY = TRUE, dom = "tp", pageLength = 5), rownames = FALSE) %>%
        DT::formatPercentage(c(LANG.pct), mark = " ", dec.mark = ",", 1) %>%
        DT::formatCurrency(c(LANG.votes), currency = "", mark = " ", dec.mark = ",", digits = 0))
      output$minor_info_table <- DT::renderDataTable(DT::datatable(test_retain, options = list(language = list(
        paginate = list(
          previous = LANG.before,
          `next` = LANG.after
        ),
        `search` = LANG.search
      ), scrollY = TRUE, dom = "tf", pageLength = 5), rownames = FALSE) %>%
        DT::formatPercentage(c(LANG.pct), mark = " ", dec.mark = ",", 1))
    }
  )

  observeEvent(input$reset, {
    updateSliderInput(session, "obs_A", value = 100)
    updateSliderInput(session, "obs_B", value = 100)
    updateSliderInput(session, "obs_C", value = 100)
    updateSliderInput(session, "obs_D", value = 100)
    updateSliderInput(session, "obs_E", value = 100)
  })

  observeEvent(input$set_to_polls, {
    updateSliderInput(session, "obs_A", value = return_polls[1])
    updateSliderInput(session, "obs_B", value = return_polls[2])
    updateSliderInput(session, "obs_C", value = return_polls[3])
    updateSliderInput(session, "obs_D", value = return_polls[4])
    updateSliderInput(session, "obs_E", value = return_polls[5])
  })

  observeEvent(input$info, {
    showModal(modalDialog(
      h5(LANG.message1),
      h5(LANG.message2),
      footer = NULL,
      easyClose = TRUE
    ))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
