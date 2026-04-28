# APPROACH 4: Modern Shiny App with bslib Theming
# ISIS Mobilization Explorer  (International Relations dataset)
#
# Data: isis_mobilization.rds
#   Built in create_data.R from the Edgerton (2023, JCR) replication archive:
#     https://github.com/jfedgerton/Edgerton-2023-JCR
#   The raw PRIO-GRID cell-year file is aggregated to country-year.
#
#   Columns:
#     country_name, year, isis_fighters, isis_attacks, n_cells,
#     mean_nightlights, mean_gcp_ppp, total_population, mean_unemployment,
#     mean_polity, mean_gov_effect, any_sunni_excluded
#
# PLSC 498 - Week 15: Interactive Visualization with R Shiny

# Install any missing packages automatically --------------------------------
required_packages <- c("shiny", "bslib", "dplyr", "ggplot2", "plotly", "tidyr")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}
library(scales)
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)

# Load data -------------------------------------------------------------------
data_file <- if (file.exists("isis_mobilization.rds")) {
"isis_mobilization.rds"
} else {
  "data/isis_mobilization.rds"
}
isis <- readRDS(data_file)

year_min <- min(isis$year, na.rm = TRUE)
year_max <- max(isis$year, na.rm = TRUE)

name_map <- c(
  "Bosnia-Herzegovina" = "Bosnia and Herzegovina",
  "German Federal Republic" = "Germany",
  "Iran (Persia)" = "Iran",
  "Italy/Sardinia" = "Italy",
  "Kosovo" = "Kosovo",
  "Kyrgyz Republic" = "Kyrgyzstan",
  "Macedonia (Former Yugoslav Republic of)" = "North Macedonia",
  "Russia (Soviet Union)" = "Russia",
  "Tanzania/Tanganyika" = "Tanzania",
  "Turkey (Ottoman Empire)" = "Turkey",
  "United Kingdom" = "UK",
  "United States of America" = "USA",
  "Yemen (Arab Republic of Yemen)" = "Yemen"
)
isis$map_name <- ifelse(isis$country_name %in% names(name_map), name_map[isis$country_name],
  isis$country_name)

# Theme -----------------------------------------------------------------------
theme_obj <- bs_theme(
  version   = 5,
  primary   = "#1f77b4",
  secondary = "#d62728",
  success   = "#2ca02c",
  danger    = "#ff7f0e",
  base_font = font_google("Roboto")
)

# UI --------------------------------------------------------------------------
ui <- page_navbar(
  title   = "ISIS Mobilization Explorer",
  theme   = theme_obj,
  bg      = "#ffffff",
  underline = TRUE,

  # ---- Dashboard ----
  nav_panel(
    "Dashboard",
    layout_sidebar(
      sidebar = sidebar(
        h4("Filters"),
        sliderInput(
          "year_range", "Year:",
          min = year_min,
          max = year_max,
          value = c(year_min, year_max),
          step = 1,
          sep = ""
        ),
        sliderInput(
          "min_fighters", "Min ISIS fighters per Country-Year:",
          min = 0,
          max = 368,
          value = 0, 
          step = 16
        ),
        checkboxInput(
          "sunni_only",
          "Include only countries with politically excluded Sunni populations",
          value = FALSE
        ),
        hr(),
        p("Country-year aggregates of the PRIO-GRID cell-level ISIS",
          "mobilization data from Edgerton (2023, JCR).",
          style = "font-size: 0.9em;")
      ),
      navset_card_tab(
        nav_panel(
          "Overview",
          layout_column_wrap(
            value_box(
              title = "Countries with ISIS Activity",
              value = textOutput("n_countries"),
              theme = "primary"
            ),
            value_box(
              title = "Total Fighters (grid-cell count)",
              value = textOutput("total_fighters"),
              theme = "secondary"
            ),
            value_box(
              title = "Total Attacks",
              value = textOutput("total_attacks"),
              theme = "primary", 
              step = 1
            ),
            col_widths = c(4, 4, 4), 
            heights_equal = c("all"), 
            height = c("200px")
          ),
          plotOutput("fighters_timeline", height = "400px"),
          layout_column_wrap(
            card(
              full_screen = TRUE,
              card_header("Attacks Over Time"),
              plotlyOutput("attacks_over_time"), 
              step = 1
            ),
            card(
              full_screen = TRUE,
              card_header("Top 10 Source Countries (of ISIS fighters)"),
              plotOutput("top_countries_bar")
            ),
            col_widths = c(6, 6), 
            heights_equal = c("all")
          )
        ),
        nav_panel(
          "Map",
          fluidRow(
            column(4,
              selectInput(
                "map_metric", "Metric to display:",
                choices  = c("ISIS Fighters" = "isis_fighters",
                             "ISIS Attacks" = "isis_attacks"),
                selected = "isis_fighters"
              )
            )
          ),
          card(
            full_screen = TRUE,
            card_header("Global ISIS Activity (log+1 scale)"),
            plotOutput("world_map", height = "550px")
          )
        ),
        nav_panel(
          "Correlates",
          card(
            full_screen = TRUE,
            card_header("Unemployment vs. ISIS Fighters"),
            plotlyOutput("unemp_scatter", height = "450px")
          ),
          layout_column_wrap(
            card(
              full_screen = TRUE,
              card_header("Polity vs. Fighters"),
              plotlyOutput("polity_scatter")
            ),
            card(
              full_screen = TRUE,
              card_header("Government Effectiveness vs. Fighters"),
              plotlyOutput("goveffect_scatter")
            ),
            col_widths = c(6, 6)
          )
        )
      )
    )
  ),

  # ---- Country Profile ----
  nav_panel(
    "Country",
    layout_sidebar(
      sidebar = sidebar(
        selectInput(
          "country_select", "Select country:",
          choices  = sort(unique(isis$map_name)),
          selected = "Iraq"
        ),
        hr(),
        p("Drill down into one country's ISIS-mobilization time series.",
          style = "font-size: 0.9em;")
      ),
      navset_card_tab(
        nav_panel(
          "Profile", 
          layout_column_wrap(
            value_box(title = "Total Fighters",
                      value = textOutput("country_fighters"),
                      theme = "primary"),
            value_box(title = "Total Attacks",
                      value = textOutput("country_attacks"),
                      theme = "secondary"),
            value_box(title = "Mean Unemployment",
                      value = textOutput("country_unemployment"),
                      theme = "primary"),
            col_widths = c(4, 4, 4)
          ),
          card(
            card_header("Fighters and Attacks By Year"),
            plotOutput("country_timeline", height = "420px")
          )
        ),
        nav_panel(
          "Comparative",
          card(
            full_screen = TRUE,
            card_header("Top-15 Source Countries vs. Selected Country"),
            plotlyOutput("country_comparison")
          )
        )
      )
    )
  ),

  # ---- Data ----
  nav_panel(
    "Data",
    layout_column_wrap(
      card(
        full_screen = TRUE,
        card_header("Raw Country-Year Data"),
        downloadButton("download_data", "Download CSV"),
        br(),
        dataTableOutput("data_table")
      ),
      col_widths = 12
    )
  ),

  # ---- About ----
  nav_panel(
    "About",
    layout_column_wrap(
      card(
        full_screen = TRUE,
        markdown(
"## ISIS Mobilization Explorer

This interactive application explores country-year patterns of ISIS mobilization
built from the replication archive for:

**Edgerton, Jared (2023).** *Journal of Conflict Resolution.*
Repository: <https://github.com/jfedgerton/Edgerton-2023-JCR>

### What you are looking at

The underlying data are PRIO-GRID cell-year observations of ISIS fighter counts
and ISIS attacks, merged with a range of geographic, economic, and political
covariates. `create_data.R` aggregates those cells up to the country-year level
for use in this classroom app.

### Variables

- **isis_fighters** - sum of the cell-level ISIS fighter `count`
- **isis_attacks** - sum of ISIS attacks across grid cells
- **mean_unemployment** - WDI male unemployment (country mean over cells)
- **mean_polity** - Polity2 score
- **mean_gov_effect** - WBGI government effectiveness
- **any_sunni_excluded** - whether any cell belongs to a politically
  excluded Sunni group

### Caveats

These aggregates are illustrative and should not be used for inference outside
of the classroom. See the original paper and replication code for the
cell-level models used in Edgerton (2023)."
        )
      ),
      col_widths = 12
    )
  )
)

# Server ----------------------------------------------------------------------
server <- function(input, output, session) {

  filtered <- reactive({
    df <- isis %>%
      filter(year >= input$year_range[1],
             year <= input$year_range[2],
             isis_fighters >= input$min_fighters)
    if (isTRUE(input$sunni_only)) {
      df <- df %>% filter(any_sunni_excluded == 1)
    }
    df
  })

  country_df <- reactive({
    isis %>% filter(country_name == input$country_select)
  })

  # ---- Overview value boxes ----
  output$n_countries <- renderText({
    length(unique(filtered()$country_name))
  })
  output$total_fighters <- renderText({
    format(sum(filtered()$isis_fighters, na.rm = TRUE), big.mark = ",")
  })
  output$total_attacks <- renderText({
    format(sum(filtered()$isis_attacks, na.rm = TRUE), big.mark = ",")
  })

  output$fighters_timeline <- renderPlot({
    df <- filtered() %>%
      group_by(year) %>%
      summarize(fighters = sum(isis_fighters, na.rm = TRUE), .groups = "drop")
    validate(need(nrow(df) > 0, "No data for the current filters."))
    ggplot(df, aes(x = year, y = fighters)) +
      geom_area(fill = "#1f77b4", alpha = 0.5) +
      geom_line(color = "#1f77b4", linewidth = 1) +
      geom_point(color = "#1f77b4", size = 2.5) +
      labs(
        title = "Total ISIS Fighters Over Time",
        x = "Year",
        y = "Fighters (grid-cell count)",
        caption = "Source: Edgerton (2023, JCR)"
      ) +
      theme_minimal(base_size = 13) +
      scale_y_continuous(labels = comma) +
      scale_x_continuous(breaks = c(year_min, year_max), labels = c(year_min, year_max)) + 
      theme(plot.title = element_text(face = "bold", size = 13))
  })

  output$attacks_over_time <- renderPlotly({
    df <- filtered() %>%
      group_by(year) %>%
      summarize(attacks = sum(isis_attacks, na.rm = TRUE), .groups = "drop")
    plot_ly(df, x = ~year, y = ~attacks,
            type = "scatter", mode = "lines+markers",
            line = list(color = "#d62728", width = 2),
            marker = list(color = "#d62728", size = 6)) %>%
      layout(xaxis = list(title = "Year", 
                          tickvals = ~year),
             yaxis = list(title = "ISIS attacks", 
                          tickformat=",d"),
             hovermode = "x unified")
  })

  output$top_countries_bar <- renderPlot({
    df <- filtered() %>%
      group_by(country_name) %>%
      summarize(fighters = sum(isis_fighters, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(fighters)) %>%
      slice_head(n = 10)
    name_map <- c(
      "Bosnia-Herzegovina" = "Bosnia and Herzegovina",
      "German Federal Republic" = "Germany",
      "Iran (Persia)" = "Iran",
      "Italy/Sardinia" = "Italy",
      "Kosovo" = "Kosovo",
      "Kyrgyz Republic" = "Kyrgyzstan",
      "Macedonia (Former Yugoslav Republic of)" = "North Macedonia",
      "Russia (Soviet Union)" = "Russia",
      "Tanzania/Tanganyika" = "Tanzania",
      "Turkey (Ottoman Empire)" = "Turkey",
      "United Kingdom" = "UK",
      "United States of America" = "USA",
      "Yemen (Arab Republic of Yemen)" = "Yemen"
    )
    df$map_name <- ifelse(
      df$country_name %in% names(name_map),
      name_map[df$country_name],
      df$country_name
    )
    validate(need(nrow(df) > 0, "No data."))
    ggplot(df, aes(x = reorder(map_name, fighters), y = fighters)) +
      geom_col(fill = "#1f77b4", alpha = 0.85) +
      coord_flip() +
      labs(x = NULL, y = "Total Fighters") +
      theme_minimal(base_size = 13)
  })

  # ---- World map ----
  output$world_map <- renderPlot({
    df <- filtered()
    metric <- input$map_metric
    validate(need(nrow(df) > 0, "No data for the current filters."))

    # Aggregate to country totals
    country_totals <- df %>%
      group_by(country_name) %>%
      summarize(total = sum(.data[[metric]], na.rm = TRUE), .groups = "drop")

    # Map ISIS country names to map_data("world") names
    name_map <- c(
      "Bosnia-Herzegovina" = "Bosnia and Herzegovina",
      "German Federal Republic" = "Germany",
      "Iran (Persia)" = "Iran",
      "Italy/Sardinia" = "Italy",
      "Kosovo" = "Kosovo",
      "Kyrgyz Republic" = "Kyrgyzstan",
      "Macedonia (Former Yugoslav Republic of)" = "North Macedonia",
      "Russia (Soviet Union)" = "Russia",
      "Tanzania/Tanganyika" = "Tanzania",
      "Turkey (Ottoman Empire)" = "Turkey",
      "United Kingdom" = "UK",
      "United States of America" = "USA",
      "Yemen (Arab Republic of Yemen)" = "Yemen"
    )
    country_totals$map_name <- ifelse(
      country_totals$country_name %in% names(name_map),
      name_map[country_totals$country_name],
      country_totals$country_name
    )

    # Log transform (add 1 to handle zeros)
    country_totals$log_total <- log1p(country_totals$total)

    # Get world map polygons
    world <- ggplot2::map_data("world")

    # Join
    world_merged <- world %>%
      left_join(country_totals, by = c("region" = "map_name"))

    metric_label <- ifelse(metric == "isis_fighters", "Fighters", "Attacks")

    ggplot(world_merged, aes(x = long, y = lat, group = group)) +
      geom_polygon(aes(fill = log_total), color = "grey70", linewidth = 0.1) +
      scale_fill_gradient(
        low      = "#fff7ec",
        high     = "#d62728",
        na.value = "#f0f0f0",
        name     = "Fighters",
        labels   = function(x) format(round(x, 1), nsmall = 1)
      ) +
      coord_fixed(1.3, xlim = c(-170, 170), ylim = c(-55, 80)) +
      labs(
        title   = paste("Global ISIS", metric_label, "(log+1 scale)"),
        caption = "Source: Edgerton (2023, JCR) | Grey = no data"
      ) +
      theme_void(base_size = 12) +
      theme(
        plot.title      = element_text(face = "bold", hjust = 0.5, size = 14),
        plot.caption    = element_text(size = 9, color = "grey50"),
        legend.position = "bottom",
        legend.key.width = unit(1.5, "cm")
      )
  })

  # ---- Correlate scatters ----
  output$unemp_scatter <- renderPlotly({
    df <- filtered()
    plot_ly(df, x = ~mean_unemployment, y = ~isis_fighters,
            type = "scatter", mode = "markers",
            text = ~paste(country_name, year),
            marker = list(size = 9, color = "#1f77b4", opacity = 0.75)) %>%
      layout(xaxis = list(title = "Mean Unemployment (WDI)"),
             yaxis = list(title = "ISIS Fighters"))
  })

  output$polity_scatter <- renderPlotly({
    df <- filtered()
    plot_ly(df, x = ~mean_polity, y = ~isis_fighters,
            type = "scatter", mode = "markers",
            text = ~paste(country_name, year),
            marker = list(size = 9, color = "#d62728", opacity = 0.75)) %>%
      layout(xaxis = list(title = "Polity2"),
             yaxis = list(title = "ISIS Fighters"))
  })

  output$goveffect_scatter <- renderPlotly({
    df <- filtered()
    plot_ly(df, x = ~mean_gov_effect, y = ~isis_fighters,
            type = "scatter", mode = "markers",
            text = ~paste(country_name, year),
            marker = list(size = 9, color = "#d62728", opacity = 0.75)) %>%
      layout(xaxis = list(title = "Government Effectiveness (WBGI)"),
             yaxis = list(title = "ISIS Fighters"))
  })

  # ---- Country tab ----
  output$country_fighters <- renderText({
    format(sum(country_df()$isis_fighters, na.rm = TRUE), big.mark = ",")
  })
  output$country_attacks <- renderText({
    format(sum(country_df()$isis_attacks, na.rm = TRUE), big.mark = ",")
  })
  output$country_unemployment <- renderText({
    v <- mean(country_df()$mean_unemployment, na.rm = TRUE)
    if (is.nan(v)) "N/A" else sprintf("%.1f%%", v)
  })

  output$country_timeline <- renderPlot({
    df <- country_df()
    validate(need(nrow(df) > 0, "No data for this country."))
    df_long <- tidyr::pivot_longer(
      df,
      cols      = c(isis_fighters, isis_attacks),
      names_to  = "metric",
      values_to = "value"
    )
    ggplot(df_long, aes(x = year, y = value, color = metric)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2.5) +
      scale_color_manual(values = c("isis_fighters" = "#1f77b4",
                                    "isis_attacks"  = "#d62728"),
                         labels = c("Fighters", "Attacks"),
                         name   = NULL) +
      labs(
        title = paste(input$country_select,
                      "- ISIS Fighters and Attacks by Year"),
        x = "Year", 
        y = "Count"
      ) + scale_y_continuous(labels = comma) +
      scale_x_continuous(breaks = c(year_min, year_max), labels = c(year_min, year_max)) + 
      theme_minimal(base_size = 13) +
      theme(plot.title      = element_text(face = "bold"),
            legend.position = "bottom")
  })

  output$country_comparison <- renderPlotly({
    df <- isis %>%
      group_by(country_name) %>%
      summarize(fighters = sum(isis_fighters, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(fighters)) %>%
      slice_head(n = 15)

    df$highlight <- ifelse(df$country_name == input$country_select,
                           "Selected", "Other")

    plot_ly(df, x = ~reorder(country_name, fighters), y = ~fighters,
            color = ~highlight, colors = c("Other" = "#1f77b4",
                                           "Selected" = "#d62728"),
            type = "bar") %>%
      layout(xaxis = list(title = "", tickangle = -45),
             yaxis = list(title = "Total Fighters"),
             showlegend = FALSE,
             margin = list(b = 100))
  })

  # ---- Data tab ----
  output$data_table <- renderDataTable({
    isis %>%
      mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
      rename(
        "Country" = country_name,
        "Year" = year,
        "Fighters" = isis_fighters,
        "Attacks" = isis_attacks,
        "Grid cells" = n_cells,
        "Mean nightlights"= mean_nightlights,
        "Mean GCP (ppp)" = mean_gcp_ppp,
        "Population" = total_population,
        "Unemployment" = mean_unemployment,
        "Polity2" = mean_polity,
        "Gov effectiveness" = mean_gov_effect,
        "Sunni excluded" = any_sunni_excluded
      )
  })

  output$download_data <- downloadHandler(
    filename = function() paste0("isis_mobilization_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(isis, file, row.names = FALSE)
  )
}

# Run -------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
