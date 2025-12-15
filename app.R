library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(tidyr)
library(tibble)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(conflicted)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("arrange", "dplyr")

# Data
df <- df_emp_honshu_clean

# Metropolitan area coordinates (Honshu only)
metro_coords <- tibble(
  `Metropolitan Areas` = c(
    "Utsunomiya Metropolitan Areas",
    "Tokyo Metropolitan Areas",
    "Nagoya Metropolitan Areas",
    "Osaka Metropolitan Areas",
    "Kyoto Metropolitan Areas",
    "Kobe Metropolitan Areas",
    "Hiroshima Metropolitan Areas",
    "Sendai Metropolitan Areas",
    "Niigata Metropolitan Areas",
    "Shizuoka Metropolitan Areas",
    "Hamamatsu Metropolitan Areas"
  ),
  lon = c(
    139.8836, 139.6917, 136.9066, 135.5023, 135.7681,
    135.1955, 132.4553, 140.8719, 139.0232, 138.3831, 137.7261
  ),
  lat = c(
    36.5551, 35.6895, 35.1815, 34.6937, 35.0116,
    34.6901, 34.3853, 38.2682, 37.9162, 34.9756, 34.7108
  )
)

# Japan base map
japan_map <- ne_countries(
  scale = "medium",
  country = "Japan",
  returnclass = "sf"
)

# UI
ui <- fluidPage(
  titlePanel("Honshu Labour Force Explorer"),
  tabsetPanel(
    tabPanel(
      "Overview",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "overview_areas",
            "Select Metropolitan Areas",
            choices = unique(df$`Metropolitan Areas`),
            selected = unique(df$`Metropolitan Areas`),
            multiple = TRUE
          ),
          selectInput(
            "overview_metric",
            "Select Indicator",
            choices = c(
              "Total",
              "Population in labour force",
              "Employed persons",
              "Unemployed",
              "Population not in labour force"
            ),
            selected = "Employed persons"
          )
        ),
        mainPanel(
          plotOutput("overview_plot", height = 400),
          DTOutput("overview_table")
        )
      )
    ),
    
    tabPanel(
      "Structure",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "structure_area",
            "Select Metropolitan Area",
            choices = unique(df$`Metropolitan Areas`)
          ),
          radioButtons(
            "structure_type",
            "Display Type",
            choices = c("Absolute", "Percentage"),
            inline = TRUE
          )
        ),
        mainPanel(
          plotOutput("structure_plot", height = 400),
          DTOutput("structure_table")
        )
      )
    ),
    
    tabPanel(
      "Compare",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "compare_a",
            "Area A",
            choices = unique(df$`Metropolitan Areas`)
          ),
          selectInput(
            "compare_b",
            "Area B",
            choices = unique(df$`Metropolitan Areas`),
            selected = unique(df$`Metropolitan Areas`)[2]
          ),
          selectInput(
            "compare_metric",
            "Metric",
            choices = c(
              "Employment rate",
              "Unemployment rate",
              "Labour force participation rate"
            )
          )
        ),
        mainPanel(
          plotOutput("compare_plot", height = 350),
          verbatimTextOutput("compare_text")
        )
      )
    ),
    
    tabPanel(
      "Map",
      sidebarLayout(
        sidebarPanel(
          checkboxInput(
            "highlight_one",
            "Highlight a single metropolitan area",
            value = FALSE
          ),
          conditionalPanel(
            condition = "input.highlight_one == true",
            selectInput(
              "map_area",
              "Select Metropolitan Area",
              choices = unique(df$`Metropolitan Areas`)
            )
          )
        ),
        mainPanel(
          plotOutput("map_plot", height = 520)
        )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Overview
  overview_data <- reactive({
    df %>%
      dplyr::filter(`Metropolitan Areas` %in% input$overview_areas) %>%
      dplyr::select(`Metropolitan Areas`, value = dplyr::all_of(input$overview_metric))
  })
  
  output$overview_plot <- renderPlot({
    ggplot(overview_data(),
           aes(x = reorder(`Metropolitan Areas`, value), y = value)) +
      geom_col() +
      coord_flip() +
      labs(x = NULL, y = input$overview_metric) +
      theme_minimal()
  })
  
  output$overview_table <- renderDT({
    overview_data() %>% dplyr::arrange(dplyr::desc(value))
  })
  
  # Structure
  structure_data <- reactive({
    df %>%
      dplyr::filter(`Metropolitan Areas` == input$structure_area) %>%
      dplyr::select(
        `Employed persons`,
        Unemployed,
        `Did housework`,
        `Attending school`,
        Others
      ) %>%
      tidyr::pivot_longer(
        cols = dplyr::everything(),
        names_to = "Category",
        values_to = "Value"
      )
  })
  
  output$structure_plot <- renderPlot({
    dat <- structure_data()
    
    if (input$structure_type == "Percentage") {
      dat <- dat %>% dplyr::mutate(Value = Value / sum(Value))
    }
    
    ggplot(dat, aes(x = "", y = Value, fill = Category)) +
      geom_col(width = 0.6) +
      coord_flip() +
      theme_minimal() +
      labs(x = NULL, y = NULL)
  })
  
  output$structure_table <- renderDT({
    dat <- structure_data()
    if (input$structure_type == "Percentage") {
      dat <- dat %>% dplyr::mutate(Percentage = Value / sum(Value))
    }
    dat
  })
  
  # Compare
  compare_data <- reactive({
    df %>%
      dplyr::filter(`Metropolitan Areas` %in% c(input$compare_a, input$compare_b)) %>%
      dplyr::mutate(
        employment_rate = `Employed persons` / `Population in labour force`,
        unemployment_rate = Unemployed / `Population in labour force`,
        participation_rate = `Population in labour force` / Total
      )
  })
  
  output$compare_plot <- renderPlot({
    metric <- switch(
      input$compare_metric,
      "Employment rate" = "employment_rate",
      "Unemployment rate" = "unemployment_rate",
      "Labour force participation rate" = "participation_rate"
    )
    
    ggplot(compare_data(),
           aes(x = `Metropolitan Areas`, y = .data[[metric]], fill = `Metropolitan Areas`)) +
      geom_col() +
      labs(x = NULL, y = input$compare_metric) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$compare_text <- renderText({
    dat <- compare_data()
    
    metric <- switch(
      input$compare_metric,
      "Employment rate" = "employment_rate",
      "Unemployment rate" = "unemployment_rate",
      "Labour force participation rate" = "participation_rate"
    )
    
    a <- dat %>%
      dplyr::filter(`Metropolitan Areas` == input$compare_a) %>%
      dplyr::pull(dplyr::all_of(metric))
    
    b <- dat %>%
      dplyr::filter(`Metropolitan Areas` == input$compare_b) %>%
      dplyr::pull(dplyr::all_of(metric))
    
    if (a > b) {
      paste(input$compare_a, "has a higher", input$compare_metric)
    } else if (a < b) {
      paste(input$compare_b, "has a higher", input$compare_metric)
    } else {
      "The two areas have the same value."
    }
  })
  
  # Map — study area highlight
  map_data_all <- reactive({
    df %>%
      dplyr::left_join(metro_coords, by = "Metropolitan Areas") %>%
      dplyr::filter(!is.na(lon), !is.na(lat))
  })
  
  map_data_single <- reactive({
    req(input$map_area)
    map_data_all() %>%
      dplyr::filter(`Metropolitan Areas` == input$map_area)
  })
  
  output$map_plot <- renderPlot({
    
    base <- ggplot() +
      geom_sf(data = japan_map, fill = "gray95", color = "gray70") +
      theme_minimal() +
      labs(
        title = "Metropolitan areas included in this study",
        subtitle = "Highlighted locations indicate the spatial scope of analysis",
        x = NULL, y = NULL
      )
    
    if (isTRUE(input$highlight_one)) {
      base +
        geom_point(
          data = map_data_single(),
          aes(x = lon, y = lat),
          size = 5
        ) +
        geom_text(
          data = map_data_single(),
          aes(x = lon, y = lat, label = `Metropolitan Areas`),
          nudge_y = 0.4,
          size = 3
        )
    } else {
      base +
        geom_point(
          data = map_data_all(),
          aes(x = lon, y = lat),
          size = 4,
          alpha = 0.8
        )
    }
  })
}

shinyApp(ui, server)
