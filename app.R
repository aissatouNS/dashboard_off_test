#Global Ecosystem Mapping

# =========================
#  INSTALL & LOAD PACKAGES
# =========================
options(repos = c(CRAN = "https://cloud.r-project.org/"))

pkgs <- c(
  "shiny","shinydashboard","readxl","dplyr","tidyr","plotly","DT","leaflet",
  "shinyWidgets","stringr","forcats","htmltools","purrr"
)
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# Small helper
`%||%` <- function(x, y) if (!is.null(x)) x else y

# Normalise any label to a likely dummy column name in your sheet
to_col <- function(s) {
  s <- iconv(s, to = "ASCII//TRANSLIT")
  s <- gsub("[^A-Za-z0-9]+", "_", s)
  s <- gsub("_+", "_", s)
  s <- gsub("^_|_$", "", s)
  s
}

# =========================
#  LOAD DATA
# =========================
# Adjust path
data <- readxl::read_excel("C:/Users/PC/OneDrive - One Family Foundation/GCSI/Global Ecosystem/dashboard_repo/analysis.xlsx", sheet = 1)

# Clean column names
names(data) <- str_replace_all(names(data), "\\s+", "_")

# Trim text cols
trim_all <- function(x) {
  x <- gsub("\u00A0", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}
data <- data %>% mutate(across(where(is.character), trim_all))

# Make sure key columns exist
key_cols <- c("Organization","Category","Status","Description","Fields","SDGs",
              "Continent","Region","Country","Scope","Longitude","Latitude")
for (nm in key_cols) if (!nm %in% names(data)) data[[nm]] <- NA

# Clean Category (remove numbering prefixes like "6. ...")
clean_category <- function(x) trim_all(gsub("^\\s*\\d+\\.?\\s*", "", x))
data <- data %>% mutate(Category_clean = clean_category(Category))

# Coerce coords safely
data <- data %>%
  mutate(
    Longitude = suppressWarnings(as.numeric(Longitude)),
    Latitude  = suppressWarnings(as.numeric(Latitude))
  )

# =========================
#  DEFINITIONS & COLOR MAPS
# =========================
category_defs <- list(
  "Global Civil Society and Grassroots Networks" =
    "Includes NGOs, community-based organizations, informal movements, innovation networks, humanitarian networks.",
  "Social and Solidarity Economy Actors" =
    "Includes social enterprises, cooperatives, mutuals, platform cooperatives, new economy movements.",
  "Inclusive and Impact Oriented Finance" =
    "Includes microfinance institutions, inclusive finance providers, impact investors, blended finance platforms.",
  "Corporate Social Responsibility and Private Sector Engagement" =
    "Includes CSR programs, ESG reporting platforms, social procurement alliances, corporate philanthropy.",
  "Knowledge and Narrative Infrastructure" =
    "Includes think tanks, academic institutes, research collaboratives, civic media, storytelling platforms.",
  "Ecosystem Builders and Connectors" =
    "Includes incubators, innovation hubs, conveners, strategic consultancies, ecosystem platforms.",
  "Multilateral and Bilateral Development Partners" =
    "Includes UN agencies, multilateral banks, bilateral donors, intergovernmental cooperation platforms.",
  "Foundations" =
    "Includes church funds, zakat, wealth advisors, donor advised funds, community foundations.",
  "Large International NGOs" =
    "Includes major global NGOs with broad thematic and geographic coverage."
)

cat_colors <- c(
  "Global Civil Society and Grassroots Networks" = "#44AA99",
  "Social and Solidarity Economy Actors"         = "#44AA99",
  "Inclusive and Impact Oriented Finance"        = "#44AA99",
  "Corporate Social Responsibility and Private Sector Engagement" = "#44AA99",
  "Knowledge and Narrative Infrastructure"       = "#44AA99",
  "Ecosystem Builders and Connectors"            = "#44AA99",
  "Multilateral and Bilateral Development Partners" = "#44AA99",
  "Foundations"                                  = "#44AA99",
  "Large International NGOs"                     = "#44AA99"
)

# SDGs (labels with spaces) → dummy columns with underscores
sdg_labels <- paste0("SDG ", 1:17)
sdg_cols   <- paste0("SDG_", 1:17)
sdg_map    <- setNames(sapply(sdg_labels, to_col), sdg_labels)   # "SDG 1" -> "SDG_1"
# Ensure missing SDG dummy columns exist (as 0)
for (cl in unname(sdg_map)) if (!cl %in% names(data)) data[[cl]] <- 0
data[unname(sdg_map)] <- lapply(data[unname(sdg_map)], function(x) suppressWarnings(as.numeric(x)) %||% 0)

sdg_list <- c(
  "SDG 1"  = "No poverty", "SDG 2"  = "Zero hunger", "SDG 3"  = "Good health and well-being",
  "SDG 4"  = "Quality education", "SDG 5"  = "Gender equality", "SDG 6"  = "Clean water and sanitation",
  "SDG 7"  = "Affordable and clean energy", "SDG 8"  = "Decent work and economic growth",
  "SDG 9"  = "Industry, innovation and infrastructure", "SDG 10" = "Reduced inequalities",
  "SDG 11" = "Sustainable cities and communities", "SDG 12" = "Responsible consumption and production",
  "SDG 13" = "Climate action", "SDG 14" = "Life below water", "SDG 15" = "Life on land",
  "SDG 16" = "Peace, justice, and strong institutions", "SDG 17" = "Partnerships for the goals"
)

sdg_colors <- c(
  SDG_1="#e5243b", SDG_2="#dda63a", SDG_3="#4c9f38", SDG_4="#c5192d", SDG_5="#ff3a21",
  SDG_6="#26bde2", SDG_7="#fcc30b", SDG_8="#a21942", SDG_9="#fd6925", SDG_10="#dd1367",
  SDG_11="#fd9d24", SDG_12="#bf8b2e", SDG_13="#3f7e44", SDG_14="#0a97d9", SDG_15="#56c02b",
  SDG_16="#00689d", SDG_17="#19486a"
)

# CONTINENTS / REGIONS / COUNTRIES (labels → dummy columns)
continent_labels <- c(
  "Africa","Americas","Antarctica","Asia","Europe","MENA","Oceania"
)
region_labels <- c(
  "Antarctica","Australia and New Zealand","Caribbean","Central America","Central Asia",
  "Eastern Africa","Eastern Asia","Eastern Europe","Middle Africa","Middle East North Africa",
  "Northern America","Oceania","South - Eastern Asia","South America","Southern Africa",
  "Southern Asia","Western Africa","Western Asia","Western Europe"
)
# Derive choices from data's dummy columns to avoid typos, but still handle spaces.
country_labels <- {
  # find likely dummy country columns: those not in the known sets but look like dummies
  known <- c(names(data), "Organization","Category","Category_clean","Status","Description","Fields",
             "SDGs","Continent","Region","Country","Scope","Longitude","Latitude")
  # Take columns with many 0/1 values as dummies, exclude SDGs/continents/regions we know
  dummy_like <- names(data)[vapply(data, function(x) is.numeric(x) && mean(x %in% c(0,1), na.rm = TRUE) > 0.8, TRUE)]
  # Exclude SDGs & our continent/region candidates
  exclude <- c(unname(sdg_map), sapply(continent_labels, to_col), sapply(region_labels, to_col))
  cand <- setdiff(dummy_like, exclude)
  # Convert back to labels for display: underscores -> spaces
  labs <- gsub("_", " ", cand)
  # Title case-ish
  labs <- stringr::str_to_title(labs)
  sort(unique(labs))
}

continent_map <- setNames(sapply(continent_labels, to_col), continent_labels)
region_map    <- setNames(sapply(region_labels,    to_col), region_labels)
country_map   <- setNames(sapply(country_labels,   to_col), country_labels)

# ---- World shapes for choropleth ----
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  sf::st_transform(4326) %>%
  dplyr::select(name, geometry)

# =========================
#  UI
# =========================
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Global Actors of Social Innovation", titleWidth = 420),
  dashboardSidebar(width = 200,
                   sidebarMenu(id = "tabs",
                               menuItem("Overview", tabName = "overview", icon = icon("home")),
                               menuItem("SDG Repartition", tabName = "chartsdg", icon = icon("list")),
                               menuItem("Geographic Repartition", tabName = "geochart", icon = icon("globe")),
                               menuItem("Map & Filters", tabName = "map", icon = icon("map")),
                               menuItem("Actor Detail", tabName = "detail", icon = icon("user"))
                   )
  ),
  dashboardBody(
    tabItems(
      # 1) OVERVIEW
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("totalActors", width = 4),
                valueBoxOutput("totalCategories", width = 4)
              ),
              fluidRow(
                box(title = "Actors by Category", width = 9, plotlyOutput("catPlot"))
              ),
              fluidRow(
                box(title = "Category Definitions", width = 10, uiOutput("catDefs"))
              )
      ),
      # 2) SDG Repartition
      tabItem(tabName = "chartsdg",
              fluidRow(
                box(title = "Actors by SDG", width = 10, plotlyOutput("chartSDG"))
              ),
              fluidRow(
                box(title = "Sustainable Development Goals list", width = 10, uiOutput("sdgList"))
              )
      ),
      # 3) Geographic Repartition
      tabItem(tabName = "geochart",
              fluidRow(
                box(title = "Actors by Continent", width = 9, plotlyOutput("chartContinent"))
              ),
              fluidRow(
                box(title = "Actors by Region", width = 9, plotlyOutput("chartRegion"))
              )
      ),
      # 4) MAP & FILTERS
      tabItem(tabName = "map",
              fluidRow(
                box(width = 3, title = "Filters",
                    pickerInput("filterCategory", "Category",
                                choices = sort(unique(na.omit(data$Category_clean))), multiple = TRUE,
                                options = list(`actions-box` = TRUE)
                    ),
                    pickerInput("filterSDG", "SDG(s)",
                                choices = setNames(names(sdg_map), paste0(names(sdg_map), " – ", sdg_list[names(sdg_map)])),
                                multiple = TRUE, options = list(`actions-box` = TRUE)
                    ),
                    pickerInput("filterContinent", "Continent",
                                choices = names(continent_map), multiple = TRUE,
                                options = list(`actions-box` = TRUE)
                    ),
                    pickerInput("filterRegion", "Region",
                                choices = names(region_map), multiple = TRUE,
                                options = list(`actions-box` = TRUE)
                    ),
                    pickerInput("filterCountry", "Country",
                                choices = names(country_map), multiple = TRUE,
                                options = list(`actions-box` = TRUE, liveSearch = TRUE)
                    ),
                    pickerInput("filterScope", "Scope",
                                choices = sort(unique(na.omit(data$Scope))), multiple = TRUE,
                                options = list(`actions-box` = TRUE)
                    )
                ),
                box(width = 9,
                    leafletOutput("mapPlot", height = 520),
                    br(), h4("Matching Actors"),
                    DTOutput("mapTable")
                )
              )
      ),
      # 5) DETAIL
      tabItem(tabName = "detail",
              fluidRow(
                box(width = 12,
                    h2(textOutput("actorOrganization")),
                    h4(textOutput("actorCategory")),
                    hr(),
                    htmlOutput("actorInfo")
                )
              )
      )
    )
  )
)

# =========================
#  SERVER
# =========================
server <- function(input, output, session) {
  
  # ---- KPI
  output$totalActors <- renderValueBox({
    valueBox(nrow(data), "Total Actors", icon = icon("users"), color = "olive")
  })
  output$totalCategories <- renderValueBox({
    valueBox(9, "Total Categories", icon = icon("list"), color = "light-blue")
  })
  
  # ---- Overview: category bar 
  output$catPlot <- renderPlotly({
    df <- data %>% count(Category_clean, name = "n") %>%
      mutate(Category_clean = forcats::fct_reorder(Category_clean, n, .desc = TRUE))
    p <- ggplot(df, aes(x = Category_clean, y = n, fill = Category_clean)) +
      geom_col() +
      geom_text(aes(label = n), vjust = -0.25, size = 4) +
      scale_fill_manual(values = cat_colors, guide = "none") +
      labs(x = "Category", y = "Number of actors") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 25, hjust = 1))
    gp <- ggplotly(p, tooltip = NULL) %>% 
      layout(showlegend = FALSE) %>% 
      config(displayModeBar = FALSE)
    for (i in seq_along(gp$x$data)) gp$x$data[[i]]$hoverinfo <- "skip"
    gp
  })
  
  output$catDefs <- renderUI({
    tagList(lapply(names(category_defs), function(cat) {
      div(tags$b(cat), ": ", category_defs[[cat]], tags$br(), tags$br())
    }))
  })
  
  # ---- SDG Bar 
  output$chartSDG <- renderPlotly({
    cnt <- vapply(unname(sdg_map), function(cl) {
      if (cl %in% names(data)) sum(as.numeric(data[[cl]]), na.rm = TRUE) else 0
    }, numeric(1))
    df <- tibble(SDG = names(sdg_map), Count = as.numeric(cnt)) %>%
      mutate(colkey = unname(sdg_map)) %>%
      arrange(match(SDG, paste0("SDG ", 1:17)))
    p <- ggplot(df, aes(x = factor(SDG, levels = paste0("SDG ", 1:17)), y = Count, fill = colkey)) +
      geom_col() +
      geom_text(aes(label = Count), vjust = -0.25, size = 4) +
      scale_fill_manual(values = sdg_colors, guide = "none") +
      labs(x = "SDG", y = "Number of actors") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 25, hjust = 1))
    gp <- ggplotly(p, tooltip = NULL) %>% 
      layout(showlegend = FALSE) %>% 
      config(displayModeBar = FALSE)
    for (i in seq_along(gp$x$data)) gp$x$data[[i]]$hoverinfo <- "skip"
    gp
  })
  
  output$sdgList <- renderUI({
    tags$ul(
      lapply(names(sdg_list), function(k) {
        tags$li(tags$b(k), " : ", sdg_list[[k]])
      })
    )
  })
  
  # ---- Continent bar (no hover)
  output$chartContinent <- renderPlotly({
    cnt <- vapply(unname(continent_map), function(cl) {
      if (cl %in% names(data)) sum(as.numeric(data[[cl]]), na.rm = TRUE) else 0
    }, numeric(1))
    df <- tibble(Continent = names(continent_map), Count = as.numeric(cnt)) %>%
      arrange(desc(Count))
    p <- ggplot(df, aes(x = forcats::fct_reorder(Continent, Count), y = Count)) +
      geom_col(fill = "orange") +
      geom_text(aes(label = Count), vjust = -0.25, size = 4) +
      coord_flip() +
      labs(x = "Continent", y = "Number of actors") +
      theme_minimal()
    gp <- ggplotly(p, tooltip = NULL) %>% config(displayModeBar = FALSE)
    for (i in seq_along(gp$x$data)) gp$x$data[[i]]$hoverinfo <- "skip"
    gp
  })
  
  # ---- Region bar (no hover)
  output$chartRegion <- renderPlotly({
    cnt <- vapply(unname(region_map), function(cl) {
      if (cl %in% names(data)) sum(as.numeric(data[[cl]]), na.rm = TRUE) else 0
    }, numeric(1))
    df <- tibble(Region = names(region_map), Count = as.numeric(cnt)) %>%
      arrange(desc(Count))
    p <- ggplot(df, aes(x = forcats::fct_reorder(Region, Count), y = Count)) +
      geom_col(fill = "red") +
      geom_text(aes(label = Count), vjust = -0.25, size = 4) +
      coord_flip() +
      labs(x = "Region", y = "Number of actors") +
      theme_minimal()
    gp <- ggplotly(p, tooltip = NULL) %>% config(displayModeBar = FALSE)
    for (i in seq_along(gp$x$data)) gp$x$data[[i]]$hoverinfo <- "skip"
    gp
  })
  
  # =========================
  #  FILTERING (wide data, dummies)
  # =========================
  filtered_data <- reactive({
    df <- data
    
    # Category
    if (length(input$filterCategory)) {
      df <- df %>% filter(Category_clean %in% input$filterCategory)
    }
    
    # SDGs (all must be 1)
    if (length(input$filterSDG)) {
      for (lab in input$filterSDG) {
        col <- sdg_map[[lab]]
        if (col %in% names(df)) df <- df %>% filter(.data[[col]] == 1)
      }
    }
    
    # Continent (all selected must be 1)
    if (length(input$filterContinent)) {
      for (lab in input$filterContinent) {
        col <- continent_map[[lab]]
        if (col %in% names(df)) df <- df %>% filter(.data[[col]] == 1)
      }
    }
    
    # Region
    if (length(input$filterRegion)) {
      for (lab in input$filterRegion) {
        col <- region_map[[lab]]
        if (col %in% names(df)) df <- df %>% filter(.data[[col]] == 1)
      }
    }
    
    # Country
    if (length(input$filterCountry)) {
      for (lab in input$filterCountry) {
        col <- country_map[[lab]]
        if (col %in% names(df)) df <- df %>% filter(.data[[col]] == 1)
      }
    }
    
    # Scope
    if (length(input$filterScope)) {
      df <- df %>% filter(Scope %in% input$filterScope)
    }
    
    df
  })
  
  # Hierarchical updates for Region & Country choices based on current (upstream) filters
  observe({
    # Apply all filters except Region/Country themselves to compute availability
    df <- data
    
    if (length(input$filterCategory)) df <- df %>% filter(Category_clean %in% input$filterCategory)
    if (length(input$filterSDG)) {
      for (lab in input$filterSDG) {
        col <- sdg_map[[lab]]
        if (col %in% names(df)) df <- df %>% filter(.data[[col]] == 1)
      }
    }
    if (length(input$filterContinent)) {
      for (lab in input$filterContinent) {
        col <- continent_map[[lab]]
        if (col %in% names(df)) df <- df %>% filter(.data[[col]] == 1)
      }
    }
    if (length(input$filterScope)) df <- df %>% filter(Scope %in% input$filterScope)
    
    # Regions available
    reg_counts <- vapply(names(region_map), function(lab) {
      col <- region_map[[lab]]
      if (col %in% names(df)) sum(df[[col]] == 1, na.rm = TRUE) else 0
    }, numeric(1))
    avail_regions <- names(region_map)[reg_counts > 0]
    
    # Countries available
    ctry_counts <- vapply(names(country_map), function(lab) {
      col <- country_map[[lab]]
      if (col %in% names(df)) sum(df[[col]] == 1, na.rm = TRUE) else 0
    }, numeric(1))
    avail_countries <- names(country_map)[ctry_counts > 0]
    
    updatePickerInput(session, "filterRegion",
                      choices = names(region_map),
                      selected = intersect(input$filterRegion, avail_regions)
    )
    updatePickerInput(session, "filterCountry",
                      choices = names(country_map),
                      selected = intersect(input$filterCountry, avail_countries)
    )
  })
  
  # ---- Map (points only; choropleth skipped to avoid name mismatches)
  output$mapPlot <- renderLeaflet({
    df <- filtered_data()
    
    # Start base map
    m <- leaflet() %>% addProviderTiles("CartoDB.Positron")
    
    # ---- Choropleth from dummy country columns ----
    # Convert Natural Earth names to your dummy column naming (spaces -> underscores, etc.)
    w <- world
    w$colname <- vapply(w$name, to_col, character(1))
    
    # Count actors per country using the filtered data + dummy columns
    w$n <- vapply(
      w$colname,
      function(cl) if (cl %in% names(df)) sum(df[[cl]] == 1, na.rm = TRUE) else 0,
      numeric(1)
    )
    
    # Color palette for gradient
    pal <- colorBin("Blues", domain = w$n, bins = 6, na.color = "#f0f0f0")
    
    m <- m %>%
      addPolygons(
        data = w,
        weight = 0.5, color = "#888888", opacity = 1,
        fillColor = ~pal(n), fillOpacity = 0.85,
        label = ~paste0(name, ": ", n, " actors"),
        highlightOptions = highlightOptions(weight = 2, color = "#444", bringToFront = TRUE)
      ) %>%
      addLegend(
        pal = pal, values = w$n, opacity = 0.85,
        title = "Actors by country", position = "bottomright"
      )
    
    # ---- Point markers when coordinates exist ----
    has_pts <- any(!is.na(df$Longitude) & !is.na(df$Latitude))
    if (has_pts) {
      pts <- df %>%
        dplyr::filter(!is.na(Longitude), !is.na(Latitude)) %>%
        dplyr::mutate(Category_clean = as.character(Category_clean))
      
      m <- m %>%
        addCircleMarkers(
          lng = ~Longitude, lat = ~Latitude,
          data = pts,
          radius = 5, stroke = FALSE, fillOpacity = 0.9, color = "#d95f02",
          label = ~Organization,
          popup = ~paste0(
            "<b>", Organization, "</b>",
            "<br/>Category: ", Category_clean,
            ifelse(!is.na(Country) && Country != "", paste0("<br/>Country: ", Country), "")
          )
        )
    }
    
    m
  })
  
  
  # ---- Table under map (count, Organization, Category)
  output$mapTable <- renderDT({
    df <- filtered_data() %>%
      transmute(Organization, Category = Category_clean) %>%
      distinct() %>%
      mutate(`#` = dplyr::row_number(), .before = 1)
    datatable(df, selection = "single",
              options = list(dom = 'tip', pageLength = 10),
              rownames = FALSE)
  })
  
  # ---- Detail page
  observeEvent(input$mapTable_rows_selected, {
    sel <- input$mapTable_rows_selected
    if (length(sel) == 1) {
      # Recompute the visible table and pick the row
      df_show <- filtered_data() %>%
        transmute(Organization, Category = Category_clean) %>%
        distinct() %>%
        mutate(`#` = dplyr::row_number(), .before = 1)
      
      org <- df_show$Organization[sel]
      
      actor <- filtered_data() %>%
        filter(Organization == org) %>%
        slice(1)
      
      updateTabItems(session, "tabs", "detail")
      output$actorOrganization <- renderText(actor$Organization %||% "")
      output$actorCategory     <- renderText(actor$Category %||% actor$Category_clean %||% "")
      
      # Pretty table for details (use comma-delimited originals)
      make_row <- function(label, value) {
        if (is.null(value) || is.na(value) || value == "") return(NULL)
        tags$tr(
          tags$th(style="width:240px;text-align:left;padding-right:12px;", label),
          tags$td(htmltools::HTML(htmltools::htmlEscape(value)))
        )
      }
      
      output$actorInfo <- renderUI({
        tags$table(class = "table table-striped",
                   tags$tbody(
                     make_row("Status", actor$Status),
                     make_row("Description", actor$Description),
                     make_row("Field(s)", actor$Fields),
                     make_row("SDGs", actor$SDGs),
                     make_row("Continent", actor$Continent),
                     make_row("Region", actor$Region),
                     make_row("Country", actor$Country),
                     make_row("Scope", actor$Scope)
                   )
        )
      })
    }
  })
}

# =========================
#  RUN APP
# =========================
#shinyApp(ui, server)
shinyApp(ui = ui, server = server)