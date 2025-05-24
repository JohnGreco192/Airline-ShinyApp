# Script by John Greco - Principal Data Analyst -(Software) applicant at CapitalOne

# install.packages(tidyverse)
# install.packages(DBI)
# install.packages(RSQLite)
# install.packages(shiny)
# install.packages(plotly)
# install.packages(scales)
# install.packages(rmarkdown)

library(tidyverse)
library(DBI)
library(RSQLite)
library(shiny)
library(plotly)
library(scales)
library(rmarkdown)

# --- 1. Get Data ---

## 1.2 Define URL, Download, Unzip
download.file(zip_url, "data.zip", mode = "wb")
unzip("data.zip", exdir = "airline_data")

## 1.2 Read to Posit Cloud(R)
flights <- read_csv("airline_data/Flights.csv")
tickets <- read_csv("airline_data/Tickets.csv")
airports <- read_csv("airline_data/Airport_Codes.csv")

# --- 2. Clean Data ---

# Helper function to remove NA values and convert columns to numeric
remove_na_and_convert_numeric <- function(df, cols) {
  for (col in cols) {
    df[[col]] <- as.numeric(df[[col]])
    df <- df %>% filter(!is.na(!!sym(col)))
  }
  return(df)
}

# Helper function to create the ROUND_TRIP_ROUTE
create_round_trip_route <- function(df, origin_col, destination_col) {
  df %>%
    mutate(
      ROUND_TRIP_ROUTE = ifelse(
        !!sym(origin_col) < !!sym(destination_col),
        paste(!!sym(origin_col), !!sym(destination_col), sep = "-"),
        paste(!!sym(destination_col), !!sym(origin_col), sep = "-")
      )
    )
}

# Helper function to remove outliers
remove_outliers <- function(df, col, n_sd = 3) {
  col_sym <- sym(col)
  df %>%
    mutate(
      mean_val = mean(!!col_sym, na.rm = TRUE),
      sd_val = sd(!!col_sym, na.rm = TRUE)
    ) %>%
    filter(
      !!col_sym >= mean_val - n_sd * sd_val & !!col_sym <= mean_val + n_sd * sd_val
    ) %>%
    select(-mean_val, -sd_val)
}

## 2.1 Flights
flights_clean <- flights %>%
  # Select required columns (to save compute on posit cloud)
  select(ORIGIN, DESTINATION, DEP_DELAY, ARR_DELAY, CANCELLED, DISTANCE, OCCUPANCY_RATE, FL_DATE) %>%
  # Filter Q1 2019
  filter(FL_DATE >= as.Date("2019-01-01") & FL_DATE <= as.Date("2019-03-31")) %>%
  # Filter cancelled
  filter(CANCELLED != 1)

# Data Quality Check 1: Numeric Columns (using helper function)
flights_clean <- remove_na_and_convert_numeric(flights_clean, c("DEP_DELAY", "ARR_DELAY", "DISTANCE", "OCCUPANCY_RATE"))

## 2.2 Tickets
tickets_clean <- tickets %>%
  # Select essential columns FIRST (to save compute on Posit cloud)
  select(ORIGIN, DESTINATION, ROUNDTRIP, ITIN_FARE) %>%
  # Roundtrip only
  filter(ROUNDTRIP == 1)

# Data Quality Check 2: ITIN_FARE (using helper function)
tickets_clean <- remove_na_and_convert_numeric(tickets_clean, c("ITIN_FARE"))

## 2.3 Airports
airports_clean <- airports %>%
  # Select columns (to save compute on posit cloud)
  select(TYPE, IATA_CODE, ISO_COUNTRY) %>%
  # Filter to US airports
  filter(ISO_COUNTRY == "US") %>%
  # Filter to medium and large airports
  filter(TYPE %in% c("medium_airport", "large_airport")) %>%
  filter(!is.na(IATA_CODE) & !is.na(TYPE))

# Data Quality Check 3: Airport TYPE
airports_clean <- airports_clean %>% filter(!is.na(TYPE))

# --- 3. Data Munging ---

## 3.1 Pre-join Aggregations
### -- Pre-join aggregated calculations were chosen for efficiency, reducing number of records to join. Flight-level calculations would require using average ticket prices from sampled ticket data.  
### -- Group by ROUND_TRIP_ROUTE created in both Tickets and Flights for join and to prevent duplication
# Aggregate flights data
flights_agg <- flights_clean %>%
  create_round_trip_route("ORIGIN", "DESTINATION") %>%
  group_by(ROUND_TRIP_ROUTE) %>%
  summarise(
    TOTAL_ROUND_TRIP_FLIGHTS = n(),
    SUM_DEP_DELAY = sum(DEP_DELAY, na.rm = TRUE),
    AVG_DEP_DELAY = mean(DEP_DELAY, na.rm = TRUE),
    SUM_ARR_DELAY = sum(ARR_DELAY, na.rm = TRUE),
    AVG_ARR_DELAY = mean(ARR_DELAY, na.rm = TRUE),
    SUM_DISTANCE = sum(DISTANCE, na.rm = TRUE),
    AVG_DISTANCE = mean(DISTANCE, na.rm = TRUE),
    AVG_OCCUPANCY_RATE = mean(OCCUPANCY_RATE, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # Remove Delay Outliers (Standard Deviation Method) from aggregated data (post aggregate prep for compute efficiency) using mean.
  remove_outliers("AVG_DEP_DELAY") %>%
  remove_outliers("AVG_ARR_DELAY")

# Aggregate tickets data
tickets_agg <- tickets_clean %>%
  create_round_trip_route("ORIGIN", "DESTINATION") %>%
  group_by(ROUND_TRIP_ROUTE) %>%
  summarise(
    TOTAL_ITIN_FARE = sum(ITIN_FARE, na.rm = TRUE),
    AVG_ITIN_FARE = mean(ITIN_FARE, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # Remove ITIN_FARE Outliers using mean and 3 standard deviations.
  remove_outliers("AVG_ITIN_FARE")

## 3.2 Create SQLite database and write tables ---
# Create a temporary database in memory
conn <- dbConnect(RSQLite::SQLite(), ":memory:")

# Write the cleaned data frames to the database
dbWriteTable(conn, "flights_agg", flights_agg)
dbWriteTable(conn, "tickets_agg", tickets_agg)
dbWriteTable(conn, "airports_clean", airports_clean)

## 3.3 SQL query to join
query <- "
SELECT
    f.*,
    t.TOTAL_ITIN_FARE,
    t.AVG_ITIN_FARE,
    a_origin.TYPE AS TYPE_origin,
    a_destination.TYPE AS TYPE_destination
FROM
    flights_agg f
LEFT JOIN
    tickets_agg t ON f.ROUND_TRIP_ROUTE = t.ROUND_TRIP_ROUTE
LEFT JOIN
    airports_clean a_origin ON SUBSTR(f.ROUND_TRIP_ROUTE, 1, 3) = a_origin.IATA_CODE
LEFT JOIN
    airports_clean a_destination ON SUBSTR(f.ROUND_TRIP_ROUTE, 5, 3) = a_destination.IATA_CODE
"

# Execute the query and fetch the results
flights_tickets_airports_joined <- dbGetQuery(conn, query)

# --- 4. Analysis ---

## 4.1 Calculate Revenue
flights_tickets_airports_joined <- flights_tickets_airports_joined %>%
  mutate(
    # Assumption: Maximum 200 seats per plane. Flights data is a sample, so actual occupancy is unknown.
    TICKET_REVENUE = AVG_OCCUPANCY_RATE * 200 * AVG_ITIN_FARE * TOTAL_ROUND_TRIP_FLIGHTS,
    BAGGAGE_REVENUE = AVG_OCCUPANCY_RATE * 200 * 0.5 * 70 * TOTAL_ROUND_TRIP_FLIGHTS, # 50% passengers, $70 per round trip
    TOTAL_REVENUE = TICKET_REVENUE + BAGGAGE_REVENUE
  )

## 4.2 Calculate Costs
flights_tickets_airports_joined <- flights_tickets_airports_joined %>%
  mutate(
    # Assumption: $8 per mile for fuel, oil, maintenance, and crew; $1.18 per mile for depreciation, insurance, and other costs.
    DISTANCE_COST = SUM_DISTANCE * (8 + 1.18),
    # Assumption: $5,000 for medium airports, $10,000 for large airports, per landing.
    ORIGIN_AIRPORT_COST = ifelse(TYPE_origin == "medium_airport", 5000, 10000) * TOTAL_ROUND_TRIP_FLIGHTS,
    DESTINATION_AIRPORT_COST = ifelse(TYPE_destination == "medium_airport", 5000, 10000) * TOTAL_ROUND_TRIP_FLIGHTS,
    # Assumption: $75 per minute for departure and arrival delays, after the first 15 minutes per flight, calculated using average delays.
    DELAY_COST = (pmax(AVG_DEP_DELAY - 15, 0) + pmax(AVG_ARR_DELAY - 15, 0)) * 75 * TOTAL_ROUND_TRIP_FLIGHTS,
    TOTAL_COST = DISTANCE_COST + ORIGIN_AIRPORT_COST + DESTINATION_AIRPORT_COST + DELAY_COST
  )

## 4.3 Calculate Total Profit Per Round Trip Route
flights_tickets_airports_joined <- flights_tickets_airports_joined %>%
  mutate(
    PROFIT = TOTAL_REVENUE - TOTAL_COST
  )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# --- Main objectives
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Data processing
top_10_flights <- flights_tickets_airports_joined %>%
  arrange(desc(TOTAL_ROUND_TRIP_FLIGHTS)) %>%
  head(10) %>%
  select(ROUND_TRIP_ROUTE, TOTAL_ROUND_TRIP_FLIGHTS)

top_10_profit <- flights_tickets_airports_joined %>%
  arrange(desc(PROFIT)) %>%
  head(10) %>%
  select(ROUND_TRIP_ROUTE, PROFIT, TOTAL_REVENUE, TOTAL_ROUND_TRIP_FLIGHTS)

flights_tickets_airports_joined <- flights_tickets_airports_joined %>%
  mutate(
    PROFIT_PER_FLIGHT = PROFIT / TOTAL_ROUND_TRIP_FLIGHTS, # Calculates the average profit generated per flight on the route.
    BREAKEVEN_FLIGHTS = 90000000 / PROFIT_PER_FLIGHT, # Estimates the number of flights required to cover $90 million in fixed costs, assuming a constant profit per flight.
    PROFIT_PER_MILE = PROFIT / SUM_DISTANCE # Calculates the profit generated per mile flown on the route, reflecting efficiency.
  )

# Weighted route score calculation
weighted_data <- flights_tickets_airports_joined %>%
  mutate(
    normalized_profit_per_mile = scales::rescale(PROFIT_PER_MILE, to = c(0, 1)),
    normalized_profit_per_flight = scales::rescale(PROFIT_PER_FLIGHT, to = c(0, 1)),
    normalized_delay = 1 - scales::rescale(AVG_DEP_DELAY + AVG_ARR_DELAY, to = c(0, 1)),
    ROUTE_SCORE = (0.35 * normalized_profit_per_mile) +
      (0.35 * normalized_profit_per_flight) +
      (0.30 * normalized_delay)
  )

recommended_routes <- weighted_data %>%
  arrange(desc(ROUTE_SCORE)) %>%
  head(5) %>%
  select(
    ROUND_TRIP_ROUTE,
    PROFIT_PER_MILE,
    PROFIT_PER_FLIGHT,
    AVG_DEP_DELAY,
    AVG_ARR_DELAY,
    ROUTE_SCORE
  )

breakeven_routes <- recommended_routes %>%
  mutate(BREAKEVEN_FLIGHTS = 90000000 / PROFIT_PER_FLIGHT) %>% # Recalculates breakeven flights for the top 5 routes.
  select(ROUND_TRIP_ROUTE, BREAKEVEN_FLIGHTS)

# Metadata table
metadata_table <- data.frame(
  Variable = c(
    "ROUND_TRIP_ROUTE",
    "TOTAL_ROUND_TRIP_FLIGHTS",
    "SUM_DEP_DELAY",
    "AVG_DEP_DELAY",
    "SUM_ARR_DELAY",
    "AVG_ARR_DELAY",
    "SUM_DISTANCE",
    "AVG_DISTANCE",
    "AVG_OCCUPANCY_RATE",
    "TOTAL_ITIN_FARE",
    "AVG_ITIN_FARE",
    "TYPE_origin",
    "TYPE_destination",
    "TICKET_REVENUE",
    "BAGGAGE_REVENUE",
    "TOTAL_REVENUE",
    "DISTANCE_COST",
    "ORIGIN_AIRPORT_COST",
    "DESTINATION_AIRPORT_COST",
    "DELAY_COST",
    "TOTAL_COST",
    "PROFIT",
    "PROFIT_PER_FLIGHT",
    "BREAKEVEN_FLIGHTS",
    "PROFIT_PER_MILE",
    "ROUTE_SCORE"
  ),
  Description = c(
    "Combined origin-destination IATA airport codes, representing a unique round-trip flight route.",
    "Total number of round-trip flights operated on the route during Q1 2019, indicating flight volume.",
    "Sum of all departure delays (in minutes) for flights on the route.",
    "Average departure delay (in minutes) per flight on the route.",
    "Sum of all arrival delays (in minutes) for flights on the route.",
    "Average arrival delay (in minutes) per flight on the route.",
    "Total cumulative flight distance (in miles) for the route.",
    "Average flight distance (in miles) per flight on the route.",
    "Average percentage of available seats occupied by passengers on the route.",
    "Total revenue generated from ticket sales for the route.",
    "Average ticket fare per passenger on the route.",
    "Type of origin airport (e.g., hub, regional).",
    "Type of destination airport (e.g., hub, regional).",
    "Revenue specifically from ticket sales on the route.",
    "Revenue generated from baggage fees on the route.",
    "Total revenue from all sources (tickets, baggage, etc.) for the route.",
    "Total cost associated with the total distance flown on the route.",
    "Total cost associated with operating from the origin airport for the route.",
    "Total cost associated with operating to the destination airport for the route.",
    "Total cost incurred due to flight delays on the route.",
    "Total cost of operating the route, including distance, airport, and delay costs.",
    "Total profit (revenue minus costs) generated by the route.",
    "Average profit generated per flight on the route.", # Updated description.
    "Estimated number of flights required to cover fixed costs of $90 million, based on average profit per flight.", # Updated description.
    "Profit generated per mile flown on the route, indicating route efficiency.",
    "A composite score evaluating the route's overall performance, weighted by profitability and delays."
  )
)

# R Markdown content
report_content <- c(
  "---",
  "title: Airline Route Analysis Report",
  "output: pdf_document",
  "---",
  "",
  "# Route Analysis: Key Findings",
  "",
  "Analysis of airline route data: flight volume, profitability, recommendations, and breakeven points.",
  "",
  "## 1. Busiest Routes: Top 10",
  "",
  "Top 10 routes ranked by total round trip flights in Q1 2019, indicating market demand.",
  "",
  knitr::kable(top_10_flights, format = "markdown", table.attr = 'style=\"width:100%;\"'),
  "",
  "## 2. Most Profitable Routes: Top 10",
  "",
  "Top 10 routes ranked by total profit in Q1 2019, reflecting overall financial performance.",
  "",
  knitr::kable(top_10_profit, format = "markdown", table.attr = 'style=\"width:100%;\"'),
  "",
  "## 3. Recommended Routes: 5 Picks",
  "",
  "Top 5 routes selected based on a weighted score prioritizing profitability per mile (35%), profitability per flight (35%), and minimizing delays (30%), in line with our brand motto 'On time, for you'.",
  "Focus on efficiency and punctuality drives both financial success and customer satisfaction.",
  "",
  knitr::kable(recommended_routes, format = "markdown", table.attr = 'style=\"width:100%;\"'),
  "",
  "## 4. Breakeven Point for Recommended Routes",
  "",
  "Estimated number of flights for each recommended route to break even, assuming a $90 million aircraft cost. This calculation uses profit per flight.",
  "The **Route Score** balances long-distance efficiency, trip profitability, and on-time performance (35% profit/mile, 35% profit/flight, 30% delays).",
  "Achieving breakeven quickly maximizes the time value of money, even with constant depreciation.",
  "",
  knitr::kable(breakeven_routes, format = "markdown", table.attr = 'style=\"width:100%;\"'),
  "",
  "## 5. Key Performance Indicators and Future Focus",
  "",
  "Strategic KPIs and analyses to optimize route performance, focusing on efficiency and brand image.",
  "",
  "### Key Performance Indicators",
  "",
  "**Financial Efficiency:**",
  "",
  "* Profit per Mile: Route profitability relative to distance.",
  "* Profit per Flight: Revenue generated per flight.",
  "",
  "**Operational Excellence:**",
  "",
  "* On-Time Performance (OTP): Percentage of scheduled flights operating on time.",
  "",
  "**Customer Satisfaction:**",
  "",
  "* Net Promoter Score (NPS): Customer loyalty.",
  "* Customer Complaint Rate: Service quality.",
  "",
  "### Future Refinement: Iterative Approach",
  "",
  "**Enhancements:**",
  "",
  "* **Route Score Optimization:** Guided by the CRISP-DM analytics lifecycle, we propose gaterhing stakeholder feedback to refine the weighting of profitability and delay factors within the Route Score.",
  "",
  "* **Sensitivity Analysis:** Perform sensitivity analysis to understand how changes in the Route Score weights impact route recommendations, informing future adjustments.",
  "",
  "* **Continuous Improvement:** Establish a review process to regularly update and validate the Route Score methodology, reflecting the iterative nature of data analysis.",
  "",
  "## Metadata",
  "",
  "<style> table { width: 100%; border-collapse: collapse; } th, td { padding: 8px; text-align: left; border-bottom: 1px solid #ddd; } th { background-color: #f2f2f2; } </style>",
  knitr::kable(metadata_table, format = "html", table.attr = 'style=\"width:100%;\"', col.names = NULL)
)

# Generate PDF
writeLines(report_content, "airline_report.Rmd")
rmarkdown::render("airline_report.Rmd", output_format = "pdf_document", output_file = "my_airline_project/airline_report.pdf")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Data Viz
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ui <- fluidPage(
  titlePanel("Airline Route Investment Optimization"),
  tabsetPanel(
    tabPanel("Profitability Overview",
             plotlyOutput("profitScatter"),
             "Profitability vs. Flight Volume, with fare and delay impacts."
    ),
    tabPanel("Top Routes",
             plotlyOutput("topRoutesWaterfall"),
             "Revenue, costs, and profit for the top 10 routes."
    ),
    tabPanel("Route Breakdown Sankey",
             selectInput("routeSelect", "Select Route:", choices = NULL),
             plotlyOutput("routeSankey"),
             "Revenue and cost component breakdown for a selected route."
    ),
    tabPanel("Recommended Routes",
             plotlyOutput("recommendedRoutesScatter"),
             "Recommended Routes: Balancing Profitability and On-Time Performance."
    ),
    tabPanel("Breakeven Analysis",
             plotOutput("breakevenChart"),
             "Breakeven Flights (Sorted), based on $90M fixed costs."
    )
  )
)

server <- function(input, output, session) {
  output$profitScatter <- renderPlotly({
    ggplotly(
      ggplot(flights_tickets_airports_joined, aes(x = TOTAL_ROUND_TRIP_FLIGHTS, y = PROFIT, size = AVG_ITIN_FARE, color = AVG_DEP_DELAY + AVG_ARR_DELAY, text = ROUND_TRIP_ROUTE)) +
        geom_point(alpha = 0.7) +
        labs(
          title = "Profitability vs. Flight Volume",
          subtitle = "Higher fares (size) and lower delays (color) correlate with higher profits.",
          x = "Total Round Trip Flights",
          y = "Profit (USD)",
          size = "Avg. Fare",
          color = "Total Delay (min)"
        ) +
        scale_y_continuous(labels = scales::comma)
    )
  })
  
  output$topRoutesWaterfall <- renderPlotly({
    top_routes <- flights_tickets_airports_joined %>%
      arrange(desc(PROFIT)) %>%
      head(10) %>%
      select(ROUND_TRIP_ROUTE, TICKET_REVENUE, BAGGAGE_REVENUE, DISTANCE_COST, ORIGIN_AIRPORT_COST, DESTINATION_AIRPORT_COST, DELAY_COST, PROFIT) %>%
      mutate(TOTAL_REVENUE = TICKET_REVENUE + BAGGAGE_REVENUE) %>%
      mutate(TOTAL_COST = DISTANCE_COST + ORIGIN_AIRPORT_COST + DESTINATION_AIRPORT_COST + DELAY_COST)
    
    top_routes_waterfall <- top_routes %>%
      select(ROUND_TRIP_ROUTE, TOTAL_REVENUE, TOTAL_COST, PROFIT) %>%
      mutate(
        TOTAL_COST = -TOTAL_COST,
        PROFIT = PROFIT
      ) %>%
      pivot_longer(cols = -ROUND_TRIP_ROUTE, names_to = "Component", values_to = "Amount")
    
    top_routes_waterfall$Component <- factor(top_routes_waterfall$Component, levels = c("TOTAL_REVENUE", "TOTAL_COST", "PROFIT"))
    
    ggplotly(ggplot(top_routes_waterfall, aes(x = ROUND_TRIP_ROUTE, y = Amount, fill = Component)) +
               geom_col() +
               coord_flip() +
               labs(title = "Top 10 Routes: Waterfall of Revenue, Costs, and Profit", x = "Route", y = "Amount (USD)") +
               scale_y_continuous(labels = scales::comma) +
               scale_fill_manual(values = c("TOTAL_REVENUE" = "steelblue", "TOTAL_COST" = "coral", "PROFIT" = "forestgreen")))
  })
  
  #Sankey Specific Code
  top_50_routes <- reactive({
    flights_tickets_airports_joined %>%
      arrange(desc(PROFIT)) %>%
      head(50) %>%
      pull(ROUND_TRIP_ROUTE)
  })
  
  observe({
    updateSelectInput(session, "routeSelect", choices = top_50_routes())
  })
  
  output$routeSankey <- renderPlotly({
    selected_route_data <- flights_tickets_airports_joined %>%
      filter(ROUND_TRIP_ROUTE == input$routeSelect) %>%
      select(TICKET_REVENUE, BAGGAGE_REVENUE, DISTANCE_COST, ORIGIN_AIRPORT_COST, DESTINATION_AIRPORT_COST, DELAY_COST)
    
    sankey_data <- data.frame(
      source = c("Revenue", "Revenue", "Costs", "Costs", "Costs", "Costs"),
      target = c("Ticket Revenue", "Baggage Revenue", "Distance Cost", "Origin Airport Cost", "Destination Airport Cost", "Delay Cost"),
      value = c(selected_route_data$TICKET_REVENUE, selected_route_data$BAGGAGE_REVENUE,
                selected_route_data$DISTANCE_COST, selected_route_data$ORIGIN_AIRPORT_COST,
                selected_route_data$DESTINATION_AIRPORT_COST, selected_route_data$DELAY_COST)
    )
    
    plot_ly(
      type = "sankey",
      orientation = "h",
      node = list(
        pad = 15,
        thickness = 20,
        line = list(color = "black", width = 0.5),
        label = c("Revenue", "Ticket Revenue", "Baggage Revenue", "Costs", "Distance Cost", "Origin Airport Cost", "Destination Airport Cost", "Delay Cost"),
        color = c("blue", "lightblue", "lightblue", "red", "coral", "coral", "coral", "coral")
      ),
      link = list(
        source = c(0, 0, 3, 3, 3, 3),
        target = c(1, 2, 4, 5, 6, 7),
        value = sankey_data$value
      )
    ) %>%
      layout(title = paste("Route Breakdown for", input$routeSelect)) #Dynamic Title
  })
  
  output$recommendedRoutesScatter <- renderPlotly({
    ggplotly(
      ggplot(recommended_routes, aes(x = PROFIT_PER_MILE, y = PROFIT_PER_FLIGHT, size = ROUTE_SCORE, color = AVG_DEP_DELAY + AVG_ARR_DELAY, text = ROUND_TRIP_ROUTE)) +
        geom_point(alpha = 0.7) +
        labs(
          title = "Recommended Routes: Score vs. Profitability",
          subtitle = "Balancing profitability and on-time performance",
          x = "Profit per Mile (USD)",
          y = "Profit per Flight (USD)",
          size = "Route Score",
          color = "Total Delay (min)"
        )
    )
  })
  
  output$breakevenChart <- renderPlot({
    breakeven_routes_plot <- breakeven_routes %>%
      arrange(BREAKEVEN_FLIGHTS) #Sort by breakeven flights
    
    ggplot(breakeven_routes_plot, aes(x = reorder(ROUND_TRIP_ROUTE, BREAKEVEN_FLIGHTS), y = BREAKEVEN_FLIGHTS)) +
      geom_col(fill = "coral") +
      coord_flip() +
      labs(title = "Breakeven Flights (Sorted)", subtitle = "Based on $90M fixed costs", x = "Route", y = "Flights to Breakeven")
  })
}


shinyApp(ui = ui, server = server)

#########################---


  
