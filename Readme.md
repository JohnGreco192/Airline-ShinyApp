# Airline Route Investment Optimization - R Shiny Application

This repository contains the R code for an interactive Shiny web application that analyzes airline route performance to identify profitable investment opportunities. The application showcases a full data pipeline, from cleaning and munging to complex financial analysis and interactive visualizations.

## Live Application

You can explore and interact with the live application here:
https://zo8fyo-john-greco.shinyapps.io/JohnGrecoAirChallenge/

## Project Overview

This project provides strategic insights for optimizing airline route investments by analyzing various factors like flight volume, profitability, delays, and operational costs. It features:

* **Robust Data Processing:** Implements data cleaning, validation, and aggregation using `tidyverse` for efficiency and quality.
* **SQL Integration:** Utilizes SQLite for in-memory database operations, demonstrating efficient data querying and joining.
* **Financial Modeling:** Calculates detailed revenue (ticket, baggage) and cost (distance, airport, delay) components to derive route profitability.
* **Strategic Metrics:** Develops key performance indicators such as "Profit per Mile," "Profit per Flight," "Breakeven Flights," and a "Route Score" for investment prioritization.
* **Interactive Visualizations:** Offers multiple interactive `plotly` and `ggplot2` visualizations within a Shiny UI to explore:
    * Overall profitability tarends.
    * Detailed revenue/cost breakdowns for top routes.
    * Sankey diagrams for individual route financial flows.
    * Recommended routes based on a weighted scoring model.
    * Breakeven analysis for investment decisions.
* **Automated Reporting:** Generates a PDF report using R Markdown to summarize key findings and provide metadata.

## Technical Details

* **Language:** R
* **Key R Packages:** `tidyverse` (data manipulation), `DBI`, `RSQLite` (database interaction), `shiny`, `plotly`, `scales` (application development and visualization), `rmarkdown` (reporting).
* **Methodologies:** Data cleaning & quality checks, feature engineering, statistical aggregation, cost/revenue modeling, weighted scoring.

## Note on Data

The original raw dataset used for this analysis is proprietary and is not included in this public repository to ensure data privacy. The R code demonstrates the full data processing and analysis pipeline, which is executable if provided with similar raw flight, ticket, and airport data.
