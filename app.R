library(dash)
library(dashBootstrapComponents)
library(dashHtmlComponents)
library(dashCoreComponents)
library(plotly)
library(purrr)
library(tidyverse)

# ==============================================================================
#                            Data wrangling
# ==============================================================================
df <- read.csv("data/Primary-energy-consumption-from-fossilfuels-nuclear-renewables.csv") %>%
    drop_na()

year_range <- seq(min(df$Year), max(df$Year), 5)
year_range <- setNames(as.list(as.character(year_range)), as.integer(year_range))

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)
app$title("World Energy Visualization")


# ==============================================================================
#                            Tab 1: Layout for sidebar
# ==============================================================================
sidebar1 <- dbcCol(
    list(
        htmlH3("World Energy Visualisation"),
        htmlH4("Global Distribution",
            style = list("color" = "#686868")
        ),
        htmlBr(),
        htmlH5(
            "Energy type",
        ),
        htmlP(
            "Select a energy type for visualization:",
            style = list("color" = "#686868", "margin" = 0, "font-size" = "14px"),
        ),
        dbcRow(
            dccDropdown(
                id = "tab1-energy-type-dropdown",
                options = df %>%
                    select(Fossil, Nuclear, Renewables) %>%
                    colnames() %>%
                    purrr::map(function(col) list(label = col, value = col)),
                value = "Fossil"
            ),
            style = list("padding" = 10)
        ),
        htmlBr(),
        htmlH5(
            "Data sources",
            style = list("width" = "50%", "display" = "inline-block"),
        ),
        dbcRow(
            dccMarkdown("Datasets for visualization of energy trends were downloaded from [here](https://www.kaggle.com/donjoeml/energy-consumption-and-generation-in-the-globe)")
        )
    ),
    style = list("max-width" = "20%", "background-image" = "url(/assets/wind-energy.jpg)")
)


# ==============================================================================
#                            Tab 1: Layout for plots
# ==============================================================================
tab1_plots <- dbcCol(
    list(
        htmlP(
            "Drag and select the number of year to view the change of engergy consumption distribution using the slide bar. You can hover or zoom to get the details of a specific region.",
            style = list("color" = "#888888"),
        ),
        dccGraph(id = "tab1-map"),
        dccSlider(
            id = "tab1-year-slider",
            min = min(df$Year),
            max = max(df$Year),
            step = 1,
            value = max(df$Year),
            marks = year_range,
            tooltip = list(
                always_visible = TRUE,
                placement = "top"
            )
        ),
        htmlBr(),
        htmlH4("Top/Bottom energy consumer nations"),
        htmlP(
            "Select the number of countries to view in the bar plot using the input tab, then select whether to view to the top or bottom consumers. Hover the bar for details.",
            style = list("color" = "#888888"),
        ),
        htmlBr(),
        dbcRow(
            list(
                dbcCol(
                    list(
                        htmlH4(
                            "Number of countries",
                            style = list("font-size" = "20px")
                        ),
                        htmlBr(),
                        dbcInput(
                            id = "tab1-input-topN",
                            min = 0,
                            max = 10,
                            step = 1,
                            type = "number"
                        )
                    )
                ),
                dbcCol(
                    list(
                        htmlH4(
                            "Ranking type",
                            style = list("font-size" = "20px"),
                        ),
                        htmlBr(),
                        dccRadioItems(
                            id = "tab1_top_bot",
                            options = list(
                                list("label" = "World", "value" = 1),
                                list("label" = "Regions", "value" = 2),
                                list("label" = "Countries", "value" = 3)
                            ),
                            value = 1,
                            labelStyle = list("margin-right" = "15px"),
                            inputStyle = list("margin-right" = "5px")
                        )
                    ),
                    style = list("padding" = 10)
                )
            )
        ),
        htmlBr(),
        dccGraph(id = "tab1-barchart")
    )
)

# ==============================================================================
#                            Tab 1: Map figure
# ==============================================================================
app$callback(
    output("tab1-map", "figure"),
    list(
        input("tab1-energy-type-dropdown", "value"),
        input("tab1-year-slider", "value")
    ),
    function(xcol, year) {
        df <- df %>% filter(Year == year)
        p <- plot_ly(df,
            type = "choropleth",
            locations = df$Code,
            z = df[, xcol],
            text = df$Entity,
            colorscale = "Greens"
        ) %>%
            layout(
                title = paste("Global", toString(xcol), "Consumption")
            )
        ggplotly(p)
    }
)

# ==============================================================================
#                            Tab 1: Barchart figure
# ==============================================================================
app$callback(
    output("tab1-barchart", "figure"),
    list(
        input("tab1-energy-type-dropdown", "value"),
        input("tab1-year-slider", "value"),
        input("tab1-input-topN", "value"),
        input("tab1_top_bot", "value")
    ),
    function(energy_type, year, topN, top_bot) {
        fig <- plot_ly(
            x = c("giraffes", "orangutans", "monkeys"),
            y = c(20, 14, 23),
            name = "SF Zoo",
            type = "bar"
        )
    }
)
# ==============================================================================
#                            Tab 2: Layout for sidebar
# ==============================================================================

sidebar2 <- dbcCol(list(
    htmlH3("World Energy Visualisation"),
    htmlH4("Historical Trends", style = list("color" = "#686868")),
    htmlBr(),
    htmlH5(
        "Country",
        style = list("width" = "50%", "display" = "inline-block"),
    ),
    htmlP("Select a country to visualize its trend:", style = list("color" = "#686868", "margin" = 0, "font-size" = "14px")),
    dbcCol(
        dccDropdown(
            id = "tab2-country-dropdown",
            options = list(
                list(label = "New York City", value = "NYC"),
                list(label = "Montreal", value = "MTL"),
                list(label = "San Francisco", value = "SF")
            ),
            value = "MTL"
        ),
        width = 12,
        style = list("padding" = 10),
    ),
    htmlBr(),
    htmlH5(
        "Region",
        style = list("width" = "50%", "display" = "inline-block"),
    ),
    htmlP("Select regions to compare with the country:", style = list("color" = "#686868", "margin" = 0, "font-size" = "14px")),
    dbcCol(
        dccDropdown(
            id = "tab2-region-dropdown",
            options = list(
                list(label = "New York City", value = "NYC"),
                list(label = "Montreal", value = "MTL"),
                list(label = "San Francisco", value = "SF")
            ),
            value = "MTL"
        ),
        width = 12,
        style = list("padding" = 10)
    ),
    htmlBr(),
    dbcRow(
        list(
            htmlH5(
                "Show World Trend",
                style = list("width" = "80%", "display" = "inline-block")
            ),
            dccChecklist(
                options = list(
                    list(label = "New York City", value = "NYC"),
                    list(label = "Montreal", value = "MTL"),
                    list(label = "San Francisco", value = "SF")
                ),
                value = list("MTL", "SF"),
                id = "tab2-world-toggle"
            )
        ),
        style = list("margin-left" = 10)
    )
),
style = list("max-width" = "80%"),
)

# ==============================================================================
#                            Tab 2: Layout for lineplots
# ==============================================================================
tab2_lineplots <- dbcCol(list(
    htmlP(
        "Select the year range for the below plots:"
    ),
    dccRangeSlider(
        id = "tab2-years-rangeslider",
        min = 0,
        max = 20,
        step = 0.5,
        value = list(5, 15)
    ),
    htmlBr(),
    dccGraph(id = "tab2-lineplot-fossil"),
    dccGraph(id = "tab2-lineplot-nuclear"),
    dccGraph(id = "tab2-lineplot-renewable")
))

tab2_layout <- dbcContainer(list(
    dbcRow(list(sidebar2, tab2_lineplots))
),
style = list("max-width" = "80%")
)

# ==============================================================================
#                            Tab 2: Lineplots for trends
# ==============================================================================
app$callback(
    output("tab2-lineplot-fossil", "figure"),
    list(
        input("tab2-country-dropdown", "value"),
        input("tab2-region-dropdown", "value"),
        input("tab2-world-toggle", "value"),
        input("tab2-years-rangeslider", "value")
    ),
    function(country, region, toggle, years) {
        fig <- plot_ly(
            x = c("giraffes", "orangutans", "monkeys"),
            y = c(20, 14, 23),
            name = "SF Zoo",
            type = "bar"
        )
    }
)

app$callback(
    output("tab2-lineplot-nuclear", "figure"),
    list(
        input("tab2-country-dropdown", "value"),
        input("tab2-region-dropdown", "value"),
        input("tab2-world-toggle", "value"),
        input("tab2-years-rangeslider", "value")
    ),
    function(country, region, toggle, years) {
        fig <- plot_ly(
            x = c("giraffes", "orangutans", "monkeys"),
            y = c(20, 14, 23),
            name = "SF Zoo",
            type = "bar"
        )
    }
)
app$callback(
    output("tab2-lineplot-renewable", "figure"),
    list(
        input("tab2-country-dropdown", "value"),
        input("tab2-region-dropdown", "value"),
        input("tab2-world-toggle", "value"),
        input("tab2-years-rangeslider", "value")
    ),
    function(country, region, toggle, years) {
        fig <- plot_ly(
            x = c("giraffes", "orangutans", "monkeys"),
            y = c(20, 14, 23),
            name = "SF Zoo",
            type = "bar"
        )
    }
)

# ==============================================================================
#                            Main skeleton of the app
# ==============================================================================
app$layout(
    dbcContainer(
        dbcRow(
            list(
                sidebar1,
                dbcCol(
                    list(
                        dbcTabs(
                            list(dbcTab(tab1_plots, label = "Map view"), dbcTab(tab2_layout, label = "Trends"))
                        )
                    )
                )
            )
        ),
        style = list("max-width" = "80%")
    )
)

app$run_server(debug = T) # Temporary for local development, delete this string when app will be deployed in heroku
# app$run_server(host = '0.0.0.0')