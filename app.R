library(dash)
library(dashBootstrapComponents)
library(dashHtmlComponents)
library(plotly)
library(purrr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(tidyr)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)
app$title("World Energy Visualization")


# ==============================================================================
#                            Data wrangling
# ==============================================================================
df <- read.csv("data/Primary-energy-consumption-from-fossilfuels-nuclear-renewables.csv") #%>% drop_na()
#df <- na.omit(head(df))
df_na <- df %>% filter(Code != "") %>% pivot_longer(c(Fossil, Renewables, Nuclear), names_to="energy_type", values_to="percentage")


year_range <- seq(min(df$Year), max(df$Year), 5)
year_range <- setNames(as.list(as.character(year_range)), as.integer(year_range))

all_country <- df %>%
  filter(Code != "" & Entity != "World") %>%
  pull(Entity) %>%
  unique()

all_continent <- df %>%
  filter(Code == "") %>%
  pull(Entity) %>%
  unique()


all_years <- df$Year %>%
  unique()

proj_param <- list(
  "World" = c(0, 0, 1),
  "North America" = c(40, -120, 2),
  "Europe" = c(50, 20, 4),
  "Africa" = c(0, 20, 2)
)
# ==============================================================================
#                            Styles
# ==============================================================================
sidebarStyle = list(
  "position"= "fixed",
  "top"= 0,
  "left"= 0,
  "bottom"= 0,
  "padding"= "2rem 1rem",
  "background-image"= "url(/assets/wind-energy.jpg)",
  "background-color" = "rgba(255, 255, 255, 0.6)",
  "background-blend-mode" = "overlay"
)

sidebar_style3 = list(#"max-width" = "25%",
					  "background-image" = "url(/assets/wind-energy.jpg)",
					  "bottom" = 0,
					  "top" = 0,
					  "display" = "none"
					  )

tabStyle = list(
  "position"="fixed",
  "top"= 0,
  "right"= 20,
  "bottom"= 0,
  "width" = "80%",
  "padding"= "2rem 1rem",
  "overflow-y"= "scroll"
)
# ==============================================================================
#                            Tab 1: Layout for sidebar
# ==============================================================================
sidebar1 <- dbcCol(
    list(
        htmlH3("World Energy Visualisation"),
        html$h4("Global Distribution", style = list("color" = "#686868")),
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
                    purrr::map(function(col) list(label = col, value = toString(col))),
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
    )
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
        dbcRow(list(
          html$div("Map View:", style=list("width"= "fit-content", "padding"= "5px 0")),
          dbcCol(
            dccDropdown(
              id="tab1-map-focus",
              options = c("World", all_continent),
              value="World",
              clearable=F
            ),
          )), style=list("width" = "20%", "padding-left" = "10px")
        ),
        dccGraph(id = "tab1-map"),
        html$div(
      		dccSlider(
      			id = "tab1-year-slider",
      			min = min(df$Year),
      			max = max(df$Year),
      			step = 1,
      			value = max(df$Year),
      			marks = year_range,
      			tooltip = list(always_visible = TRUE, placement = "top"),
      		),
      		style = list("padding" = "0vh 10vw")
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
                            value=10,
                            type = "number",
                            debounce=T,
                            required=T,
                            minlength=1
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
                                list("label" = "Top", "value" = "Top"),
                                list("label" = "Bottom", "value" = "Bottom")
                                ),
                            value="Top",
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
        input("tab1-year-slider", "value"),
        input("tab1-map-focus", "value")
    ),
    function(energy_type, year, scope) {
        df <- df %>%
		       filter(Year == year)

        p <- plot_ly(df,
            type = "choropleth",
            locations = df$Code,
            z = df[, energy_type],
            text = df$Entity,
            colorscale = "Greens",
            reversescale = TRUE,
            zauto = FALSE,
            zmin = 0,
            zmax = 100
        ) %>%
		      layout(title = paste("Global", toString(energy_type), "Energy Consumption in", toString(year)),
		             geo = list(showcountries = T,
		                        center = list("lat" = proj_param[[scope]][1], "lon" = proj_param[[scope]][2]),
		                        projection = list("scale" = proj_param[[scope]][3])
		                        ))
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


    function(energy, year, topN, top_bot, df=df_na) {


    if (top_bot == "Top"){
        df_fil <- df %>% filter(
            Year == year & energy_type == energy) %>% arrange(desc(percentage)) %>% slice_max(order_by=percentage, n=topN)

    } else if (top_bot == "Bottom") {

       df_fil <- df %>% filter(
            Year == year & energy_type == energy) %>% arrange(desc(percentage)) %>% slice_min(order_by=percentage, n=topN, with_ties=F)

        }


    bar_chart <- ggplot(
        df_fil,
        aes(x=percentage,
            y=reorder(Entity, -percentage),
           fill=percentage)) +
    geom_bar(stat='identity') +
    geom_text(aes(label = round(percentage, 1)), colour = "black") +
    labs(x="Percentage %",
     y="Country") +
    scale_fill_distiller(palette= "Greens",
    limits = c(0, 100)) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 102)) +
    theme(legend.position="none")

    if (top_bot == "Top"){
       bar_chart <- bar_chart + ggtitle(paste0("Top ", topN, " ", energy, " Energy Consumers in ", year))

    } else if (top_bot == "Bottom"){

      bar_chart <- bar_chart + ggtitle(paste0("Bottom ", topN, " ", energy, " Energy Consumers in ", year))
    }

    ggplotly(bar_chart)

    }


)
# ==============================================================================
#                            Tab 2: Layout for sidebar2
# ==============================================================================

sidebar2 <- dbcCol(list(
  html$h3("World Energy Visualisation"),
  html$h4("Historical Trends", style = list("color" = "#686868")),
  html$br(),
  html$h5("Country", style = list("width" = "50%", "display" = "inline-block")),
  html$p("Select a country to visualize its trend:", style=list("color" = "#686868", "margin" = 0, "font-size" = "14px")),
  dccDropdown(id = "tab2-country-dropdown", options = all_country, value = "Canada"),
  html$br(),
  html$h5("Region", style = list("width" = "50%", "display" = "inline-block")),
  html$p("Select regions to compare with the country:", style=list("color" = "#686868", "margin" = 0, "font-size" = "14px")),
  dccDropdown(id = "tab2-region-dropdown", options = all_continent, value = "North America", multi = TRUE),
  html$br(),
  dbcRow(list(
    html$h5("Show World Trend", style = list("width" = "80%", "display" = "inline-block")),
    dbcChecklist(options = list(list("label" = "", "value" = 1)), value = list(1), id = "tab2-world-toggle", switch = TRUE)
  ))
))

# ==============================================================================
#                            Tab 2: Layout for lineplots
# ==============================================================================
slider_marks <- list()
show_years <- all_years[c(seq(1, length(all_years), 5), length(all_years))]
for (y in show_years){
  slider_marks[as.character(y)] = as.character(y)
}

tab2_lineplots <- dbcCol(list(
  html$div(list(
    html$p("Select the year range for the below plots:", style = list("color" = "#888888")),
    dccRangeSlider(min = min(all_years), max = max(all_years), step = 1,
                   value = c(min(all_years), max(all_years)),
                   tooltip = list("placement" = "top", "always_visible" = FALSE),
                   marks = slider_marks,
                   id = "tab2-year-slider"),
    dccGraph(id = "tab2-lineplot-fossil"),
    dccGraph(id = "tab2-lineplot-nuclear"),
    dccGraph(id = "tab2-lineplot-renewable")
  ), style = list("padding-top" = "30px"))
))


# ==============================================================================
#                            Tab 2: Lineplots for trends
# ==============================================================================
app$callback(
  output("tab2-lineplot-fossil", "figure"),
  list(
    input("tab2-country-dropdown", "value"),
    input("tab2-region-dropdown", "value"),
    input("tab2-world-toggle", "value"),
    input("tab2-year-slider", "value")
  ),
  function(country, region, toggle, years){
    entity_vec <- c(country, region %>% as.character())
    if (length(toggle) > 0) {
      entity_vec <- c(entity_vec, "World")
    }

    data_use <- df %>%
      filter(Entity %in% entity_vec & Year >= years[1] & Year <= years[2])

    graph <- ggplot(data_use, aes(x = Year, y = Fossil, color = Entity)) +
      geom_line() +
      labs(title = paste("Fossil fuels usage from", years[1], "to", years[2]),
           y = "Fossil fuel Usage (%)")

    ggplotly(graph)
  }
)

app$callback(
  output("tab2-lineplot-nuclear", "figure"),
  list(
    input("tab2-country-dropdown", "value"),
    input("tab2-region-dropdown", "value"),
    input("tab2-world-toggle", "value"),
    input("tab2-year-slider", "value")
  ),
  function(country, region, toggle, years){
    entity_vec <- c(country, region %>% as.character())
    if (length(toggle) > 0) {
      entity_vec <- c(entity_vec, "World")
    }

    data_use <- df %>%
      filter(Entity %in% entity_vec & Year >= years[1] & Year <= years[2])

    graph <- ggplot(data_use, aes(x = Year, y = Nuclear, color = Entity)) +
      geom_line() +
      labs(title = paste("Nuclear energy usage from", years[1], "to", years[2]),
           y = "Nuclear energy Usage (%)")

    ggplotly(graph)
  }
)

app$callback(
  output("tab2-lineplot-renewable", "figure"),
  list(
    input("tab2-country-dropdown", "value"),
    input("tab2-region-dropdown", "value"),
    input("tab2-world-toggle", "value"),
    input("tab2-year-slider", "value")
  ),
  function(country, region, toggle, years){
    entity_vec <- c(country, region %>% as.character())
    if (length(toggle) > 0) {
      entity_vec <- c(entity_vec, "World")
    }

    data_use <- df %>%
      filter(Entity %in% entity_vec & Year >= years[1] & Year <= years[2])

    graph <- ggplot(data_use, aes(x = Year, y = Renewables, color = Entity)) +
      geom_line() +
      labs(title = paste("Renewable energy usage from", years[1], "to", years[2]),
           y = "Renewable energy Usage (%)")

    ggplotly(graph)
  }
)



# ==============================================================================
#                       Tab 3 - A placeholder
# ==============================================================================

sidebar3 <- dbcCol(
    list(
        dccDropdown(
            id = "tab2-country-dropdown",
            options = list(
                list(label = "Location1", value = "L1")
            ),
            value = "L1"
        ),
        dccDropdown(
                id = "tab2-region-dropdown",
                options = list(
                    list(label = "Location1", value = "L1")
                ),
                value = "L1"
            ),
        dccChecklist(
            options = list(
                list(label = "", value = 1)
            ),
            value = list(1),
            id = "tab2-world-toggle"
        )
    ),
    style = sidebar_style3
)

# ==============================================================================
#                            Main skeleton of the app
# ==============================================================================
app$layout(
    dbcContainer(
        dbcRow(
            list(
                dbcCol(
                    list(
                        dbcRow(
                            id="sidebar1",
                            sidebar1
                        ),
                        dbcRow(
                            id="sidebar3", #Placeholder
                            sidebar3,
                        )
                    ),
                       md=2,
                       style=sidebarStyle
                       ),
                dbcCol(
                    list(
                        dbcRow(
                            list(
                                dbcTabs(
                                    id="tabs",
                                    list(
                                        dbcTab(
                                            tab1_plots,
                                            label = "Map view",
                                            tab_id = "tab-1"
                                        ),
                                        dbcTab(
                                            tab2_lineplots,
                                            label="Trends",
                                            tab_id = "tab-2"
                                        )
                                    )
                                )
                            )
                        )
                    ),
                    md=10,
                    style = tabStyle
                )
            )
        ),
        fluid=TRUE
    )
)

app$callback(
    output( id="sidebar1", "children"),
    list(input("tabs", "active_tab")),
    function(at) {
        if (at == "tab-1") {
            return(sidebar1)
        } else if (at == "tab-2") {
            return(sidebar2)
        }
    }
)


# app$run_server(host = '0.0.0.0', debug = T) # Temporary for local development, delete this string when app will be deployed in heroku
app$run_server(host = '0.0.0.0')

















