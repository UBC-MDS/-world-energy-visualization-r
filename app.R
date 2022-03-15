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



# ==============================================================================
#                            Styles
# ==============================================================================
sidebar_style3 = list(#"max-width" = "25%", 
					  "background-image" = "url(/assets/wind-energy.jpg)",
					  "bottom" = 0,
					  "top" = 0,
					  "display" = "none"
					  )

# ==============================================================================
#                            Tab 1: Layout for sidebar
# ==============================================================================
sidebar1 <- div(
    list(
        htmlH3("World Energy Visualisation"),
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
        dccGraph(id = "tab1-map"),
		dccSlider(
			id = "tab1-year-slider",
			min = min(df$Year),
			max = max(df$Year),
			step = 1,
			value = max(df$Year),
			marks = year_range,
			tooltip = list(always_visible = TRUE, placement = "top") 
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
        input("tab1-year-slider", "value")
    ),
    function(energy_type, year) {
        df <- df %>% 
		       filter(Year == year)
        p <- plot_ly(df,
            type = "choropleth",
            locations = df$Code,
            z = df[, energy_type],
            text = df$Entity,
            colorscale = "Greens"
        ) %>% 
		layout(title = paste("Global", toString(energy_type), "Consumption"))
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
    limits = c(0, 100), direction = 1) + 
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

sidebar2 <- div(
    list(
        htmlH3("World Energy Visualisation"),
        htmlBr(),
        htmlH5(
            "Country",
            style = list("width" = "80%", "display" = "inline-block"),
        ),
        htmlP("Select a country to visualize its trend:", style = list("color" = "#686868", "margin" = 0, "font-size" = "14px")),
        dbcCol(
            dccDropdown(
                id = "tab2-country-dropdown",
                options = list(
                    list(label = "giraffes", value = "giraffes"),
                    list(label = "orangutans", value = "orangutans"),
                    list(label = "monkeys", value = "monkeys")
                ),
                value = "giraffes"
            ),
            style = list("margin-left" = 10),
        ),
        htmlBr(),
        htmlH5(
            "Region",
            style = list("width" = "80%", "display" = "inline-block"),
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
            style = list("margin-left" = 10)
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
                        list(label = "", value = 1)
                    ),
                    value = list(1),
                    id = "tab2-world-toggle"
                )
            ),
            style = list("margin-left" = 10)
        )
    )
)

# ==============================================================================
#                            Tab 2: Layout for lineplots
# ==============================================================================
tab2_lineplots <- dbcCol(list(
	htmlDiv(list(
		htmlP(
			"Select the year range for the below plots:"
		),
		dccRangeSlider(
            id = "tab2-years-rangeslider",
            min = min(df$Year),
            max = max(df$Year),
            step = 1,
            value = list(1980, 2010),
            marks = year_range,
            tooltip = list(always_visible = TRUE, placement = "top")
	    )
	)),
		
	htmlBr(),
	dccGraph(id = "tab2-lineplot-fossil"),
	dccGraph(id = "tab2-lineplot-nuclear"),
	dccGraph(id = "tab2-lineplot-renewable")
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
        input("tab2-years-rangeslider", "value")
    ),
    function(country, region, toggle, years) {
        fig <- plot_ly(
            x = country,
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
                       style=list(
                           "background-image" = "url(/assets/wind-energy.jpg)"
                           )
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
                                            label = "Global Distribution",
                                            tab_id = "tab-1"
                                        ),
                                        dbcTab(
                                            tab2_lineplots,
                                            label="Historical trends",
                                            tab_id = "tab-2"
                                        )
                                    )
                                )
                            )
                        )
                    ),
                    md=10
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


app$run_server(host = '0.0.0.0', debug = T) # Temporary for local development, delete this string when app will be deployed in heroku
# app$run_server(host = '0.0.0.0')

















