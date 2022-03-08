library(dash)
library(dashHtmlComponents)
library(plotly)
library(purrr)
library(tidyverse)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

df <- read.csv("data/Primary-energy-consumption-from-fossilfuels-nuclear-renewables.csv") %>%
    drop_na()

year_range <- seq(min(df$Year), max(df$Year),5)
year_range <- setNames(as.list(as.character(year_range)), as.integer(year_range))

app$layout(
    dbcContainer(
            list(
                htmlBr(),
                dccGraph(id='plot-area'),
                dccSlider(
                    id="slider-year",
                    min=min(df$Year),
                    max=max(df$Year),
                    step=1,
                    value=max(df$Year),
                    marks=year_range,
                    tooltip=list(
                        always_visible=TRUE,
                        placement="top"
                        )
                ),
                htmlBr(),
                htmlP("Engery type"),
                dccDropdown(
                    id='col-select',
                    options = df %>%
                        select(Fossil, Nuclear, Renewables) %>%
                        colnames() %>%
                        purrr::map(function(col) list(label = col, value = col)), 
                    value='Fossil')
        )
    )
)

app$callback(
    output('plot-area', 'figure'),
    list(input('col-select', 'value'),
         input("slider-year", "value")),
    function(xcol, year) {
        df = df %>% filter(Year==year)
        p <- plot_ly(df, type='choropleth', 
                     locations=df$Code, 
                     z=df[,xcol], 
                     text=df$Entity, 
                     colorscale="Greens") %>%
            layout(
                title = paste( "Global", toString(xcol), "Consumption")
            )
        ggplotly(p)
    }
)

app$run_server(host = '0.0.0.0')