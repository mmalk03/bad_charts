library(shiny)
library(shinydashboard)
library(shinyjs)
library(reshape2)
library(dplyr)
library(ggplot2)
library(SmarterPoland)
library(latticeExtra)
library(plotrix)
library(tidyr)
library(ggsci)
library(gridExtra)

source('ui/pie_chart_1_ui.r')
source('ui/pie_chart_2_ui.r')

source('data/pie_chart_1.r')
source('data/pie_chart_2.r')

ui <- dashboardPage(
    skin = 'black',
    dashboardHeader(
        title = 'Bad charts'
    ),
    dashboardSidebar(
        sidebarMenu(id='tabs',
            menuItem('Pie chart 1', tabName = 'pie_chart_1', icon = icon('chart-pie')),
            menuItem('Pie chart 2', tabName = 'pie_chart_2', icon = icon('chart-pie'))
        )
    ),
    dashboardBody(
        shinyjs:::useShinyjs(),
        tabItems(
            tab_item_pie_chart_1,
            tab_item_pie_chart_2
        )
    )
)

server <- function(input, output, session) {

    output[['bad_pie_chart_1']] <- renderPlot({
        data(countries)
        population_by_continent <- countries %>%
            select(population, continent) %>%
            group_by(continent) %>%
            summarise(population = round(sum(population) / sum(countries$population) * 100)) %>%
            arrange(desc(population)) %>%
            mutate(continent = factor(continent, levels = continent))

        pie3D(population_by_continent$population, labels = population_by_continent$continent, main = "Population percentage by continent",
            explode = 0.1, radius = .9, labelcex = 1.2, height = 0.2, shade=0.5, start = 0, mar=c(0,0,0,6),
            col=pal_jco()(5))

    })
    output[['good_pie_chart_1']] <- renderPlot({ good_pie_chart_1_plot })
    output[['bad_pie_chart_2']] <- renderPlot({ bad_pie_chart_2_plot })
    output[['good_pie_chart_2']] <- renderPlot({ good_pie_chart_2_plot })

    observeEvent(input$action_button_pie_chart_1, {
        if(input$action_button_pie_chart_1 %% 2 == 0) {
            shinyjs::hide(id = 'box_pie_chart_1_good_viz')
            shinyjs::hide(id = 'box_pie_chart_1_actual_values')
        } else {
            shinyjs::show(id = 'box_pie_chart_1_good_viz')
            shinyjs::show(id = 'box_pie_chart_1_actual_values')
        }
    })

    observeEvent(input$action_button_pie_chart_2, {
        if(input$action_button_pie_chart_2 %% 2 == 0) {
            shinyjs::hide(id = 'box_pie_chart_2_good_viz')
            shinyjs::hide(id = 'box_pie_chart_2_actual_values_1')
            shinyjs::hide(id = 'box_pie_chart_2_actual_values_2')
        } else {
            shinyjs::show(id = 'box_pie_chart_2_good_viz')
            shinyjs::show(id = 'box_pie_chart_2_actual_values_1')
            shinyjs::show(id = 'box_pie_chart_2_actual_values_2')
        }
    })
}

shinyApp(ui, server)
