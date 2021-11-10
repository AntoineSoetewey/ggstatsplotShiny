set.seed(123)
library(ggstatsplot)
library(shiny)
library(rlang)
library(ggplot2)
library(palmerpenguins)

ui <- fluidPage(
    titlePanel("{ggstatsplot} example"),
    h4(tags$a(href = "https://www.antoinesoetewey.com/", "Antoine Soetewey")),
    sidebarPanel(
        selectInput("x", "Grouping variable", "X Variable", choices = names(penguins)[c(1, 2, 7, 8)]),
        selectInput("y", "Dependent variable", "Y Variable", choices = names(penguins)[3:6]),
        hr(),
        selectInput("paired", "Samples", choices = c("independent", "paired")),
        selectInput("type", "Type", choices = c("parametric", "nonparametric", "robust", "bayes")),
        selectInput("p.adjust.method", "P-values adjustment method", choices = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")),
        hr(),
        selectInput("plot.type", "Plot type", choices = c("boxplot" = "box", "boxplot & violin plot" = "boxviolin", "violin plot" = "violin")),
        checkboxInput("pairwise.comparisons", "Pairwise comparisons?", value = TRUE),
        selectInput("pairwise.display", "Pairwise display", choices = c("significant", "all", "non-significant")),
        checkboxInput("centrality.plotting", "Centrality plotting?", value = FALSE),
        checkboxInput("remove.caption", "Remove caption?", value = TRUE),
        hr(),
        HTML('<p>Report a <a href="https://github.com/AntoineSoetewey/ggstatsplotShiny/issues">bug</a> or view the <a href="https://github.com/AntoineSoetewey/ggstatsplotShiny">code</a>. See more information in this <a href="https://statsandr.com/blog/how-to-do-a-t-test-or-anova-for-many-variables-at-once-in-r-and-communicate-the-results-in-a-better-way/">article</a>.</p><p>Back to <a href="https://www.antoinesoetewey.com/">antoinesoetewey.com</a> or <a href="https://statsandr.com/">statsandr.com</a>.</p>')
    ),
    mainPanel(plotOutput("plot"))
)

server <- function(input, output) {
    
    output$plot <- renderPlot({
        if (input$paired == "paired") {
            p <- ggwithinstats(penguins,
                          !!input$x,
                          !!input$y,
                          plot.type = input$plot.type,
                          type = input$type,
                          pairwise.comparisons = input$pairwise.comparisons,
                          pairwise.display = input$pairwise.display,
                          p.adjust.method = input$p.adjust.method,
                          bf.message = FALSE,
                          centrality.plotting = input$centrality.plotting)
        } else {
            p <- ggbetweenstats(penguins,
                           !!input$x,
                           !!input$y,
                           plot.type = input$plot.type,
                           type = input$type,
                           pairwise.comparisons = input$pairwise.comparisons,
                           pairwise.display = input$pairwise.display,
                           p.adjust.method = input$p.adjust.method,
                           bf.message = FALSE,
                           centrality.plotting = input$centrality.plotting)
            
        }
        if (input$remove.caption) {
            p + labs(caption = NULL)
        } else {
            p
        }
    })
}

shinyApp(ui, server)
