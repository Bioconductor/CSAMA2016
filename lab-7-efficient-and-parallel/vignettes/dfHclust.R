dfHclust = function (df, labels) 
{
#
# crude interactive interface for hierarchical clustering
# author VJ Carey <stvjc@channing.harvard.edu>
#
    stopifnot(inherits(df, "data.frame"))
    stopifnot(length(labels)==nrow(df))
    stopifnot(ncol(df) > 1)
    require(shiny)
    require(cluster)
    nms = names(df)
    cmeths = c("ward.D", "ward.D2", "single", "complete", "average", 
        "mcquitty", "median", "centroid")
    dmeths = c("euclidean", "maximum", "manhattan", "canberra", 
        "binary")
    ui <- fluidPage(titlePanel(paste(substitute(df), "hclust")), 
        sidebarPanel(helpText(paste("Select distance:")), fluidRow(selectInput("dmeth", 
            NULL, choices = dmeths, selected = dmeths[1])), helpText(paste("Select clustering method:")), 
            fluidRow(selectInput("meth", NULL, choices = cmeths, 
                selected = cmeths[1])), helpText(paste("Select height for cut:")), 
            fluidRow(numericInput("cutval", NULL, value = 4, 
                min = 0, max = Inf, step = 1)), helpText(paste("Select variables for clustering from", 
                substitute(df), ":")), fluidRow(checkboxGroupInput("vars", 
                NULL, choices = nms, selected = nms[1:3]))), 
        mainPanel(tabsetPanel(tabPanel("tree", plotOutput("plot1")), 
            tabPanel("pairs", plotOutput("pairsplot")), tabPanel("silh", 
                plotOutput("silplot")))))
    server <- function(input, output) {
        output$plot1 <- renderPlot({
            xv = df[, input$vars]
            plot(hclust(dist(data.matrix(xv), method = input$dmeth), 
                method = input$meth), xlab = paste(input$dmeth, 
                "distance;", input$meth, "clustering"), labels = labels)
            abline(h = input$cutval, lty = 2, col = "gray")
        })
        output$pairsplot <- renderPlot({
            xv = df[, input$vars]
            pairs(data.matrix(xv))
        })
        output$silplot <- renderPlot({
            xv = df[, input$vars]
            dm = dist(data.matrix(xv), method = input$dmeth)
            hc = hclust(dist(data.matrix(xv), method = input$dmeth), 
                method = input$meth)
            ct = cutree(hc, h = input$cutval)
            plot(silhouette(ct, dm))
        })
    }
    shinyApp(ui, server)
}
