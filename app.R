#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

vaccini <- read.csv(file = 'data/somministrazioni-vaccini-summary-latest.csv')
vaccini$data_somministrazione <- as.Date(vaccini$data_somministrazione, format = "%Y-%m-%d")

#create UI
ui <- fluidPage(
    titlePanel(h1('Vaccini COVID-19', style ='color:orangered; background-color:khaki;
                     padding-left: 15px')),
    sidebarLayout(
        sidebarPanel(
            selectInput('selectRegion', 'Seleziona Regione', choices = c("Abruzzo","Basilicata","Calabria","Campania",'Emilia-Romagna',"Friuli-Venezia Giulia",'Lazio',"Liguria", "Lombardia","Marche","Molise" ,'Piemonte', "Provincia Autonoma Bolzano / Bozen","Provincia Autonoma Trento", 'Puglia',"Sardegna","Sicilia","Toscana","Umbria","Veneto","Valle d'Aosta")),
            dateRangeInput('selectDate', 'Seleziona Data', start = min(vaccini$data_somministrazione), end = max(vaccini$data_somministrazione))
        ),
        mainPanel((shiny::tabsetPanel (type="tabs",
                                       shiny::tabPanel("Vaccini somministrati nella regione selezionata per periodo selezionato",
                                                       h4("Prima dose", style ='color:orange'),verbatimTextOutput("mtble"),
                                                       h4("Seconda dose", style ='color:coral'),verbatimTextOutput(outputId = "mtble2"),
                                                       ("Prima dose vs Seconda dose"), plotOutput(outputId = "Graph",width = "70%"),
                                                       ("Totale vaccini somministrati"), plotly::plotlyOutput(outputId = "Graph2")
                                                       
                                                       
                                       ))))
    ))

#create server

server <- function(input, output){
    
    datasetInput <- reactive({
        
        dates <- input$selectDate
        startDate <- dates[1]
        endDate <- dates[2]
        region <- input$selectRegion
        newdata <- vaccini[(vaccini$data_somministrazione>=startDate & vaccini$data_somministrazione <= endDate),]
    })
    
    #function to make sum of selected data in future outputs
    myfunction <- function(x,y){
        paste(with(x, sum(y[x$nome_area==input$selectRegion])))
    }
    
    #outputs
    output$mtble <- renderText({
        a <- datasetInput()
        b <- a$prima_dose
        myfunction(a,b)
    })
    
    output$mtble2  <- renderText({
        c <- datasetInput()
        d <- c$seconda_dose
        myfunction(c,d)
    })
    
    
    output$Graph <- renderPlot({
        
        c <- datasetInput()
        
        primadose <- with(c, sum(c$prima_dose[c$nome_area==input$selectRegion]))
        secondadose <- with(c, sum(c$seconda_dose[c$nome_area==input$selectRegion]))
        k <- data.frame(primadose,secondadose)
        k <- t(k)
        barplot(k,
                space=0.3,
                col = c("orange", "coral"),
                width = c(1,1),
                legend = rownames(k), beside = TRUE,
                cex.axis=0.3
                
        )
        
    })
    
    
    output$Graph2 <- plotly::renderPlotly({
        
        
        d <- datasetInput()
        d <- d[(d$nome_area==input$selectRegion),]
        data_di_somministrazione <- d$data_somministrazione
        
        
        p <- ggplot2::ggplot() +
            ggplot2::geom_point(data=d,ggplot2::aes(x = data_di_somministrazione, y= totale), color="orange" ) +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1)) +
            ggplot2::labs(y="Data di somministrazione", x="Totale di vaccini")
        
        
        plotly::ggplotly(p)
        p
        
    })
    
}

shinyApp(ui = ui, server = server)



