#Shiny app that allows you to select your variables and predict the future tuition cost based on historical data

library(tidytuesdayR)
library(shiny)
library(ggrepel)
library(plotly)
library(ggplot2)
library(rstatix)

#Read in the dataset
historical_tuition <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/historical_tuition.csv')

#Modify year 
historical_tuition$yearmod <- sapply(strsplit(historical_tuition$year, split='-', fixed=TRUE), function(x) (x[1]))
historical_tuition$yearmod <- as.integer(historical_tuition$yearmod)

#Assign our unique input variables
Type <- unique(historical_tuition$type)
TuitionType <- unique(historical_tuition$tuition_type)

# Define UI for application that draws a histogram, creates a linear model, and applies that model
ui <- fluidPage(

    # Application title
    titlePanel("Prediction of Tuition Cost"),

    # Generate a row with a sidebar
    sidebarLayout(      
        
        # Define the sidebar with three inputs
        sidebarPanel(

            #Filter the dataset
            helpText("Filters:"),
            
            selectInput("Type", "Select a Type:", choices = c(Type)),
            
            selectInput("TuitionType", "Select a Tuition Type:", choices = c(TuitionType)),
            
            helpText('Constant tuition types have been adjusted for inflation. Current represents the cost of tuition for that school year.'),
            
            #Get a value of year to predict
            numericInput("estyear", "Enter a Year to Estimate:", 2020, min = 1980, max = 2200),
            verbatimTextOutput("value"),
            
            helpText("For the 2017–18 academic year, annual current dollar prices for undergraduate tuition, fees, room, and board were estimated to be $17,797 at public institutions, $46,014 at private nonprofit institutions, and $26,261 at private for-profit institutions."),
            helpText(" Between 2007–08 and 2017–18, prices for undergraduate tuition, fees, room, and board at public institutions rose 31 percent, and prices at private nonprofit institutions rose 23 percent, after adjustment for inflation."),
            helpText("The price for undergraduate tuition, fees, room, and board at private for-profit institutions decreased 9 percent between 2007–08 and 2017–18, after adjustment for inflation."),
            helpText('SOURCE: U.S. Department of Education, National Center for Education Statistics. (2019). Digest of Education Statistics, 2018 (NCES 2020-009), Chapter 3.')
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h2(textOutput("Caption")),
           
            tabsetPanel(
                tabPanel("Historical Plot", plotlyOutput("Plot")),
                tabPanel("Model", verbatimTextOutput("Model")),
                tabPanel("Predicted Cost", verbatimTextOutput("txt"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    myData = reactive({
        Set <- historical_tuition 
        
        if(input$Type != "All")
        {
            Set <- subset(Set, Set$type == input$Type)
        }
        
        if(input$TuitionType != "All")
        {
            Set <- subset(Set, Set$tuition_type == input$TuitionType)
        }
        
        return(Set)
    })

    #Render a plotly plot of historical tuition
    output$Plot = renderPlotly({
        myData()
        p <- ggplot(data = myData(), mapping = aes(x = yearmod)) +
            geom_line(aes(y = tuition_cost)) + 
            geom_point(aes(y = tuition_cost))+
            theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
            ggtitle(paste(input$Type,"Tuition over Time for Tuition Type:", input$TuitionType))+
            theme(plot.title = element_text(hjust = 0.5))+ 
            xlab("Year") + 
            ylab("College Tuition Cost") 
        ggplotly(p, tooltip = "text")
        
    })
    
    #Create a linear model based off of data from filters
    output$Model = renderPrint({
        myData()
        model <- lm(tuition_cost ~ yearmod, data = myData())
        summary(model)
        #predict(model, data.frame(yearmod = get(input$estyear)))
    })
    
    #Apply the model to predict future tuition
    output$txt <- renderText({
        myData()
        model <- lm(tuition_cost ~ yearmod, data = myData())
        paste0("The estimated ",input$Type, " ", input$TuitionType, " tuition cost of ", input$estyear, " is $", trunc(predict(model, data.frame(yearmod = input$estyear))))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
