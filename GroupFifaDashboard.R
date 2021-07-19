
# 6070 - Data Vizualization Winter A 2020
# Professor: Susan Gracia
# Due: 02/8/2020
# Group Rshiny Dashboard presenting FIFA player statistics

# ------------------------------------------------------------------------------------------------------------------

library(kableExtra)
library(shiny)
library(cowplot)
library(ggplot2)
library(dplyr)
library(maps)
library(highcharter)
bcl <- read.csv("/Users/karan/Desktop/Fifa19Analysis(ALY6040)/data.csv")

Original_bcl <- bcl

Fixed_bcl <- Original_bcl %>% 
    mutate(
        Value = case_when(
            grepl("M", bcl$Value) ~ as.numeric(substring(bcl$Value, 2, nchar(as.character(bcl$Value))-1)) * 1000000,
            grepl("K", bcl$Value) ~ as.numeric(substring(bcl$Value, 2, nchar(as.character(bcl$Value))-1)) * 1000,
            TRUE ~ as.numeric(bcl$Value)
        ),
        Wage = case_when(
            grepl("M", bcl$Wage) ~ as.numeric(substring(bcl$Wage, 2, nchar(as.character(bcl$Wage))-1)) * 1000000,
            grepl("K", bcl$Wage) ~ as.numeric(substring(bcl$Wage, 2, nchar(as.character(bcl$Wage))-1)) * 1000,
            TRUE ~ as.numeric(bcl$Wage)
        ),
        Release.Clause = case_when(
            grepl("M", bcl$Release.Clause) ~ as.numeric(substring(bcl$Release.Clause, 2, nchar(as.character(bcl$Wage))-1)) * 1000000,
            grepl("K", bcl$Release.Clause) ~ as.numeric(substring(bcl$Release.Clause, 2, nchar(as.character(bcl$Wage))-1)) * 1000,
            TRUE ~ as.numeric(bcl$Release.Clause)
        )   
    )

bcl$Value=Fixed_bcl$Value
bcl$Wage=Fixed_bcl$Wage
bcl$Release.Clause=Fixed_bcl$Release.Clause

cat('Converted into integer ')
View(Fixed_bcl)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("FIFA 2019"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(sliderInput("valueInput", "Player Salary:", min = 10000, max = 118500000,
                                 value = c(1000,40000000), pre = "$"),
                     sliderInput("ageInput", "Age", min = 16, max = 45,
                                 value = c(25,40), pre = ""),
                     radioButtons("footInput", "Preferred Foot",
                                  choices = c("Left", "Right"),
                                  selected = "Right"),
                     selectInput("clubInput", "Club:",
                                 choices = Fixed_bcl$Club),
                     plotOutput("coolplot")
                    
                
                   
                     
                     ),
        mainPanel(
      
          plotOutput("bar"), 
                  br(), br(), 
                  tableOutput("results")
          )
    )

)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$coolplot <- renderUI({
        selectInput("clubInput", "Club", 
                    sort(unique(Fixed_bcl$Club)),
                    selected = "Juventus")
    })
    filtered <- reactive({
        if (is.null(input$clubInput)) {
            return(NULL)
        }
        bcl %>%
            filter(Value >= input$valueInput[1],
                   Value <= input$valueInput[2],
                   Age >= input$ageInput[1],
                   Age <= input$ageInput[2],
                   Preferred.Foot == input$footInput,
                   Club == input$clubInput
            )
    })
    output$coolplot <- renderPlot({
        if(is.null(filtered())){
            return()
        }
        require(scales)
        players_plot <- filtered() %>%
            group_by(Value)%>%
            arrange(desc(Value))%>%
            head(10)%>%
            ggplot(mapping = aes(x = Name, y = Value, color = Nationality, fill = Nationality, alpha = Value, size=Value))+
            geom_bar(stat='identity')+
            coord_polar()+
            theme_minimal()+
            labs(x = 'Name', y ='Value', title ='Highest Valued Players')+
            theme(plot.title = element_text(hjust=0.5),legend.position ='bottom')+
            scale_y_continuous(labels = comma)
        
        players_plot
    })
    output$bar <- renderPlot({
      ggplot(filtered(), aes(Position, Value, colour = factor(Position))) + 
        geom_boxplot(show.legend = FALSE) +
        xlab("Player Position") +
        ylab("Player Salaries €") +
        ggtitle("Salary distribution accross Team Positions")
      
    })
    
    output$results <- function(){
      filtered() %>%
        kable() %>%
        kable_styling(bootstrap_options = c("striped", "hover"))
    }
}
shinyApp(ui = ui, server = server)
