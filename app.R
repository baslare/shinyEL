require(shiny)
require(shinyjs)
require(shinythemes)
require(tidyverse)
require(ggsci)
require(hexbin)
source("hexFunctions.R")
source("global.R")


listSeasons<- as.list(unique(data$season))
#listNames <- as.list(unique(data$name))
#listTeams <- as.list(unique(data$team))
sciCols <- c("red", "pink", "purple", "deep-purple",
             "indigo", "blue", "light-blue", "cyan", "teal", "green", "light-green",
             "lime", "yellow", "amber", "orange", "deep-orange", "brown", "grey",
             "blue-grey")


ui <- fluidPage(theme=shinytheme(theme = "cyborg"),div(style="text-align:center", h1("Euroleague Shot Charts")),
                useShinyjs(),
                sidebarLayout(sidebarPanel(radioButtons(inputId= "hex", choices = c("hexmap","heatmap"),label="Plot Type"),
                div(id="sidebar2",radioButtons(inputId = "player",choices = c("team","player"),label = "Teams/Players"),
                uiOutput(outputId = "out"),
                shinyjs::hidden(selectizeInput(inputId = "selectionSeason",choices = listSeasons,label = "Season")),
                shinyjs::hidden(selectizeInput(inputId = "selection",choices = c(""),label = "Players")),
                shinyjs::hidden(selectInput(inputId = "selectTeams",choices = c(""),label = "Teams")),
                shinyjs::hidden(selectizeInput(inputId = "selectOpp",choices = list(self=FALSE,opponent=TRUE),label = "Self/Opponent FGs")),
                shinyjs::hidden(selectizeInput(inputId = "colors",choices = list("red","green","blue","purple"),selected="purple",label = "Heatmap Colors")),
                shinyjs::hidden(selectInput(inputId = "colorsHex",choices = sciCols,selected="pink",label = "Hexmap Colors")),
                shinyjs::hidden(sliderInput(inputId="hexSize",min = 0.2,value = 1.25,max = 2,label = "Max Hex Size(meters)")),
                shinyjs::hidden(sliderInput(inputId="hexMult",min = 0.1,value = 0.3,max = 1,label = "Hex Size Multiplier")),
                div(id="sidebar3",sidebarPanel()),
                actionButton(inputId = "button1",label = "Plot!"))),
                
                div(mainPanel(div(plotOutput(outputId = "heatmap"))))))


server <- function(input, output){
  
  
  observeEvent(eventExpr = input$selectionSeason, handlerExpr = {
    updateSelectInput(inputId = "selectTeams",choices = unique(data[data$season == input$selectionSeason,]$team))
    updateSelectizeInput(inputId = "selection",choices = unique(data[data$season == input$selectionSeason,]$name))
    
  
    
  })
  
  
  
  observeEvent(eventExpr = input$hex,handlerExpr= {
    if(input$hex == "hexmap"){
      #shinyjs::hide(id="sidebar2")
      
      shinyjs::show(id="colorsHex")
      shinyjs::hide(id="colors")
      shinyjs::show(id="hexSize")
      shinyjs::show(id="hexMult")
    }else{
      #shinyjs::show(id="sidebar2")
      shinyjs::show(id="colors")
      shinyjs::hide(id="colorsHex")
      shinyjs::hide(id="hexSize")
      shinyjs::hide(id="hexMult")
    }
    
  })
  
  
  
  
  observeEvent(eventExpr = input$player,handlerExpr = {
    if(input$player == "team"){
      
      shinyjs::show(id = "selectTeams")
      shinyjs::hide(id = "selection")
      shinyjs::show(id = "selectOpp")
    }else{
      
      shinyjs::show(id="selection")
      shinyjs::hide(id="selectTeams")
      shinyjs::hide(id = "selectOpp")
      
     
    }
    shinyjs::show(id="selectionSeason")
  })
  
  
  output$heatmap <- renderPlot(width = 600,height = 600,{
    
    
    
    if(input$button1 == 0){
      return()
    }
    
    
    
    redT <- c("black","brown","red","orange","yellow","white")
    blueT <- c("black","navy","blue","lightblue","white")
    greenT <- c("black","darkgreen","green","lightgreen","yellow")
    purpleT <- (c("black","#522149","purple","pink","white"))
    listColors <- list(redT,blueT,greenT, purpleT)
    names(listColors) <- c("red","blue","green","purple")
    sciCols <- c("red", "pink", "purple", "deep-purple",
                 "indigo", "blue", "light-blue", "cyan", "teal", "green", "light-green",
                 "lime", "yellow", "amber", "orange", "deep-orange", "brown", "grey",
                 "blue-grey")
    
    plCheck <- logical()
    inpName <- character()
    oppCheck <- logical()
    
    if(isolate(input$player) =="player"){
      plCheck <- TRUE
      inpName <-isolate(input$selection)
      
    }else if(isolate(input$selectOpp)){
      plCheck <- FALSE
      oppCheck <- TRUE
      inpName <- isolate(input$selectTeams)
    }else{
      plCheck <- FALSE
      oppCheck <- FALSE
      inpName <- isolate(input$selectTeams)
    }
    
  season <- isolate(input$selectionSeason)
  scicol <- isolate(input$colorsHex)
  hexSize <- isolate(input$hexSize)*100
  hexMult <- isolate(input$hexMult)
  

  input$button1
  if(isolate(input$hex) == "heatmap"){
    isolate(heatFunction(data,player = plCheck,opp = oppCheck,season = season,input = inpName,input_title = inpName,col_palette = listColors[[input$colors]]))  
  }else{
    isolate(hexFunction(data,binwidth = hexSize ,radiusFactor = hexMult,season = season,player = plCheck,opp = oppCheck,input = inpName,col_palette = scicol)) 
  }
   
    
   })
  
  
} 

shinyApp(ui,server)
