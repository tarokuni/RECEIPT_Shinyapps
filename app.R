# command to add Bioconductor repository; needed when deploying to shinyapps.io
## options(repos = BiocManager::repositories())


# packages
library(shiny, warn.conflicts = FALSE) # app
library(tidyverse, warn.conflicts = FALSE) # ggplot
library(bnlearn, warn.conflicts = FALSE) # bn
library(gRain, warn.conflicts = FALSE) # for exact inference 
library(Rgraphviz, warn.conflicts = FALSE) # additional options for figures
library(readxl) # excel

# data import
data <- read_xlsx("Micro-storylines.xlsx", sheet = 2, skip = 1)
colnames <- data[,2]
data <- t(data)[3:nrow(t(data)), ]
dimnames(data)[2] <- colnames
data <- transform(data, 
          Score = as.numeric(Score),
          Category = as.numeric(Category),
          `X..Change` = as.numeric(`X..Change`),
          `Absolute.change` = as.numeric(`Absolute.change`))
dimnames(data)[2] <- colnames # names change for some reason

# transform data to long
data <- 
  data %>% 
  rownames_to_column() %>% 
  mutate(Intensity = factor(str_detect(rowname, "Intensity", negate = TRUE)),
         HA = factor(str_detect(rowname, "HA", negate = TRUE)),
         `COVID-19` = factor(str_detect(rowname, "COVID-19"))) %>% 
  select("Score", "Intensity", "HA", "COVID-19")
 



# Create DAG for calculation
dag <- model2network(paste0("[Intensity][HA][COVID-19]",
                            "[Score|Intensity:HA:COVID-19]"))


# And DAG for visualization
dag_display <- model2network(paste0("[Cyclone\nIdai]",
                            "[Climate\nchange]",
                            "[Humanitarian\naccess\nlevel]",
                            "[COVID-19-like\noutbreak]",
                            "[Cyclone\nintensity|Cyclone\nIdai:Climate\nchange]",
                            "[People in\nneed of\nhumanitarian\naccess|",
                            "Cyclone\nintensity:Humanitarian\naccess\nlevel:",
                            "COVID-19-like\noutbreak]",
                            "[Crisis\nSeverity|People in\nneed of\nhumanitarian\naccess]"))

# If checking
# graphviz.plot(dag)

##


# learn parameters
bn_model <- bn.fit(dag, data = data)


# plot function for DAG
plotbn <- function(dag, intensity, HA, COVID){
  
  priors <- c(intensity, HA, COVID)
  
  # if-else is for technical issues in highlighting
  if (intensity & HA & COVID){ # all conditions are chosen
    
    g <- graphviz.plot(dag,
                      layout = "dot",
                      render = FALSE)
    
  } else {
    hlight <- list(nodes = c("Climate\nchange", "Humanitarian\naccess\nlevel" ,
                             "COVID-19-like\noutbreak")[!priors],
                   arcs = arcs(dag)[c(2,4,5),][!priors, ],
                   col = "grey",
                   textCol = "grey")
    
    g <- graphviz.plot(dag,
                  highlight = hlight,
                  layout = "dot",
                  render = FALSE)
    
  }

  nodeRenderInfo(g) <- list(shape = list("Climate\nchange" = "rectangle",
                                         "Humanitarian\naccess\nlevel" = "rectangle",
                                         "COVID-19-like\noutbreak" = "rectangle"))
  
  renderGraph(g) 
}


#################
#################



# App part

ui <- fluidPage(
  
  titlePanel("Extreme coastal flooding in Mozambique from Cyclone Idai"),

  sidebarLayout(
    sidebarPanel(width = 3,
       radioButtons("climate_change",
                    "World with/without climate change:",
                    choiceNames = c("With", "Without"), 
                    choiceValues = c(TRUE, FALSE),
                    selected = TRUE),
       
       radioButtons("covid","COVID-19-like outbreak:",
                    choiceNames = c("Yes", "No"),
                    choiceValues = c(TRUE, FALSE),
                    selected = FALSE),
       conditionalPanel(condition = "input.covid == `TRUE`",
                        sliderInput("covid_p", "Probability of COVID-19-like outbreak", 
                                    min = 0, max = 1, value = 1)),
       
       radioButtons("HA","Humanitarian access present:",
                    choiceNames = c("Yes", "No"),
                    choiceValues = c(TRUE, FALSE),
                    selected = TRUE),
       conditionalPanel(condition = "input.HA == `TRUE`",
                        sliderInput("HA_p", "Probability of Humanitarian access", 
                                    min = 0, max = 1, value = 1))
       
    ),
    mainPanel(width = 8,
        plotOutput("bn")

      )
    
  ),

  h2("The (expected) INFORM Severity Index is", strong(textOutput("index", inline = TRUE)))
  
  
  
)

#################
#################


server <- function(input, output, session) {

  
  # drawing the DAG
  output$bn <- renderPlot({
    plotbn(dag_display, 
           as.logical(input$climate_change),
           as.logical(input$HA), 
           as.logical(input$covid))
  }, res = 196)
  
  
  output$index <- renderText({
  
  # Calculate probabilities  
  # set prior
    
    evidence <- list(Intensity = input$climate_change)
    
    if (as.logical(input$covid) == TRUE){
      bn_model$`COVID-19` <- array(c(input$covid_p, 1 - input$covid_p),
                                   dimnames = list(c(TRUE, FALSE)))
    } else {
      evidence[["COVID-19"]] = input$covid
    }
    
    if (as.logical(input$HA) == TRUE){
      bn_model$HA <- array(c(input$HA_p, 1 - input$HA_p),
                                   dimnames = list(c(TRUE, FALSE)))
    } else {
      evidence[["HA"]] = input$HA
    }
    

    # Monte Carlo
    val <- cpdist(bn_model, nodes = "Score",
                    evidence = evidence,
                    method = "lw")[["Score"]]
    
  
    
    format(round(mean(val), digits = 1), nsmall = 1)
    })
  
}


shinyApp(ui, server)
