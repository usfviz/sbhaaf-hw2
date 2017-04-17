library(shiny)
library(ggvis)
library(reshape2)
library(dplyr)

setwd('~/MSAN/Data Visualization/hw2/')
fertility <- read.csv('API_SP.DYN.TFRT.IN_DS2_en_csv_v2/API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv', header = T)
life_exp <- read.csv('API_SP.DYN.LE00.IN_DS2_en_csv_v2/API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv', header = T)[,c(1,5:59)]
meta <- read.csv('API_SP.DYN.LE00.IN_DS2_en_csv_v2/Metadata_Country_API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv')
regions <- merge(data.frame(Country=fertility$Country.Name, Country.Code=fertility$Country.Code),data.frame(Country.Code = meta$Country.Code, Region=meta$Region))[,2:3]
fertility <- fertility[,c(1,5:59)]

global <- data.frame(Country=fertility$Country.Name, year=rep(1960:2014, each=nrow(fertility)),
                   fertility=melt(fertility[2:ncol(fertility)])[,2], life_exp=melt(life_exp[2:ncol(life_exp)])[,2])

global <- droplevels(merge(global, regions))
global <- droplevels(global[global$Region != '',])

extract_inputs <- function(x, env = parent.frame()) {
  # Base case
  if (is.name(x) || is.atomic(x)) {
    return(list(expr = x, inputs = NULL))
  }
  
  # Recursive case: should be a call
  stopifnot(is.call(x))
  
  # If it's a call to I, it's an input and should be evaluated
  if (identical(x[[1]], quote(I))) {
    stopifnot(length(x) == 2)
    input <- eval(x[[2]], env)
    nm <- input$id
    
    return(list(
      expr = substitute(input$nm, list(nm = as.name(nm))),
      inputs = setNames(list(input), nm) 
    ))
  }
  
  # Otherwise, recurse through each argument to the calls
  args <- as.list(x[-1])
  args_out <- lapply(args, extract_inputs, env = env)
  
  expr <- lapply(args_out, "[[", "expr")
  inputs <- unlist(lapply(args_out, "[[", "inputs"), recursive = FALSE)
  inputs <- inputs[!duplicated(names(inputs))]
  
  list(
    expr = as.call(c(x[[1]], expr)),
    inputs = inputs
  )  
  
}

ui <- shinyUI(fluidPage(
   # Application title
   titlePanel("Fertility Rate vs. Life Expectency"),
   
   # Sidebar with a slider input for number of bins 
   # sidebarLayout(
   #   sidebarPanel(
   #     sliderInput('year','Year',1960,2014,step=1)
   #   ),
     mainPanel(
          ggvisOutput("ggvis"),
          uiOutput("ggvis_ui")
     )
   # )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  # data <- reactive({global[global$year==1960,]})
  # print(data)
  
  global %>% 
      ggvis(~life_exp, ~fertility, key := ~Country) %>%
      filter(year == eval(input_slider(1960, 2014, value=1))) %>%
      # layer_smooths(span = input_slider(0.5, 1, value = 1)) %>%
      layer_points(fill = ~Region) %>%
      scale_numeric("x", domain = c(20, 85)) %>%
      scale_numeric("y", domain = c(0, 8)) %>%
      add_tooltip(function(data){data$Country}, "hover") %>%
      bind_shiny("ggvis", "ggvis_ui")
})

# Run the application 
shinyApp(ui = ui, server = server)

