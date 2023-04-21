##libraries
library(rvest)   
library(data.table)
library(tidyjson)
library(httr)
library(stringi)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)    
library(readxl)
library(stringdist)
library(Rfast) #skew and kurt
library(rjson) # write in json
library(jsonlite) #convert to json


files <- list.files(path = 'data', pattern = '.xlsx', full.names = T)

path <- files[1] #change the files here


Validate_files <- function(path) {
  validation <- read_excel(path)
 
  ##we start by validating the headers names
  list_of_heads <- c('counter','plate','strain','well_type','robot','stacker_position',
                     'value')
  val_names <- names(validation)
  
  if(length(list_of_heads[!(list_of_heads %in% val_names)]) ==0) {
    validation <- validation %>% 
      dplyr::select(all_of(list_of_heads))
    print(paste0('Headers names validation: Completed'))
    
    #delete all possible nas found
    validation <- na.omit(validation)
    
    #validating the plats. we only want plates that contain 96 wells
    validation_plats <- validation %>% 
      dplyr::group_by(plate) %>% 
      dplyr::summarise(
        n = n(),.groups='drop') %>% 
      dplyr::filter(n!=96)
    
    
    #validation <<- validation
    if (nrow(validation_plats)>0) {
      
      validation <- validation %>% 
        dplyr::filter(plate!=validation_plats$plate)
                      
      print(paste0('Plate ',validation_plats$plate,' does not have 96 wells, therefore, it was removed. Plate Validation : Completed' ))} else{ 
        print('Plates validation: Completed')}
    
    } else{
     return ( print(paste0('Data is corrupted, headers names Validation: Failed')))
    }
  
 # validation <- validation %>% 
  #  dplyr::filter(t.test(value)$p.value < 0.05 | value==0)
 return (validation)
    
}


statistical_summary <- function(DF){
  
  statistics_df <- DF
  
  statistics_df <- statistics_df %>% 
    dplyr::group_by(plate) %>% 
    dplyr::summarise(
      Minimum           = min(value),
      Maximum           = max(value),
      Mean              = mean(value),
      StandardDeviation = sd(value),
      Kurtosis          = kurt(value),
      Skewness          = skew(value),
    .groups='drop')
  return(statistics_df)
}

Validated_df <- Validate_files(path)  
if(is.data.frame(Validated_df)==FALSE){
  print('Validations: Failed. Data is corrupted. Another file is required')
}else{
  print('file has been validated and is able to proceed')
  summary <- statistical_summary(Validated_df)
  #write.csv(summary, 'data/summary.csv')
  
  Jsummary <- toJSON(summary)
  #write(Jsummary, "data/JSON_summary.json")
}



best_df <- function(DF,threshold =0, max_number_per_plate=nrow(DF), Robots=c('Bender','Terminator','C3P0')) {
  
  bestDF <- DF %>%
    dplyr::filter(robot==Robots) %>% #######working
    dplyr::group_by(plate) %>% 
    dplyr::filter(value > max(value[well_type=='Parent Strain'])+ threshold) %>% 
    dplyr::slice(1:max_number_per_plate)
  }

Final_df <-as.data.frame(best_df(Validated_df))

to_plot_DF <- rbind(Final_df,Validated_df %>% dplyr::filter((well_type =='Parent Strain')), by='Plate') %>% 
  dplyr::mutate(
    value = as.numeric(value),
    plate =as.numeric(plate)
  )

to_plot_df <- na.omit(to_plot_DF)


####plot i wanna use.

to_plot_df$plate <- factor(to_plot_df$plate, levels = rep(as.character(seq(1, 60))))
Validated_df$plate <- factor(Validated_df$plate, levels = rep(as.character(seq(1, 60))))
Validated_df <- na.omit(Validated_df)
Validated_df <- as.data.frame(Validated_df)

myplot <- ggplot(to_plot_df, aes(x =plate, y = value, color = well_type)) + 
  geom_boxplot() ########working

myplot_OG <- ggplot(Validated_df, aes(x =plate, y = value, color = well_type)) + 
  geom_boxplot() 


##########perfect
########working
#buttom com filtro
#evocar funcao



# Now, let's create a UI that includes three buttons for each group
library(shiny)

ui <- fluidPage(
  titlePanel("My Plot"),
  sidebarLayout(
    sidebarPanel(
      #   selectInput("plate", "Select a plate to delet outplier:", choices = unique(to_plot$plate)),
      #   actionButton("removeoutliers", "Remove Outliers"),
      checkboxGroupInput("group", "Select groups:", choices = unique(to_plot_df$robot), selected = unique(to_plot_df$robot))
    ),
    mainPanel(
     
      plotOutput("myplot_OG"),
      plotOutput("myplot"),
      plotOutput("myplot_ALthebest")
    )
  )
)

# Now, let's create a server function that filters the data based on the button clicks

server <- function(input, output) {
  

  
  output$myplot_OG <- renderPlot({
    filtered_df_all <- Validated_df[Validated_df$robot %in% input$group,]
    ggplot( filtered_df_all, aes(x = plate, y = value, color = well_type)) + 
      geom_boxplot() +
      labs(title = "All the data from CSV")
  })
  
  
  output$myplot <- renderPlot({
    filtered_df <- to_plot_df[to_plot_df$robot %in% input$group,]
    ggplot(filtered_df, aes(x = plate, y = value, color = well_type)) + 
      geom_boxplot()+
      labs(title = "Data only from the Standard Well's that surpass their father's values")
  })
  
  output$myplot_ALthebest <- renderPlot({
    filtered_df_best <- to_plot_df[to_plot_df$robot %in% input$group,] %>% dplyr::filter(well_type=='Standard Well')
    ggplot(filtered_df_best, aes(x = plate, y = value, color=robot)) + 
      geom_boxplot()+
      labs(title = "Best Standard Well's compared to their parent's")
  })
  
  
}

shinyApp(ui, server)



removeOutliersIQR <- function(dfData, limitRemove = 3, strongOutliers = T){
  dfData <- new_df  
  print("IQR!")
  flagStop <- F
  
  if(strongOutliers){
    outliersDim <- 3
  } else {
    outliersDim <- 1.5
  }
  
  nExecs <- 0
  while((!flagStop) & nExecs < limitRemove){
    
    # Gets the first and last quantile
    normVals <- quantile(dfData$value,c(0.25,0.75))
    q1 <- normVals[[1]]
    q3 <- normVals[[2]]
    
    # Gets the interquartile range
    iqr <- q3-q1
    
    maxVals <- q3 + (iqr * outliersDim)
    minVals <- q1 - (iqr * outliersDim)
    # minVals <- case_when(
    #   (nExecs == 0) ~ q1 - (iqr * outliersDim),
    #   T ~ 0
    # )
    
    if(any(dfData$value >= maxVals) | any(dfData$value <= minVals)){
      
      dfData <- dfData %>% 
        dplyr::filter(
          ITENS < maxVals,
          ITENS > minVals
        )
      
    } else {
      flagStop <- T
    }
    
    nExecs <- nExecs + 1
    
  }
  
  return(dfData)
  
}





