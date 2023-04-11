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

new_df <- Validate_files(path)  
if(is.data.frame(new_df)==FALSE){
  print('Validations: Failed. Data is corrupted. Another file is required')
}else{
  print('file has been validated and is able to proceed')
  summary <- statistical_summary(new_df)
  write.csv(summary, 'data/summary.csv')
  
  Jsummary <- toJSON(summary)
  write(Jsummary, "data/JSON_summary.json")
}



best_df <- function(DF,threshold =0, max_number_per_plate=nrow(DF), Robots=c('Bender','Terminator','C3P0')) {
  
  ist_maybe12122 <- DF %>%
    dplyr::filter(robot==Robots) %>% #######working
    dplyr::group_by(plate) %>% 
    dplyr::filter(value > max(value[well_type=='Parent Strain'])+ threshold) %>% 
    dplyr::slice(1:max_number_per_plate)
}

dd <- best_df(new_df,Robots = 'C3P0')


    
    
    


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

#new_df3 %>% dplyr::filter(t.test(new_variable)$p.value< 1.5) %>% View()