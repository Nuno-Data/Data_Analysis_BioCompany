###extra
#####new input

library(shiny)

ui <- fluidPage(
  titlePanel("My Plot"),
  sidebarLayout(
    
    sidebarPanel(
      checkboxGroupInput("group", "Select Robots:", choices = unique(to_plot$robot), selected = unique(to_plot$robot))
    ),
    mainPanel(
      plotOutput("myplot")
      
    )
  )
)

# Now, let's create a server function that filters the data based on the button clicks

server <- function(input, output) {
  
  
  
  
  
  # Create a plot that displays the filtered data
  output$myplot <- renderPlot({
    filtered_df <- to_plot %>% dplyr::filter(robot %in% input$robot)
    ggplot(to_plot, aes(x = plate, y = value, color = well_type)) + 
      geom_boxplot()+
      labs(title = "My Plot_WellType")
  })
  
  
  
  
  
}

shinyApp(ui, server)


####


#ggplot(to_plot, aes(x = as.character(plate), y = value, color = well_type)) + 
#  geom_boxplot()
#ggplot(filteredData(), aes(x = plate, y = value, color = robot)) +
#  geom_point() +
#  labs(title = "My Plot")
# Finally, let's run the app

#ends









filteredData2 <- reactive({
  to_plot_var <- new_df 
  if (input$btnA > 0) {
    to_plot_var <- new_df[new_df$robot == "Bender",]
  }
  if (input$btnB > 0) {
    to_plot_var <- new_df[new_df$robot == "C3P0",]
  }
  
  if (input$btnC > 0) {
    to_plot_var <- new_df[new_df$robot == "Terminator",]
  }
  
  if (input$btnD > 0) {
    to_plot_var <- new_df
  }
  to_plot_var <-  new_df 
  return(to_plot_var)
  
})






####un used plots

myplot <- ggplot(dd, aes(x=plate, y=value, color = robot)) +
  geom_point() +
  labs(title = "My Plot")

myplot <- ggplot(to_plot, aes(x=plate, y=value, color = robot)) +
  geom_point() +
  labs(title = "My Plot")


myplot1 <- ggplot(to_plot, aes(x=plate, y=value, color = well_type)) +
  geom_point() +
  labs(title = "My Plot")



myplot2 <- boxplot(df$value~dd$plate) ##values from the best data

myplot2 <- boxplot(to_plot[to_plot$well_type=='Standard Well',]$value~to_plot[to_plot$well_type=='Standard Well',]$plate,
                   main="Different boxplots for each month",
                   xlab="Month Number",
                   ylab="Degree Fahrenheit",
                   col="orange",
                   border="brown"
)



# To add the label of x axis
my_names <- sapply(strsplit(myplot$names , '\\.') , function(x) x[[2]] )
my_names <- my_names[seq(1 , length(my_names) , 2)]
axis(1, 
     at = seq(1.5 , 14 , 2), 
     labels = my_names , 
     tick=FALSE , cex=0.3)


myplot_lostcoumt <- boxplot(to_plot[to_plot$well_type=='Standard Well',]$value~to_plot[to_plot$well_type=='Standard Well',]$plate , 
                            bylab="sickness",
                            main="sickness of several wheat lines" , 
                            col=c("slateblue1" , "tomato"),ylim = c(0, (max(to_plot$value)+(max(to_plot$value)*.1))))

myplot_lostcoumt <- boxplot(to_plot[to_plot$well_type=='Standard Well',]$value~to_plot[to_plot$well_type=='Standard Well',]$plate , 
                            bylab="sickness",
                            main="sickness of several wheat lines" , 
                            col=c("slateblue1" , "tomato"))



yplot_lostcoumt2 <- ggplot(to_plot1 %>% dplyr::filter(well_type=='Standard Well'), aes(x =(plate), y = value, color = well_type)) + 
  geom_boxplot()



data <- data.frame(group = rep(c("A", "B", "C"), each = 10),
                   value = rnorm(30),
                   color = rep(c("red", "blue", "green"), each = 10))

# create a boxplot with colored background
data <- data.frame(group = rep(c("A", "B", "C"), each = 10),
                   value = rnorm(30),
                   color = rep(c("red", "blue", "green"), each = 10))

# create a boxplot with colored background
ggplot(data, aes(x = group, y = value)) +
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "grey90")) +
  facet_wrap(~color)

myplot3 <- boxplot(new_df$value~new_df$plate, color=new_df$robot) ##values from all the data

myplot <- ggplot(to_plot, aes(x =(as.character(to_plot$plate)), y = value, color = well_type)) + 
  geom_boxplot() ########working

to_plot_SW <- to_plot %>% 
  dplyr::filter(well_type=='Standard Well')

myplotrobot <- ggplot(to_plot_SW, aes(x = as.character(plate), y = value, color = robot)) + 
  geom_boxplot()
