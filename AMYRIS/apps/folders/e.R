library(shiny)
library(dplyr)

# Define data
df <- data.frame(
  robot = c("Bender", "C3P0", "Terminator", "Bender", "Terminator", "C3P0"),
  score = c(75, 82, 95, 67, 89, 74)
)

# Define UI
ui <- fluidPage(
  titlePanel("Button Example"),
  sidebarLayout(
    sidebarPanel(
      # Define buttons
      actionButton("btnA", "Filter Bender"),
      actionButton("btnB", "Filter C3P0"),
      actionButton("btnC", "Filter Terminator"),
      br(),
      actionButton("btnReset", "Reset Filter")
    ),
    mainPanel(
      # Output table
      tableOutput("table")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Define filtered data
  filtered_data <- reactive({
    if (input$btnA > 0) {
      df %>% filter(robot == "Bender")
    } else if (input$btnB > 0) {
      df %>% filter(robot == "C3P0")
    } else if (input$btnC > 0) {
      df %>% filter(robot == "Terminator")
    } else {
      df
    }
  })
  
  # Reset filter when reset button is clicked
  observeEvent(input$btnReset, {
    updateActionButton(session, "btnA", label = "Filter Bender")
    updateActionButton(session, "btnB", label = "Filter C3P0")
    updateActionButton(session, "btnC", label = "Filter Terminator")
  })
  
  # Render table with filtered data
  output$table <- renderTable({
    filtered_data()
  })
  
  # Update button labels based on current filter
  observe({
    if (identical(filtered_data(), df %>% filter(robot == "Bender"))) {
      updateActionButton(session, "btnA", label = "Bender (Selected)")
      updateActionButton(session, "btnB", label = "Filter C3P0")
      updateActionButton(session, "btnC", label = "Filter Terminator")
    } else if (identical(filtered_data(), df %>% filter(robot == "C3P0"))) {
      updateActionButton(session, "btnA", label = "Filter Bender")
      updateActionButton(session, "btnB", label = "C3P0 (Selected)")
      updateActionButton(session, "btnC", label = "Filter Terminator")
    } else if (identical(filtered_data(), df %>% filter(robot == "Terminator"))) {
      updateActionButton(session, "btnA", label = "Filter Bender")
      updateActionButton(session, "btnB", label = "Filter C3P0")
      updateActionButton(session, "btnC", label = "Terminator (Selected)")
    } else {
      updateActionButton(session, "btnA", label = "Filter Bender")
      updateActionButton(session, "btnB", label = "Filter C3P0")
      updateActionButton(session, "btnC", label = "Filter Terminator")
    }
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)


library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      actionButton("btnA", "Filter A"),
      actionButton("btnB", "Filter B"),
      actionButton("btnC", "Filter C"),
      actionButton("reset", "Reset")
    ),
    mainPanel(
      tableOutput("table")
    )
  )
)

server <- function(input, output, session) {
  # Create a reactive object to store the filtered data
  observeEvent(input$reset, {
    shinyBS::updateButton(session, "btnA", label = "Filter A",value=0) 
    shinyBS::updateButton(session, "btnB", label = "Filter B", value=0)
    shinyBS::updateButton(session, "btnC", label = "Filter C", value=0)
  })
  
  
  observeEvent(input$reset_input, {
    updateactionButton(session, "mynumber", value = 20)
    updateTextInput(session, "mytext", value = "test")
  })
  
  filtered_data <- reactive({
    if (input$btnA > 0) {
      mtcars[mtcars$cyl == 4, ]
      input$btnA=0
    } else if (input$btnB > 0) {
      mtcars[mtcars$cyl == 6, ]
    } else if (input$btnC > 0) {
      mtcars[mtcars$cyl == 8, ]
    } else {
      mtcars
    }
  })
  
  # Update the table when the filtered data changes
  output$table <- renderTable({
    filtered_data()
  })
  
  # Reset the input and filtered data when the reset button is clicked

}

shinyApp(ui, server)




library(shiny)

# Define UI
ui <- fluidPage(
  actionButton("btnA", "Option A"),
  actionButton("btnB", "Option B"),
  actionButton("btnReset", "Reset"),
  textOutput("output")
)

# Define server
server <- function(input, output, session) {
  
  # Define initial data
  to_plot <- mtcars
  
  # Define filtered data based on button clicks
  filtered_data <- reactive({
    if (input$btnA > 0) {
      to_plot[to_plot$cyl == 4,]
    } else if (input$btnB > 0) {
      to_plot[to_plot$cyl == 6,]
    } else {
      to_plot
    }
  })
  
  # Update filtered data when button is clicked
  observe({
    output$output <- renderText({
      filtered_data()
    })
  })
  
  # Reset all inputs and data
  observeEvent(input$btnReset, {
    updateActionButton(session, "btnA", label = "Option A")
    updateActionButton(session, "btnB", label = "Option B")
    to_plot <- mtcars
  })
}

# Run the app
shinyApp(ui, server)

observeEvent(input$reset,{
  output$tab_1_ui <- renderUI({
    tab1_ui
  })
})


  
library(shiny)


  ui = pageWithSidebar(
    
    headerPanel("'Reset inputs' button example"),
    
    sidebarPanel(
      shinyjs::useShinyjs(),
      id = "side-panel",
      actionButton("btnA", "Robot Bender"),
      actionButton("btnC", "Robot Terminator"),
      actionButton("btnB", "Robot C3PO"),
      tags$hr(),
      actionButton("reset_input", "Reset inputs")
    ),
    
    mainPanel(
      h4("Summary"),
      verbatimTextOutput("summary")
    )
    
  )
  
  server = function(input, output, session) {
    
    output$summary <- renderText({
      return(paste(input$mytext, input$mynumber))
    })
    
    observeEvent(input$reset_input, {
      shinyjs::reset("side-panel")
    })
  }
  shinyApp(ui, server)
  
  
  
  set.seed(123)
  df <- data.frame(
    group = sample(LETTERS[1:3], 100, replace = TRUE),
    value = rnorm(100),
    color = factor(sample(1:3, 100, replace = TRUE))
  )  
  to_plot
  ui <- fluidPage(
    
    # Sidebar panel with checkboxes to filter the boxplot
    sidebarPanel(
      checkboxGroupInput("group", "Select groups:", choices = unique(to_plot$robot), selected = unique(to_plot$robot))
    ),
    
    # Plot output to display the filtered boxplot
    plotOutput("boxplot")
    
  )
  
  # Define the server for the Shiny app
  server <- function(input, output) {
    
    # Render the boxplot based on the selected groups
    output$boxplot <- renderPlot({
      filtered_df <- to_plot[to_plot$robot %in% input$group,]
      ggplot(filtered_df, aes(x = plate, y = value,fill = color)) +
        geom_boxplot() +
        labs(title = "Filtered Boxplot")
    })
    
  }
  
  # Run the Shiny app
  shinyApp(ui, server)
  
  
  
  server <- function(input, output) {
    
    
    
    filteredData3 <- reactive({
      
      if (input$btnA > 0) {
        
        return (to_plot[to_plot$robot == "Bender"&to_plot$well_type=='Standard Well',])
        
        
      }
      if (input$btnB > 0) {
        
        return(to_plot[to_plot$robot == "C3P0"&to_plot$well_type=='Standard Well',])
      }
      if (input$btnC > 0) {
        
        return(to_plot[to_plot$robot == "Terminator"&to_plot$well_type=='Standard Well',])
      }
      
      else
        return(to_plot)
    })
    
    
    # Create a plot that displays the filtered data
    output$myplot <- renderPlot({
      ggplot(filteredData3(), aes(x = plate, y = value, color = well_type)) + 
        geom_boxplot()+
        labs(title = "My Plot_WellType")
    })
    
    output$myplot_ALLL <- renderPlot({
      ggplot(filteredData3(), aes(x = plate, y = value, color = well_type)) + 
        geom_boxplot() + scale_fill_manual(values = c("red", "blue", "green")) 
    })
    
    output$myplot_ALthebest <- renderPlot({
      ggplot(filteredData3(), aes(x = plate, y = value)) + 
        geom_boxplot(aes(colour = color))
    })
    
    
  }
  
  shinyApp(ui, server)