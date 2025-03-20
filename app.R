library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Customer Segmentation with RFM Model"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Data Overview", tabName = "data_overview", icon = icon("table")),
      menuItem("Data Visualization", tabName = "data_viz", icon = icon("chart-bar")),
      menuItem("RFM Model", tabName = "rfm_model", icon = icon("chart-pie"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Home Tab
      tabItem(tabName = "home",
              h2("Welcome to Retail Analytics Dashboard"),
              p("Use the sidebar to navigate.")
      ),
      
      # Data Overview Tab
      tabItem(tabName = "data_overview",
              h2("Data Overview"),
              fileInput("file", "Upload your CSV", accept = ".csv"),
              h3("Data Before Cleansing and Transformation"),
              DTOutput("original_data_table"),
              h3("Data After Cleansing and Transformation"),
              DTOutput("cleaned_data_table")
      ),
      
      # Data Visualization Tab
      tabItem(tabName = "data_viz",
              h2("Data Visualization"),
              fluidRow(
                valueBoxOutput("total_users_box"),
                valueBoxOutput("total_purchases_box"),
                valueBoxOutput("cart_ratio_box")
              ),
              fluidRow(
                column(width = 6, box(plotOutput("period_barplot"), title = "Event Period Distribution", width = 12)),
                column(width = 6, box(plotOutput("event_type_plot"), title = "Event Count by Event Type", width = 12))
              ),
              h3("Average Price by Hour and Day"),
              fluidRow(
                box(plotOutput("avg_price_hour_plot"), width = 6),
                box(plotOutput("avg_price_plot"), width = 6)
              ),
              h3("Product Category Distribution"),
              fluidRow(
                box(plotOutput("category_plot"), width = 12)
              )
              
      ),
      
      # RFM Model Tab
      tabItem(tabName = "rfm_model",
              h3("RFM Segmentation Table"),
              DTOutput("rfm_table"),
              h3("RFM Interpretation"),
              DTOutput("rfm_table_result"),
              uiOutput("rfm_interpretation")
      )
    )
  )
)

# Define Server
server <- function(input, output) {
  # Reactive expression to read original CSV data
  df_original <- reactive({
    req(input$file)  # Ensure file is uploaded
    read.csv(input$file$datapath, sep = ",", stringsAsFactors = FALSE)
  })
  
  df_clean <- reactive({
    req(df_original())  # Ensure original data exists
    
    df_cleaned <- df_original() %>%
      mutate_if(is.character, trimws) %>%  # Remove extra spaces
      drop_na() %>%  # Remove missing values
      rename_all(tolower)  # Convert column names to lowercase
    
    # Remove first column if there are multiple columns
    if (ncol(df_cleaned) > 1) {
      df_cleaned <- df_cleaned[, -1]
    }
    
    # Convert event_time column into date and time
    if ("event_time" %in% names(df_cleaned)) {
      df_cleaned <- df_cleaned %>%
        mutate(
          event_time = as.character(event_time),
          event_time = ymd_hms(event_time),
          event_date = as.Date(event_time),
          event_hour = format(event_time, "%H:%M:%S"),
          event_hour_numeric = hour(event_time),
          day = day(event_time),
          week = week(event_time),
          month_name = month(event_time, label = TRUE, abbr = FALSE),
          period = case_when(
            event_hour_numeric >= 5 & event_hour_numeric < 9  ~ "Early Morning",
            event_hour_numeric >= 9 & event_hour_numeric < 12 ~ "Morning",
            event_hour_numeric >= 12 & event_hour_numeric < 17 ~ "Afternoon",
            event_hour_numeric >= 17 & event_hour_numeric < 21 ~ "Evening",
            TRUE ~ "Night"
          )
        ) %>%
        select(-event_hour_numeric)
    }
    
    # Process category_code to extract category and clean subcategory
    if ("category_code" %in% names(df_cleaned)) {
      df_cleaned <- df_cleaned %>%
        separate(category_code, into = c("category", "subcategory"), sep = "\\.", extra = "merge", fill = "right") %>%
        mutate(subcategory = sub("\\..*", "", subcategory))  # Remove everything after the first dot
    }
    
    return(df_cleaned)
  })
  
  # KPI for Total Users
  total_users <- reactive({
    req(df_clean())
    length(unique(df_clean()$user_id))
  })
  
  output$total_users_box <- renderValueBox({
    valueBox(
      value = total_users(),
      subtitle = "Total Users",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  # KPI for Total Purchases
  total_purchases <- reactive({
    req(df_clean())
    sum(df_clean()$event_type == "purchase", na.rm = TRUE)
  })
  
  output$total_purchases_box <- renderValueBox({
    valueBox(
      value = total_purchases(),
      subtitle = "Total Purchases",
      icon = icon("shopping-cart"),
      color = "green"
    )
    
    
    
  })
  
  # KPI for Total Cart Events
  total_cart_events <- reactive({
    req(df_clean())
    sum(df_clean()$event_type == "cart", na.rm = TRUE)  # Count total cart events
  })
  
  # KPI for Cart-to-Purchase Ratio (as percentage)
  cart_ratio <- reactive({
    req(total_cart_events(), total_purchases())
    if (total_cart_events() == 0) {
      return(0)  # Avoid division by zero
    }
    round((total_purchases() / total_cart_events()) * 100, 2)  # Percentage
  })
  
  output$cart_ratio_box <- renderValueBox({
    valueBox(
      value = paste0(cart_ratio(), "%"),
      subtitle = "Cart-to-Purchase Ratio",
      icon = icon("percentage"),
      color = "purple"
    )
  })
  
  # Reactive data for period distribution
  output$period_barplot <- renderPlot({
    req(df_clean())  # Ensure data exists
    
    # Create period distribution
    period_distribution <- table(df_clean()$period)
    
    # Generate bar plot
    barplot(period_distribution,
            main = "Distribution of Event Times Throughout the Day",
            xlab = "Time Period", 
            ylab = "Frequency",
            col = "skyblue", 
            las = 1)
  })
  
  
  # Render Original Data Table
  output$original_data_table <- renderDT({
    req(df_original())
    datatable(df_original(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Render Cleaned Data Table
  output$cleaned_data_table <- renderDT({
    req(df_clean())
    datatable(df_clean(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Reactive plot for event counts by event type
  output$event_type_plot <- renderPlot({
    req(df_clean())  # Ensure data exists
    
    # Create summary table for event counts by event type
    event_type_count <- df_clean() %>%
      group_by(event_type) %>%
      summarise(event_count = n())
    
    # Generate ggplot bar chart
    ggplot(event_type_count, aes(x = event_type, y = event_count, fill = event_type)) +
      geom_bar(stat = "identity") +
      labs(title = "Event Counts by Event Type",
           x = "Event Type",
           y = "Event Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Reactive expression to calculate average price by hour
  avg_price_by_hour <- reactive({
    req(df_clean())  # Ensure cleaned data exists
    
    df_clean() %>%
      group_by(hour = as.numeric(substr(event_hour, 1, 2))) %>%  # Extract numeric hour
      summarise(avg_price = mean(price, na.rm = TRUE)) %>%
      arrange(hour)
  })
  
  # Render Line Chart for Average Price by Hour
  output$avg_price_hour_plot <- renderPlot({
    req(avg_price_by_hour())
    
    ggplot(avg_price_by_hour(), aes(x = hour, y = avg_price)) +
      geom_line(color = "blue") +
      geom_point(color = "red") +
      labs(title = "Average Price by Hour",
           x = "Hour of the Day",
           y = "Average Price") +
      theme_minimal()
  })
  
  # Render Table for Average Price by Hour
  output$avg_price_table <- renderDT({
    req(avg_price_by_hour())
    
    datatable(
      avg_price_by_hour() %>%
        mutate(avg_price = round(avg_price, 2)),  # Round price to 2 decimal places
      options = list(pageLength = 12, scrollX = TRUE),
      rownames = FALSE  # Remove row index
    )
  })
  
  # Reactive Data for Average Price by Day
  avg_price_by_day <- reactive({
    req(df_clean())  # Ensure data is available
    
    df_clean() %>%
      group_by(event_date) %>%
      summarise(avg_price = round(mean(price, na.rm = TRUE), 2))  # Round price to 2 decimal places
  })
  
  # Render the Line Chart for Average Price by Day
  output$avg_price_plot <- renderPlot({
    req(avg_price_by_day())  # Ensure data is available
    
    ggplot(avg_price_by_day(), aes(x = event_date, y = avg_price)) +
      geom_line(color = "blue") +
      geom_point(color = "red") +
      labs(title = "Average Price by Day",
           x = "Date",
           y = "Average Price") +
      theme_minimal()
  })
  
  # Reactive Data for Product Category Distribution
  category_distribution <- reactive({
    req(df_clean())  # Ensure data is available
    
    df_clean() %>%
      group_by(category) %>%
      summarise(product_count = n()) %>%
      arrange(desc(product_count))
  })
  
  # Render the Bar Plot for Product Category Distribution
  output$category_plot <- renderPlot({
    req(category_distribution())  # Ensure data is available
    
    ggplot(category_distribution(), aes(x = reorder(category, -product_count), y = product_count, fill = category)) +
      geom_bar(stat = "identity") +
      labs(title = "Product Category Distribution",
           x = "Product Category",
           y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Reactive expression to calculate RFM values
  rfm_data <- reactive({
    req(df_clean())  # Ensure data is available
    
    df_clean() %>%
      mutate(date = as.Date(event_time)) %>%  # Ensure date format
      group_by(user_id) %>%
      summarise(
        Recency = as.numeric(difftime(max(date, na.rm = TRUE), date, units = "days")), 
        Frequency = n(), 
        Monetary = sum(price, na.rm = TRUE)
      ) %>%
      arrange(desc(Monetary))  # Sort by highest spending customers
  })
  
  # Render the RFM table
  output$rfm_table <- renderDT({
    req(rfm_data())  # Ensure data exists
    
    datatable(rfm_data(), 
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)  # Remove row index
  })
  # Sample RFM Data (Replace this with actual RFM computation)
  df_rfm_sample <- data.frame(
    user_id = c(648775038),
    Recency = c(0),
    Frequency = c(19),
    Monetary = c(5961.34)
  )
  
  # Render RFM Table
  output$rfm_table_result <- renderDT({
    datatable(df_rfm_sample, rownames = FALSE, options = list(pageLength = 5, autoWidth = TRUE))
  })
  
  # Render RFM Explanation
  output$rfm_interpretation <- renderUI({
    HTML("
      <p><strong>What Do These Numbers Mean?</strong></p>
      <ul>
        <li><strong>Recency (R):</strong> The number is <b>0</b>, meaning this customer made a purchase <b>today</b>. They are <b>very active</b> and still shopping with you.</li>
        <li><strong>Frequency (F):</strong> The number is <b>19</b>, meaning this customer has bought from you <b>19 times</b>. This is a <b>loyal customer</b> who keeps coming back.</li>
        <li><strong>Monetary (M):</strong> The total amount spent is <b>$5,961.34</b>. This customer <b>spends a lot</b>, making them a <b>high-value customer</b>.</li>
      </ul>
      
      <p><strong>What Should You Do?</strong></p>
      <ul>
        <li>✅ This is one of your <b>best customers</b>!</li>
        <li>✅ They shop often and spend a lot.</li>
        <li>✅ Consider offering them <b>special discounts</b> or <b>VIP perks</b> to keep them engaged.</li>
      </ul>
      
      <p><strong>Issue Detected:</strong></p>
      <ul>
        <li>🔹 The same customer appears <b>multiple times</b> in the dataset.</li>
        <li>🔹 The RFM calculation should ensure each customer appears only <b>once</b>.</li>
      </ul>
    ")
  })
  
  rfm_data <- reactive({
    req(df_clean())  # Ensure cleaned data is available
    
    df_clean() %>%
      mutate(event_date = as.Date(event_time)) %>%
      group_by(user_id) %>%
      summarise(
        Recency = as.numeric(difftime(max(df_clean()$event_date), max(event_date), units = "days")),  
        Frequency = n(),  
        Monetary = sum(price, na.rm = TRUE)  
      ) %>%
      mutate(
        R_Score = ntile(-Recency, 4),
        F_Score = ntile(Frequency, 4),
        M_Score = ntile(Monetary, 4),
        RFM_Score = R_Score * 100 + F_Score * 10 + M_Score,
        Segment = case_when(
          R_Score == 4 & F_Score == 4 & M_Score == 4 ~ "Champions",
          R_Score >= 3 & F_Score >= 3 & M_Score >= 3 ~ "Loyal Customers",
          R_Score >= 2 & F_Score >= 2 & M_Score >= 2 ~ "Potential Loyalists",
          R_Score == 1 & F_Score >= 2 ~ "At Risk",
          R_Score == 1 & F_Score <= 2 ~ "Lost",
          TRUE ~ "Others"
        )
      ) %>%
      arrange(desc(Monetary))  
  })
  
  output$rfm_table <- renderDT({
    req(rfm_data())  
    datatable(rfm_data(), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  
  
  
  
}

# Run the app
shinyApp(ui = ui, server = server)
