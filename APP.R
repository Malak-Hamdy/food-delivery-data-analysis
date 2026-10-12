# Food Delivery Service Efficiency Analysis
# Interactive Shiny app: data cleaning, exploratory plots,
# association rules (Apriori) and K-means clustering.
#
# How to run: keep this file and Food_Delivery_Times.csv in the same folder.
# Open that folder in R or RStudio and run:  shiny::runApp()

library(shiny)
library(bslib)
library(ggplot2)
library(arules)
library(arulesViz)

# ================== DATA CLEANING ==================

# Columns the analysis needs. An uploaded file must contain all of them.
required_cols <- c(
  "Distance_km", "Weather", "Traffic_Level", "Time_of_Day", "Vehicle_Type",
  "Preparation_Time_min", "Courier_Experience_yrs", "Delivery_Time_min"
)

# Most frequent non-missing value (table() ignores NA)
get_mode <- function(x) {
  counts <- table(x)
  names(counts)[which.max(counts)]
}

# Cleans a raw delivery data frame. It is used for the built-in data and for
# uploaded files, so both go through exactly the same steps.
clean_delivery_data <- function(raw) {
  df <- raw

  # 1. Blank text cells (",," in the CSV) are read as "" and not as NA.
  #    Convert them to NA so they are handled like other missing values.
  text_cols <- names(df)[vapply(df, is.character, logical(1))]
  for (col in text_cols) {
    df[[col]] <- trimws(df[[col]])
    df[[col]][which(df[[col]] == "")] <- NA
  }

  # 2. Fill missing values
  #    numeric -> median, text -> most frequent value (mode)
  exp_missing <- is.na(df$Courier_Experience_yrs)
  df$Courier_Experience_yrs[exp_missing] <-
    median(df$Courier_Experience_yrs, na.rm = TRUE)

  for (col in c("Traffic_Level", "Time_of_Day", "Weather")) {
    missing_here <- is.na(df[[col]])
    df[[col]][missing_here] <- get_mode(df[[col]])
  }

  # 3. Outliers: drop delivery times outside the IQR fences
  q1    <- quantile(df$Delivery_Time_min, 0.25, na.rm = TRUE)
  q3    <- quantile(df$Delivery_Time_min, 0.75, na.rm = TRUE)
  iqr   <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  keep  <- !is.na(df$Delivery_Time_min) &
           df$Delivery_Time_min >= lower &
           df$Delivery_Time_min <= upper
  df <- df[keep, ]
  rownames(df) <- NULL

  # 4. Derived columns
  df$speed         <- df$Distance_km / df$Delivery_Time_min * 60  # km per hour
  df$Late_Delivery <- ifelse(df$Delivery_Time_min > 60, 1, 0)

  df
}

# Built-in dataset, cleaned once when the app starts
CLEAN_DATA <- clean_delivery_data(read.csv("Food_Delivery_Times.csv"))

# ================== UI ==================
ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "minty"
  ),
  titlePanel("Food Delivery Service Efficiency Analysis"),

  sidebarLayout(
    sidebarPanel(
      h3("Upload Dataset"),
      fileInput("file", "Choose CSV File", accept = ".csv"),
      br(),

      h3("Algorithm Parameters"),

      h4("Association Rules (Apriori)"),
      sliderInput("support", "Minimum Support:",
                  min = 0.001, max = 0.1, value = 0.01, step = 0.001),
      sliderInput("confidence", "Minimum Confidence:",
                  min = 0.1, max = 1, value = 0.4, step = 0.05),
      br(),

      h4("K-Means Clustering"),
      numericInput("clusters", "Number of Clusters:",
                   value = 3, min = 2, max = 10),

      br(),
      actionButton("run", "Run Analysis", class = "btn btn-success")
    ),

    mainPanel(
      tabsetPanel(

        tabPanel("Data Preview",
                 h3("Dataset Head"),
                 tableOutput("head"),
                 h3("Structure"),
                 verbatimTextOutput("structure")
        ),

        tabPanel("Data Cleaning Summary",
                 verbatimTextOutput("cleaning_summary")
        ),

        tabPanel("Visualizations",
                 h3("Exploratory Plots"),
                 plotOutput("plot1", height = "900px")
        ),

        tabPanel("Association Rules",
                 h3("Apriori Rules"),
                 tableOutput("rules_table"),
                 plotOutput("rules_plot")
        ),

        tabPanel("K-Means Clustering",
                 h3("Cluster Plot"),
                 plotOutput("cluster_plot"),
                 h3("Cluster Summary"),
                 tableOutput("cluster_summary")
        )
      )
    )
  )
)

# ================== SERVER ==================
server <- function(input, output, session) {

  tdata_reactive <- reactive({
    if (is.null(input$file)) {
      CLEAN_DATA
    } else {
      raw <- read.csv(input$file$datapath)
      validate(need(
        all(required_cols %in% names(raw)),
        paste("Uploaded file is missing these columns:",
              paste(setdiff(required_cols, names(raw)), collapse = ", "))
      ))
      clean_delivery_data(raw)
    }
  })

  # ---- Visualizations
  output$plot1 <- renderPlot({
    tdata <- tdata_reactive()
    par(mfrow = c(3, 3))

    hist(tdata$Delivery_Time_min,
         col = "orange",
         main = "Delivery Time Distribution",
         xlab = "Delivery time (min)")

    boxplot(tdata$Delivery_Time_min,
            col = "lightblue",
            main = "Delivery Time Boxplot",
            ylab = "Delivery time (min)")

    boxplot(Delivery_Time_min ~ Vehicle_Type,
            data = tdata,
            col = "red",
            main = "Delivery Time by Vehicle Type",
            xlab = "Vehicle type",
            ylab = "Delivery time (min)")

    plot(tdata$Distance_km,
         tdata$Delivery_Time_min,
         col = "darkgreen",
         main = "Delivery Time vs Distance",
         xlab = "Distance (km)",
         ylab = "Delivery time (min)")

    hist(tdata$Distance_km,
         col = "gray",
         main = "Distance Distribution",
         xlab = "Distance (km)")

    hist(tdata$Preparation_Time_min,
         col = "lightgreen",
         main = "Preparation Time Distribution",
         xlab = "Preparation time (min)")

    boxplot(Delivery_Time_min ~ Traffic_Level,
            data = tdata,
            col = "turquoise",
            main = "Delivery Time vs Traffic Level",
            xlab = "Traffic level",
            ylab = "Delivery time (min)")

    barplot(table(tdata$Time_of_Day),
            col = "darkblue",
            main = "Orders by Time of Day",
            xlab = "Time of day",
            ylab = "Count")

    par(mfrow = c(1, 1))
  })

  # Data Preview
  output$head <- renderTable({
    head(tdata_reactive())
  })

  output$structure <- renderPrint({
    str(tdata_reactive())
  })

  output$cleaning_summary <- renderPrint({
    summary(tdata_reactive())
  })

  # ---- Association
  rules_reactive <- eventReactive(input$run, {
    tdata <- tdata_reactive()

    tdata$Distance_cat <- cut(tdata$Distance_km,
                              breaks = c(-Inf, 5, 10, 15, Inf),
                              labels = c("Short", "Medium", "Long", "VeryLong"))
    tdata$Prep_cat <- cut(tdata$Preparation_Time_min,
                          breaks = c(-Inf, 10, 20, Inf),
                          labels = c("Quick", "Moderate", "Slow"))
    tdata$Experience_cat <- cut(tdata$Courier_Experience_yrs,
                                breaks = c(-Inf, 2, 5, Inf),
                                labels = c("New", "Experienced", "Veteran"))
    tdata$Delivery_cat <- cut(tdata$Delivery_Time_min,
                              breaks = c(-Inf, 40, 70, Inf),
                              labels = c("Fast", "Average", "Slow"))

    cat_data <- tdata[, c("Distance_cat", "Prep_cat", "Experience_cat", "Delivery_cat",
                          "Weather", "Traffic_Level", "Time_of_Day", "Vehicle_Type")]
    # Convert to text before adding "Unknown". Filling a factor directly fails
    # because "Unknown" is not one of its levels: R warns and leaves NA.
    cat_data[] <- lapply(cat_data, as.character)
    cat_data[is.na(cat_data)] <- "Unknown"
    cat_data[] <- lapply(cat_data, as.factor)

    trans <- as(cat_data, "transactions")
    apriori(
      trans,
      parameter = list(
        supp   = input$support,
        conf   = input$confidence,
        minlen = 4,
        maxlen = 6
      )
    )
  })

  output$rules_table <- renderTable({
    req(rules_reactive())
    top_rules <- head(sort(rules_reactive(), by = "lift"), 20)
    as(top_rules, "data.frame")
  })

  output$rules_plot <- renderPlot({
    req(rules_reactive())
    top_rules <- head(sort(rules_reactive(), by = "lift"), 10)
    plot(top_rules, method = "graph", control = list(type = "items"))
  })

  # ---- K-means ----
  output$cluster_plot <- renderPlot({
    tdata <- tdata_reactive()

    cluster_data <- tdata[, c(
      "Distance_km",
      "Delivery_Time_min",
      "Preparation_Time_min",
      "speed"
    )]

    cluster_data <- data.frame(lapply(cluster_data, as.numeric))
    cluster_data_scaled <- scale(cluster_data)

    set.seed(123)
    km_res <- kmeans(
      x       = cluster_data_scaled,
      centers = input$clusters,
      nstart  = 25
    )

    tdata$Cluster_KM <- factor(km_res$cluster)

    centers_plot <- aggregate(
      cbind(Distance_km, Delivery_Time_min) ~ Cluster_KM,
      data = tdata,
      FUN  = mean
    )

    ggplot(tdata,
           aes(x = Distance_km,
               y = Delivery_Time_min,
               color = Cluster_KM)) +
      geom_point(alpha = 0.6) +
      geom_point(data = centers_plot,
                 aes(x = Distance_km, y = Delivery_Time_min, color = Cluster_KM),
                 size = 6, shape = 18, show.legend = FALSE) +
      theme_minimal() +
      labs(
        title = "K-means Clusters of Food Delivery Orders",
        x     = "Distance (km)",
        y     = "Delivery Time (min)",
        color = "Cluster"
      )
  })

  output$cluster_summary <- renderTable({
    tdata <- tdata_reactive()

    cluster_data <- tdata[, c(
      "Distance_km",
      "Delivery_Time_min",
      "Preparation_Time_min",
      "speed"
    )]

    cluster_data <- data.frame(lapply(cluster_data, as.numeric))
    cluster_data_scaled <- scale(cluster_data)

    set.seed(123)
    km_res <- kmeans(
      x       = cluster_data_scaled,
      centers = input$clusters,
      nstart  = 25
    )

    data.frame(
      Cluster = 1:length(km_res$size),
      Size    = km_res$size
    )
  })
}

shinyApp(ui = ui, server = server)
