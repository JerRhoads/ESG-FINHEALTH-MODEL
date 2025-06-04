library(shiny)
library(tidyverse)
library(ggplot2)

# Simulate data
set.seed(123)
n <- 100
data <- tibble(
  company = paste0("Firm_", 1:n),
  total_assets = runif(n, 500, 5000),
  total_liabilities = runif(n, 200, 4000),
  retained_earnings = runif(n, -500, 3000),
  ebit = runif(n, 100, 2000),
  market_cap = runif(n, 300, 5000),
  sales = runif(n, 500, 6000),
  esg_env = runif(n, 0, 10),
  esg_soc = runif(n, 0, 10),
  esg_gov = runif(n, 0, 10)
) %>%
  mutate(
    A = (total_assets - total_liabilities) / total_assets,
    B = retained_earnings / total_assets,
    C = ebit / total_assets,
    D = market_cap / total_liabilities,
    E = sales / total_assets,
    z_score = 1.2*A + 1.4*B + 3.3*C + 0.6*D + 1.0*E
  )

# PCA on ESG scores
esg_pca <- prcomp(data %>% select(esg_env, esg_soc, esg_gov), center = TRUE, scale. = TRUE)
data$esg_score <- scale(esg_pca$x[,1])
data$esg_adjusted_z <- data$z_score - 0.5 * data$esg_score

# UI
ui <- fluidPage(
  titlePanel("ESG-Adjusted Financial Health Explorer"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("esg_penalty", "ESG Penalty Weight:", min = 0, max = 1, value = 0.5, step = 0.1),
      selectInput("highlight", "Highlight Company:", choices = c("None", data$company))
    ),
    mainPanel(
      plotOutput("scatterPlot"),
      tableOutput("companyTable")
    )
  )
)

# Server
server <- function(input, output) {
  adjusted_data <- reactive({
    data %>%
      mutate(esg_adjusted_z = z_score - input$esg_penalty * esg_score)
  })

  output$scatterPlot <- renderPlot({
    plot_data <- adjusted_data()
    ggplot(plot_data, aes(x = z_score, y = esg_adjusted_z, label = company)) +
      geom_point(aes(color = esg_score)) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      geom_text(data = plot_data %>% filter(company == input$highlight), vjust = -1, color = "blue") +
      scale_color_gradient2(low = "red", high = "green", mid = "yellow", midpoint = 0) +
      labs(title = "ESG-Adjusted Z-Score", x = "Z-Score", y = "Adjusted Z") +
      theme_minimal()
  })

  output$companyTable <- renderTable({
    if (input$highlight != "None") {
      adjusted_data() %>% filter(company == input$highlight)
    }
  })
}

shinyApp(ui = ui, server = server)
