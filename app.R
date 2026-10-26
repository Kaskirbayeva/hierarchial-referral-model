############################################################
# app.R - REFERRAL DESTINATION DECISION-SUPPORT PROTOTYPE
#
# Proof-of-concept Shiny app (shadow mode) for the hierarchical
# RF model. Asks only for information available to the referring
# clinician at referral (patient characteristics, diagnosis,
# admission and referring-facility details). Regional capacity
# and economic indicators are auto-filled from the selected
# region (2022 data used for model development).
#
# Predictions with top probability >= 70% are shown as a
# recommendation; lower-confidence predictions are flagged for
# manual review. The threshold is illustrative, not optimised.
# NOT a validated clinical decision-support tool.
#
# Run after the pipeline (scripts 04 and 15):
#   shiny::runApp()      # from the repository root
# With the default synthetic data the predictions are for
# demonstration only and carry no clinical meaning.
############################################################

library(shiny)
library(ranger)
library(tidyverse)

required_files <- c(
  "output/shiny/shiny_inputs.rds",
  "output/models/rf_stage1_drg_vs_nondrg.rds",
  "output/models/rf_stage2_city_vs_regional.rds",
  "output/models/rf_stage3_nondrg.rds"
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Missing file(s): ", paste(missing_files, collapse = ", "),
       ". Run 00_main.R (steps 04 and 15) first.")
}

shiny_inputs <- readRDS("output/shiny/shiny_inputs.rds")
rf_stage1 <- readRDS("output/models/rf_stage1_drg_vs_nondrg.rds")
rf_stage2 <- readRDS("output/models/rf_stage2_city_vs_regional.rds")
rf_stage3 <- readRDS("output/models/rf_stage3_nondrg.rds")

category_levels      <- shiny_inputs$category_levels
date_levels          <- shiny_inputs$date_factor_levels
regional_lookup      <- shiny_inputs$regional_lookup
class_order          <- shiny_inputs$class_order
non_drg_classes      <- shiny_inputs$non_drg_classes
CONFIDENCE_THRESHOLD <- shiny_inputs$confidence_threshold

predict_hierarchical <- function(newdata) {
  p1 <- predict(rf_stage1, data = newdata)$predictions
  p2 <- predict(rf_stage2, data = newdata)$predictions
  p3 <- predict(rf_stage3, data = newdata)$predictions
  prob <- c(
    unname(p1[1, "DRG"]) * p2[1, c("City", "Regional")],
    unname(p1[1, "NON_DRG"]) * p3[1, non_drg_classes]
  )
  names(prob) <- c("City", "Regional", non_drg_classes)
  prob[class_order]
}

ui <- fluidPage(
  titlePanel("Referral Destination Decision-Support Prototype"),
  div(style = "background-color:#fff3cd; padding:10px; border-radius:5px; margin-bottom:15px;",
      strong("Prototype for research demonstration only. "),
      "Not validated for clinical use. Final referral decisions remain the responsibility of the referring clinician."),
  sidebarLayout(
    sidebarPanel(
      h4("Patient characteristics"),
      numericInput("age", "Age (years)", value = 45, min = 0, max = 110),
      radioButtons("female", "Sex", choices = c("Female" = "1", "Male" = "0"), inline = TRUE),
      selectInput("residence", "Region of residence", choices = category_levels$residence),
      selectInput("employment_group", "Employment status", choices = category_levels$employment_group),
      selectInput("benefit_group", "MSHI benefit category", choices = category_levels$benefit_group),
      h4("Referral and admission details"),
      selectInput("refer_level", "Referring facility level", choices = category_levels$refer_level),
      selectInput("refer_region", "Referring facility region", choices = category_levels$refer_region),
      selectizeInput("icd10_ref", "Diagnosis at referral (ICD-10 code)", choices = NULL,
                     options = list(placeholder = "Start typing a code...")),
      selectInput("sur_type", "Surgery type", choices = category_levels$sur_type),
      selectInput("admission_day_of_week", "Admission day of week", choices = date_levels$admission_day_of_week),
      selectInput("admission_month", "Admission month", choices = date_levels$admission_month),
      actionButton("predict_btn", "Predict referral destination", class = "btn-primary")
    ),
    mainPanel(
      uiOutput("recommendation_box"),
      h4("Predicted probability by destination"),
      plotOutput("prob_plot", height = "300px"),
      h4("Regional indicators used (auto-filled)"),
      tableOutput("regional_context_table")
    )
  )
)

server <- function(input, output, session) {

  updateSelectizeInput(session, "icd10_ref", choices = category_levels$icd10_ref, server = TRUE)

  result <- eventReactive(input$predict_btn, {
    req(input$icd10_ref)
    region_row <- regional_lookup %>% filter(region == input$refer_region) %>% slice(1)
    if (nrow(region_row) == 0) {
      showNotification(paste("No regional indicators for", input$refer_region), type = "error")
      return(NULL)
    }
    newdata <- tibble(
      age = input$age, female = as.numeric(input$female),
      residence = input$residence, employment_group = input$employment_group,
      employment_region = region_row$employment_region, benefit_group = input$benefit_group,
      refer_level = input$refer_level, refer_region = input$refer_region,
      icd10_ref = input$icd10_ref, sur_type = input$sur_type,
      doctors_per10k = region_row$doctors_per10k, beds_per10k = region_row$beds_per10k,
      wage = region_row$wage, unemployment_rate = region_row$unemployment_rate,
      log_population = region_row$log_population, log_grp_pc = region_row$log_grp_pc,
      log_investment_pc = region_row$log_investment_pc,
      admission_day_of_week = factor(input$admission_day_of_week, levels = date_levels$admission_day_of_week, ordered = TRUE),
      admission_month       = factor(input$admission_month, levels = date_levels$admission_month, ordered = TRUE)
    )
    prob <- tryCatch(predict_hierarchical(newdata), error = function(e) {
      showNotification(paste("Prediction failed:", e$message), type = "error"); NULL
    })
    if (is.null(prob)) return(NULL)
    list(region_row = region_row, prob = prob, top_class = names(which.max(prob)), top_prob = max(prob))
  })

  output$recommendation_box <- renderUI({
    res <- result()
    if (is.null(res)) return(div(style = "color:#888;", "Enter referral details and click Predict."))
    high <- res$top_prob >= CONFIDENCE_THRESHOLD
    div(style = if (high) "background-color:#d4edda; padding:15px; border-radius:5px; border:1px solid #28a745;"
                else "background-color:#f8d7da; padding:15px; border-radius:5px; border:1px solid #dc3545;",
        h3(paste("Predicted destination:", res$top_class)),
        p(strong("Confidence: "), sprintf("%.1f%%", 100 * res$top_prob)),
        if (high) p(strong("High confidence"), " - shown as a recommendation.")
        else p(strong(sprintf("Below the %.0f%% confidence threshold", 100 * CONFIDENCE_THRESHOLD)),
               " - flagged for manual review."))
  })

  output$prob_plot <- renderPlot({
    res <- result(); req(res)
    tibble(class = factor(class_order, levels = rev(class_order)), probability = as.numeric(res$prob)) %>%
      ggplot(aes(class, probability)) +
      geom_col(fill = "grey30") +
      geom_text(aes(label = scales::percent(probability, accuracy = 0.1)), hjust = -0.1, size = 4) +
      geom_hline(yintercept = CONFIDENCE_THRESHOLD, linetype = "dashed", colour = "red") +
      coord_flip(ylim = c(0, 1.08)) +
      scale_y_continuous(labels = scales::percent) +
      theme_minimal(base_size = 13) + labs(x = NULL, y = "Predicted probability")
  })

  output$regional_context_table <- renderTable({
    res <- result(); req(res)
    r <- res$region_row
    tibble(Indicator = c("Doctors per 10,000 population", "Hospital beds per 10,000 population",
                         "Average monthly wage (tenge)", "Unemployment rate (%)"),
           Value = c(sprintf("%.1f", r$doctors_per10k), sprintf("%.1f", r$beds_per10k),
                     sprintf("%.0f", r$wage), sprintf("%.2f", r$unemployment_rate)))
  })
}

shinyApp(ui = ui, server = server)
