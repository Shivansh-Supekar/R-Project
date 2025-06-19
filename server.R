# server.R

shinyServer(function(input, output) {
  
  output$data_info <- renderPrint({
    cat(
      "Heart Failure Dataset Information

This dataset contains 299 records of patients who had heart failure.
Each record includes 13 clinical features and an outcome variable.

Features:
- age: Age of the patient (in years)
- anaemia: 1 = Yes, 0 = No (low red blood cell count)
- creatinine_phosphokinase: Level of the CPK enzyme in the blood
- diabetes: 1 = Yes, 0 = No
- ejection_fraction: % of blood pumped out of heart with each beat
- high_blood_pressure: 1 = Yes, 0 = No
- platelets: Platelet count in the blood
- serum_creatinine: Level of creatinine in the blood
- serum_sodium: Sodium level in the blood
- sex: 1 = Male, 0 = Female
- smoking: 1 = Yes, 0 = No
- time: Follow-up period (in days)
- DEATH_EVENT: 1 = Patient died during follow-up, 0 = survived

Dataset Summary:
- Rows: 299
- Columns: 13
- No missing values

Source: Kaggle - Heart Failure Clinical Records Dataset"
    )
  })
  
  output$summary <- renderPrint({
    cat(" Summary of Numeric Variables\n\n")
    print(summary(df[c("age", "ejection_fraction", "serum_creatinine", "platelets", "time", "creatinine_phosphokinase", "serum_sodium")]))
    
    cat("\n Summary of Binary/Categorical Variables\n\n")
    print(summary(df[c("anaemia", "diabetes", "high_blood_pressure", "sex", "smoking", "DEATH_EVENT")]))
  })
  
  
  
  output$boxPlot <- renderPlot({
    ggplot(df, aes_string(x = "DEATH_EVENT", y = input$plot_var, fill = "DEATH_EVENT")) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = paste(input$plot_var, "by DEATH_EVENT"))
  })
  
  

  output$kmPlot <- renderPlot({

    # RETURN ONLY THE PLOT OBJECT
    ggsurvplot(km_fit, data = df, conf.int = TRUE)$plot
  })
  
  
  
  
  output$corrPlot <- renderPlot({
    df_corr <- df %>%
      select(all_of(numeric_cols))
    
    cor_matrix <- cor(df_corr)
    corrplot(cor_matrix, method = "color", addCoef.col = "black", 
             tl.cex = 0.8, number.cex = 0.7, tl.col = "black", 
             col = colorRampPalette(c("red", "white", "blue"))(200),
             mar = c(0,0,1,0), title = "Correlation Matrix")
  })
  
  
  output$logit_summary <- renderPrint({
    req(input$logit_vars)
    
    formula_str <- paste("DEATH_EVENT ~", paste(input$logit_vars, collapse = " + "))
    model <- glm(as.formula(formula_str), data = df, family = "binomial")
    or_data <- broom::tidy(model, exponentiate = TRUE, conf.int = TRUE)
    
    cat("Logistic Regression Results:\n\n")
    for (i in 2:nrow(or_data)) {  
      term <- or_data$term[i]
      or <- round(or_data$estimate[i], 2)
      low <- round(or_data$conf.low[i], 2)
      high <- round(or_data$conf.high[i], 2)
      pval <- round(or_data$p.value[i], 4)
      
      cat(sprintf("- %s:\n", term))
      cat(sprintf("   Odds Ratio (OR) = %.2f, 95%% CI = [%.2f, %.2f], p-value = %.4f\n", or, low, high, pval))
      
      if (pval < 0.05) {
        if (or > 1) {
          cat("    Interpretation: Statistically significant. This variable is associated with **higher** odds of death.\n\n")
        } else {
          cat("    Interpretation: Statistically significant. This variable is associated with **lower** odds of death.\n\n")
        }
      } else {
        cat("    Interpretation: Not statistically significant. No strong evidence of association.\n\n")
      }
    }
  })
  
  
  output$cox_summary <- renderPrint({
    req(input$cox_vars)
    
    formula_str <- paste("Surv(time, DEATH_EVENT_num) ~", paste(input$cox_vars, collapse = " + "))
    cox_model <- coxph(as.formula(formula_str), data = df)
    hr_data <- broom::tidy(cox_model, exponentiate = TRUE, conf.int = TRUE)
    
    cat("Cox Proportional Hazards Model Results:\n\n")
    for (i in 1:nrow(hr_data)) {
      term <- hr_data$term[i]
      hr <- round(hr_data$estimate[i], 2)
      low <- round(hr_data$conf.low[i], 2)
      high <- round(hr_data$conf.high[i], 2)
      pval <- round(hr_data$p.value[i], 4)
      
      cat(sprintf("- %s:\n", term))
      cat(sprintf("   Hazard Ratio (HR) = %.2f, 95%% CI = [%.2f, %.2f], p-value = %.4f\n", hr, low, high, pval))
      
      if (pval < 0.05) {
        if (hr > 1) {
          cat("    Interpretation: Statistically significant. This variable is associated with **increased** risk of death over time.\n\n")
        } else {
          cat("    Interpretation: Statistically significant. This variable is associated with **decreased** risk of death over time.\n\n")
        }
      } else {
        cat("    Interpretation: Not statistically significant. No strong evidence of association.\n\n")
      }
    }
  })
  
  
  
})

