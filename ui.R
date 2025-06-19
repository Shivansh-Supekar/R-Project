# ui.R

shinyUI(fluidPage(
  shinythemes::shinytheme("cosmo"),
  titlePanel("Heart Failure Data Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("plot_var","Choose Variable for Boxplot",choices = numeric_cols),
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Information",verbatimTextOutput("data_info")),
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("Boxplot", plotOutput("boxPlot")),
        tabPanel("KM Curve", plotOutput("kmPlot")),
        tabPanel("Correlation Matrix", plotOutput("corrPlot")),
        tabPanel("Logistic Findings", 
                 checkboxGroupInput("logit_vars", "Select Variables for Logistic Regression:", 
                                    choices = c("age", "ejection_fraction", "serum_creatinine", 
                                                "platelets", "serum_sodium", "creatinine_phosphokinase", 
                                                "sex", "anaemia", "diabetes", "high_blood_pressure", "smoking")),
                 verbatimTextOutput("logit_summary")),
        
        tabPanel("Cox Findings", 
                 checkboxGroupInput("cox_vars", "Select Variables for Cox Model:", 
                                    choices = c("age", "ejection_fraction", "serum_creatinine", 
                                                "platelets", "serum_sodium", "creatinine_phosphokinase", 
                                                "sex", "anaemia", "diabetes", "high_blood_pressure", "smoking")),
                 verbatimTextOutput("cox_summary"))
        
        
      )
    )
  )
))
