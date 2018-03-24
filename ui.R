library(shiny)
shinyUI(pageWithSidebar (
  headerPanel( "Scheme Deposit Maturity Amount"),
  sidebarPanel(width=3,
    numericInput("interest", label = "Enter Interest Rate (in % i.e. 10 if it is 10%", value=8, min=0),
    selectInput("compounding", label="Compounding Frequency", choices=c(1,2,4,12)),
    textInput("inst", label = "Installment Size", value="500,1000,2000,5000"),
    textInput("maturity", label = "Maturity Period", value="2,3,5,10"),
    p(strong("Fill/Edit the Following 3 Fields Only If You Want To Find The Underline Interest Rate Of An Annuity")),
    numericInput("Tenure", label = "Tenure of the Annuity", value=0, min=0),
    numericInput("IS", label = "Pamyment Per Term", value=0, min=0),
    numericInput("MatAmt", label = "Amount Receivable", value=0, min=0),
    submitButton(text="Apply")
  ),
  mainPanel (
    h1("Amount to be Received"),
    tabsetPanel(
    tabPanel("Amount", tableOutput("P"), plotOutput("C")),
    tabPanel("Interest Rate", tableOutput("X"))
  ))
))
