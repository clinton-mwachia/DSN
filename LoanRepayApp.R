library(shiny)
library(shinydashboard)
library(ggplot2)
library(loggit)
setLogFile("logfile.json")
loggit("INFO","app has started",app="start")
ui <- fluidPage(
   dashboardPage(skin = "purple",
                 
     dashboardHeader(
       title = "User Inputs:"
     ),
     dashboardSidebar(collapsed = T,
       sidebarMenu(
         menuItem("Enter the threshhold level",
                  sliderInput("threshhold","Select the threshhold level",0,1,.15)),
         menuItem("Personal Details",
           textInput("name","Enter the name of the client","Client Name"),
       selectInput("purpose","Select The Purpose Of The Loan",
                   choices = c("all_other","credit_card","debt_consolidation","educational","home_improvement","major_purchase","small_business")),
       sliderInput("intrate","Enter The Interest Rate",.06,.22,.10),
       sliderInput("instalment","Enter the installment",15.69,918.02,20.0),
       sliderInput("annualincome","Enter the annual income",8.294,14.528,10.0),
       sliderInput("fico","Enter the fico score",627,827,500),
       sliderInput("revolbal","Enter the borrowers revolvers balance",0,149527,10000),
       sliderInput("inqlast6mnths","Enter the borrower's number of inquiries by creditors in the last 6 months.",0,8,5),
       sliderInput("publrecord","Enter the borrower's number of derogatory public records.",0,3,2)
       ),
       submitButton("submit"))),
     dashboardBody(
       fluidPage(
         height="200px",
         fluidRow(
                  box(
                    height = "70px", width = "100px",
                    title = "Client name:",textOutput("name"),background = "aqua"),
                  box(
                    height = "70px", width = "100px",
                    "Purpose of the loan:",textOutput("purpose"),background ="purple",solidHeader = T
                    )
                    ),
        fluidRow(
                  box(
                    width = "auto",background = "blue",
                   "Summary table:",tableOutput("table"))),
                   fluidRow(
                    box(height = "70px",width = "100px",
                      "Loan status:",textOutput("LoanStatus"),background = "olive")
                      
                    ))
                      
                    )
                  
                  
           ))

server <- function(input,output, session){
  session$onSessionEnded(function(){loggit("INFO","app has stopped",app="stop")})
  output$name <- renderText({input$name})
  output$purpose <- renderText({input$purpose})
  output$table <- renderTable({
    thresh = input$threshhold
    Int.rate <- input$intrate
    Installment <- input$instalment
    Annual.income <- input$annualincome
    Fico <- input$fico
    Revol.bal <- input$revolbal
    Inq.6.months <- input$inqlast6mnths
    Pub.rec <- input$publrecord
    data.table::data.table(thresh,Int.rate,Installment,Annual.income,Fico,Revol.bal,Inq.6.months,Pub.rec)})
  output$LoanStatus <- renderText({
    thresh = input$threshhold
    R = input$revolbal
    INQ = input$inqlast6mnths
    PUBR = input$publrecord
    INT = input$intrate
    LOG = input$annualincome
    FICO = input$fico
    INST = input$instalment
    pre <- function(R,INQ,PUBR,INT,LOG,FICO,INST){
      p= 5.440167-0.000007371908*(R)+0.1839775 *(INQ)+0.4329467*(PUBR)+
        6.353772*(INT)-0.4609295*(LOG)-0.005319106 *(FICO) + 0.001249011*(INST)
      p = exp(p)/(1+exp(p))
      p
    }
    proceed = pre(R,INQ,PUBR,INT,LOG,FICO,INST)
      if(proceed > thresh){
        "The client is viable to borrow a loan"
      }else{
        "The client is not viable to borrow a loan"
      }
  })
}
shinyApp(ui = ui, server = server)

