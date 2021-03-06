library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(DT)
library(readxl)
library(writexl)

donations <- try(read_csv("data/donations.csv") %>%
                     mutate(Date = as_date(Date)))
if (inherits(donations, "try-error")) {
    donations <- tibble(Date = ymd(), DonorID = numeric(),
                                                      Type = character(), Account = character(),
                                                      Amount = numeric(), Initiative = character(),
                                                      Donor = character())
    
    write_csv(donations, "data/donations.csv")
}

## Set images resource path
addResourcePath("images", "images")

ui <- fluidPage(theme = shinytheme("cerulean"),
                
   includeCSS("css/styles.css"),
   
   # Application title
   titlePanel("Seasons of Giving"),
   
   sidebarLayout(
      sidebarPanel(width = 3,
         a(href = "https://oaiti.org", target = "_blank", img(src = "images/oaiti_transparent.png", width = "135")),
         h4("About"),
         HTML("This customizable application let's your non profit track and analyze donations. 
              We support adding and importing of structured donation data, and then provide several dashboard pages 
              in order to visualize the trend in giving over time across a number of variables and factors."),
         
         hr(),
         
         h4("Import"),
         
         HTML("You can import your own data. The data must be in a suitable structure described <a href = 'https://github.com/OAITI/givings'>here</a>. 
              <b>Warning: Uploading your own data will replace all existing data in the app!</b>"),
         
         br(), br(),
         
         fileInput("import", "Donations Table",
                   accept = c(
                       ".xls",
                       ".xlsx",
                       "text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")
         ),
         
         hr(),
         
         h4("Export"),
         
         HTML("You can export your full donations data below."),
         
         br(), br(),
         
         downloadButton("export", "Export")
      ),
      
      mainPanel(width = 9,
          tabsetPanel(
              tabPanel(title = "Donation Entries", 
                       wellPanel(
                           h4("Donation Entry"),
                           dateInput("date", label = "Date:"),
                           fluidRow(column(6, h5(textOutput("donorID"))),
                                    column(6, column(1, icon("user-friends", lib = "font-awesome")), checkboxInput("joint", "First Joint Contribution", value = FALSE))
                                    ),
                           selectizeInput("donorname", "Donor:", choices = sort(unique(donations$Donor)), options = list(create = TRUE)),
                           ## If Joint is selected the Donor ID is shown and option to enter one more donor name
                           conditionalPanel(
                               condition = "input.joint == true",
                               selectizeInput("donor2name", "Joint Donor: ", choices = sort(unique(donations$Donor)), options = list(create = TRUE))
                           ),
                
                           fluidRow(column(6, selectizeInput("account",
                                                          "Account:",
                                                          choices = sort(donations$Account), options = list(create = TRUE))),
                                    column(6, selectizeInput("initiative",
                                                          "Initiative:", 
                                                          choices = NULL,
                                                          options = list(create = TRUE)))),
                           
                           fluidRow(column(6, numericInput("amount",
                                        "Amount:", value = 1, min = 1, step = 10)),
                                    column(6, selectInput("type",
                                                          "Type:",
                                                          choices = c("Cash", "Check", "Online", "Other")))),
                           
                           actionButton("save", "Add Donation", icon = icon("plus-square", lib = "font-awesome"))
                       ),
                       hr(),
                       h4(textOutput("dayTotalGiven")),
                       hr(),
                       h4("Sanity Check - Entered Data"),
                       DTOutput("donationTable", height = "30%")
                       
                       ),
              tabPanel(title = "Church Dashboard",
                       wellPanel(
                           h4("Configuration"),
                           dateRangeInput("daterange1", "Date Range:")
                       ),
                       br(), br(),
                       h4(textOutput("totalGiven")),
                       br(), br(),
                       plotOutput("accDist"), br(),
                       plotOutput("donorDist"),
                       br(),
                       hr(),
                       DTOutput("churchTable", height = "30%")
                       ),
              tabPanel(title = "Account Dashboard",
                       wellPanel(
                           h4("Configuration"),
                           
                           selectizeInput("acc_report", label = "Enter Account:", choices = sort(unique(donations$Account))),
                           dateRangeInput("daterange2", "Date Range:")
                       ),
                       br(), br(),
                       h4(textOutput("acctotalGiven")),
                       br(), br(),
                       fluidRow(column(6, plotOutput("acctimePlot")),column(6, plotOutput("uniqueDonors"))),
                       br(),
                       plotOutput("accdonorDist"),
                       br(),
                       plotOutput("accinitDist"),
                       br(),
                       hr(),
                       DTOutput("accTable", height = "30%")
              ),
              tabPanel(title = "Individuals Dashboard",
                       wellPanel(
                           h4("Configuration"),
                           
                           selectizeInput("donorname_report", label = "Enter Donor Name:", choices = sort(unique(donations$Donor))),
                           dateRangeInput("daterange3", "Date Range:")
                       ),
                       br(), br(),
                       h4(textOutput("donortotalGiven")),
                       br(), br(),
                       plotOutput("donortimePlot"),
                       br(),
                       plotOutput("donoraccDist"),
                       br(),
                       hr(),
                       DTOutput("indivTable", height = "30%")
              )
              
          )
          )
   )
)

server <- function(input, output, session) {
    
    donations <- reactiveFileReader(500, session, "data/donations.csv", read_csv) 

    #####################
    ## DONATION ENTRIES
    #####################
   output$donorID <- renderText({       
       if(!input$joint){
           curID <- donations()[donations()$Donor == input$donorname,]$DonorID[1]
           paste("ID: ", curID)
       } else
           paste("ID: ", NA)
    })
   
   observe({
       donorselect <- donations()[donations()$Donor != input$donorname,]$Donor
       updateSelectInput(session, "donor2name", choices = donorselect)
   })
   
   observeEvent(input$save, {
       # Save Donations File
       # Check if new Donor and add Donor ID - save donors file
       # Check if new Account and add Account - save accounts file
       date <- input$date
       donorID <- donations()[donations()$Donor == input$donorname,]$DonorID[1]
       donorname <- input$donorname
       type <- input$type
       account <- input$account
       amount <- input$amount
       initiative <- input$initiative

       if(input$joint) {
          # merge donorname
          donorname <- paste(input$donor2name, "and", input$donorname)
          # reset donorID again
          donorID <- NA
       } 
       if (is.na(donorID)) {
           # Add to donors table
           if (nrow(donations()) == 0) {
               donorID <- 1
           } else {
               donorID <- max(donations()$DonorID) + 1
           }
       }
       # TODO: have a try catch here instead?
       if (!(is.null(amount) || nchar(amount) == 0)) {
       #    donationID <- max(donations()$DonationID) + 1 # if ever donations need to be deleted
           new_donation <- tibble(Date = date, DonorID = donorID, Type = type, Account = account,
                                      Initiative = initiative, Amount = amount, Donor = donorname)
           write_csv(rbind(donations(), new_donation), "data/donations.csv")
           # If it was a joint donor, reset the checkbox and show added name
           updateCheckboxInput(session, "joint", value = FALSE)
           updateSelectizeInput(session, "donorname", 
                                choices = sort(unique(rbind(donations(), new_donation)$Donor)), selected = donorname)
           
       }

   })
   
   # observe({
   #     accounts_init <- sort(unique(donations()$Account))
   #     accounts <- sapply(strsplit(accounts_init, "_"), `[`, 1)
   #     updateSelectInput(session, "account", choices = sort(unique(accounts)))
   # })
   
   observe({
       initiatives <- unique(donations()[donations()$Account == input$account,]$Initiative)
      # initiatives <- initiatives[grep(paste0(input$account, "_"), initiatives)]

       if (sum(is.na(initiatives)) > 0) {
           updateSelectInput(session, "initiative", choices = sort(initiatives))
       } else {
           updateSelectInput(session, "initiative", choices = "")
       }
   })
   
   output$dayTotalGiven <- renderText({ 
       if (nrow(donations()) == 0) return(NULL)

      dayTotal <- donations() %>%
           filter(Date == input$date, Donor == input$donorname) %>% 
           summarise(sum(Amount))
       paste0("Day Total: $", dayTotal)
   })
   
   observe({
       updateDateRangeInput(session, "daterange1", start = min(donations()$Date), end = max(donations()$Date))
       updateDateRangeInput(session, "daterange2", start = min(donations()$Date), end = max(donations()$Date))
       updateDateRangeInput(session, "daterange3", start = min(donations()$Date), end = max(donations()$Date))
   })
   
   #####################
   ## DATE BASED PLOTS - CHURCH DASHBOARD
   #####################
   
   d11 <- reactive({
       as.Date(format(input$daterange1[1]))
   })
   d21 <- reactive({
       as.Date(format(input$daterange1[2]))
   })
   
   output$totalGiven <- renderText({ 
       if (nrow(donations()) == 0) return(NULL)
       
       total <- donations() %>%
           filter(Date >= d11() & Date <= d21()) %>% 
           summarise(sum(Amount))
       paste0("Total Funds: $", total )
   })
   
   output$accDist <- renderPlot({
       if (nrow(donations()) == 0) return(NULL)
       
       sum_donations <- donations() %>%
           filter(Date >= d11(), Date <= d21()) %>%
           group_by(Account) %>%
           summarise(Total = sum(Amount)) %>%
           arrange(desc(Total)) %>%
           mutate(Account = factor(Account, levels = rev(Account))) 
       
       ggplot(sum_donations, aes(x = Account, y = Total)) +
           geom_bar(stat = "identity") +
           theme_minimal() +
           scale_y_continuous(labels = scales::dollar) +
           labs(title = "List of Accounts in order of Funds",
                subtitle = paste0("From ", d11(), " to ", d21())) +
           coord_flip()
   })
   
   output$donorDist <- renderPlot({
       if (nrow(donations()) == 0) return(NULL)
       
       sum_donations <- donations() %>%
           filter(Date >= d11() & Date <= d21()) %>%
           group_by(Donor) %>%
           summarise(Total = sum(Amount)) %>%
           arrange(desc(Total)) %>%
           mutate(Donor = factor(Donor, levels = rev(Donor))) 
       
       ggplot(sum_donations, aes(x = Donor, y = Total)) +
           geom_bar(stat = "identity") +
           theme_minimal() +
           scale_y_continuous(labels = scales::dollar) +
           labs(title = "List of Donors in order of Donation Amounts", 
                subtitle = paste0("From ", d11(), " to ", d21())) +
           coord_flip()
   })
   
   output$churchTable <- renderDT({
       donations() %>%
           filter(Date >= d11(), Date <= d21()) %>%
           arrange(desc(Date)) %>%
           datatable(caption = paste0("Donations to the Church From ", d11(), " to ", d21()))
   }, rownames = FALSE)
   
   #####################
   ## ACCOUNT BASED PLOTS - ACC DASHBOARD
   #####################
   
   observe({
       updateSelectInput(session, "acc_report", choices = sort(unique(donations()$Account)))
   })

   d12 <- reactive({
       as.Date(format(input$daterange2[1]))
   })
   d22 <- reactive({
       as.Date(format(input$daterange2[2]))
   })
   
   acc <- reactive({ 
       input$acc_report
   })
   output$acctotalGiven <- renderText({ 
       if (nrow(donations()) == 0) return(NULL)
    
       total <- donations() %>%
           filter(Date >= d12() & Date <= d22(), Account == acc()) %>% 
           summarise(sum(Amount))
       paste0("Total Amount in ", acc(), ": $", total )
   })
   
   output$acctimePlot <- renderPlot({
       if (nrow(donations()) == 0) return(NULL)
       
       all_dates <- tibble(Date = seq.Date(round_date(d12(), unit = "week"), round_date(d22(), unit = "week"),
                                           by = "weeks"))
       weekly_donations <- donations() %>%
           filter(Date >= d12(), Date <= d22(), Account == acc()) %>%
           mutate(Week = as.Date(round_date(Date, unit = "week"))) %>%
           right_join(all_dates, by = c("Week" = "Date")) %>%
           replace_na(list(Amount = 0)) %>%
           group_by(Week) %>%
           summarise(`Weekly Amount` = sum(Amount)) 
      
       ggplot(weekly_donations, aes(x = Week, y = `Weekly Amount`)) +
           geom_line() +
           theme_minimal() +
           scale_y_continuous(#limits = c(0, max(weekly_donations$`Weekly Amount`)+5), 
                              breaks = scales::pretty_breaks(n = 10), labels = scales::dollar) +
           scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
           labs(title = paste0("Account Giving over Time for ", acc()),
                subtitle = paste0("From ", d12(), " to ", d22()),
                x = "Date", y = "Weekly Amount")
   })
   output$uniqueDonors <- renderPlot({ 
       if (nrow(donations()) == 0) return(NULL)
       
       all_dates <- tibble(Date = seq.Date(round_date(d12(), unit = "week"), round_date(d22(), unit = "week"),
                                           by = "weeks"))
       unique_donors <- donations() %>%
           filter(Date >= d12(), Date <= d22(), Account == acc()) %>%
           mutate(Week = as.Date(round_date(Date, unit = "week"))) %>%
           group_by(Week) %>%
           summarise(Number = length(unique(Donor))) %>%
           right_join(all_dates, by = c("Week" = "Date")) %>%
           replace_na(list(Number = 0))
       ggplot(unique_donors, aes(x = Week, y = Number)) +
           geom_line() +
           theme_minimal() +
           scale_y_continuous(#limits = c(0, max(unique_donors$Number)), 
                              breaks = scales::pretty_breaks(n = max(unique_donors$Number)), labels = scales::comma) +
           scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
           labs(title = paste0("Unique Donors over Time for ", acc()), 
                subtitle = paste0("From ", d12(), " to ", d22()), x = "Date", y = "Weekly Donors")
   })
   
   output$accdonorDist <- renderPlot({
       if (nrow(donations()) == 0) return(NULL)
       
       sum_donations <- donations() %>%
           filter(Date >= d12(), Date <= d22(), Account == acc()) %>%
           group_by(Donor) %>%
           summarise(Total = sum(Amount)) %>%
           arrange(desc(Total)) %>%
           mutate(Donor = factor(Donor, levels = rev(Donor))) 
       
       ggplot(sum_donations, aes(x = Donor, y = Total)) +
           geom_bar(stat = "identity") +
           theme_minimal() +
           scale_y_continuous(labels = scales::dollar) +
           labs(title = paste0("Donors Giving to ", acc()), 
                subtitle = paste0("From ", d12(), " to ", d22())) +
           coord_flip()
   })
   
   output$accinitDist <- renderPlot({
       if (nrow(donations()) == 0) return(NULL)
       
       initiatives_present <- donations() %>%
           filter(Date >= d12(), Date <= d22(), Account == acc()) %>%
           .$Initiative
       
       if(sum(!is.na(initiatives_present)) > 0) {
           sum_donations <- donations() %>%
               filter(Date >= d12(), Date <= d22(), Account == acc()) %>%
               group_by(Initiative) %>%
               summarise(Total = sum(Amount)) %>%
               arrange(desc(Total)) %>%
               mutate(Initiative = factor(Initiative, levels = rev(Initiative))) 
           
           ggplot(sum_donations, aes(x = Initiative, y = Total)) +
               geom_bar(stat = "identity") +
               theme_minimal() +
               scale_y_continuous(labels = scales::dollar) +
               labs(title = paste0("Initiatives Under ", acc()), subtitle = paste0("From ", d12(), " to ", d22())) +
               coord_flip()
       }
   })
   
   output$accTable <- renderDT({
       donations() %>%
           filter(Date >= d12(), Date <= d22(), Account == acc()) %>%
           arrange(desc(Date)) %>%
           datatable(caption = paste0("Donations to ", acc()," From ", d12(), " to ", d22()))
   }, rownames = FALSE)
   
   #####################
   ## INDIVIDUAL BASED PLOTS - INDIVIDUAL DASHBOARD
   #####################
   
   observe({
       updateSelectInput(session, "donorname_report", choices = sort(unique(donations()$Donor)))
   })
   d13 <- reactive({
       as.Date(format(input$daterange3[1]))
   })
   d23 <- reactive({
       as.Date(format(input$daterange3[2]))
   })
   donor <- reactive({
       input$donorname_report
   })
   
   output$donortotalGiven <- renderText({ 
       if (nrow(donations()) == 0) return(NULL)
       
       total <- donations() %>%
           filter(Date >= d13() & Date <= d23(), Donor == donor()) %>% 
           summarise(sum(Amount))
       paste0("Total Given by ", donor(), ": $", total)
   })
   
   output$donortimePlot <- renderPlot({
       if (nrow(donations()) == 0) return(NULL)
       
       all_dates <- tibble(Date = seq.Date(round_date(d13(), unit = "week"), round_date(d23(), unit = "week"),
                                           by = "weeks"))
       weekly_donations <- donations() %>%
           filter(Date >= d13(), Date <= d23(), Donor == donor()) %>%
           mutate(Week = as.Date(round_date(Date, unit = "week"))) %>%
           right_join(all_dates, by = c("Week" = "Date")) %>%
           replace_na(list(Amount = 0)) %>%
           group_by(Week) %>%
           summarise(`Weekly Amount` = sum(Amount)) 
       
       ggplot(weekly_donations, aes(x = Week, y = `Weekly Amount`)) +
           geom_line() +
           theme_minimal() +
           scale_y_continuous(#limits = c(0, max(weekly_donations$`Weekly Amount`)+5), 
                              breaks = scales::pretty_breaks(n = 10), labels = scales::dollar) +
           scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
           labs(title = paste0(donor(), "'s Giving"), 
                subtitle = paste0("From ", d13(), " to ", d23()),  x = "Date", y = "Weekly Amount")
   })
   output$donoraccDist <- renderPlot({
       if (nrow(donations()) == 0) return(NULL)

       sum_donations <- donations() %>%
           filter(Date >= d13(), Date <= d23(), Donor == donor()) %>%
           group_by(Account) %>%
           summarise(Total = sum(Amount)) %>%
           arrange(desc(Total)) %>%
           mutate(Account = factor(Account, levels = rev(Account))) 
       
       ggplot(sum_donations, aes(x = Account, y = Total)) +
           geom_bar(stat = "identity") +
           theme_minimal() +
           scale_y_continuous(labels = scales::dollar) +
           labs(title = paste0(donor(), "'s Favorite Accounts"),
                subtitle = paste0("From ", d13(), " to ", d23())) +
           coord_flip()
   })
   
   output$indivTable <- renderDT({
       donations() %>%
           filter(Date >= d13(), Date <= d23(), Donor == donor()) %>%
           arrange(desc(Date)) %>%
           datatable(caption = paste0(donor(), "'s Giving From ", d13(), " to ", d23()))
   }, 
   rownames = FALSE)
   
   # TODO: Remove this-only to check if tables are populating right!
   output$donationTable <- renderDT({
       donations() %>%
           arrange(desc(Date))
   }, rownames = FALSE)
   
   ## Export routine
   output$export <- downloadHandler(
     filename = function() {
         if (!is.null(input$import$datapath) && tools::file_ext(input$import$datapath) == "csv") {
             paste('donations-', Sys.Date(), '.csv', sep='')
         } else {
             paste('donations-', Sys.Date(), '.xlsx', sep='')
         }
     },
     content = function(con) {
         # Make format wide
         donations_wide <- donations() %>%
             dplyr::rename(Family = Donor) %>%
             mutate(Account = ifelse(is.na(Initiative), Account, paste0(Account, "_", Initiative))) %>%
             group_by(Date, Family, Type, Account) %>%
             summarise(Amount = sum(Amount)) %>%
            # unite(col = "Account", Account, Initiative, sep = "_") %>%
             spread(key = Account, value = Amount, fill = 0) 
         rowsums <- rowSums(donations_wide[,setdiff(names(donations_wide), c("Initiative", "Date","Family","Type","Total"))], na.rm = TRUE)
         donations_wide$Total <- rowsums
         if (!is.null(input$import$datapath) && tools::file_ext(input$import$datapath) == "csv") {
             write_csv(donations_wide, con)
         } else {
             writexl::write_xlsx(donations_wide, con) 
         }
       
     }
   )
   
   ## Import routine
   observe({
       inFile <- input$import
       
       if (!is.null(inFile)) {
           if (tools::file_ext(inFile$datapath) == "csv") {
               import_data <- read_csv(inFile$datapath) 
           } else {
               import_data <- read_excel(inFile$datapath)
           }
       
           x <- import_data
           
           ## Check if wide...
           if (!("Account" %in% names(import_data))) {
               x <- x %>%
                   dplyr::rename(Donor = Family) %>%
                   select(-Total) %>%
                   arrange(Date) %>%
                   mutate(DonorID = as.numeric(factor(Donor, levels = unique(Donor)))) %>%
                   gather(key = Account, value = Amount, 4:(ncol(.) - 1)) %>%
                   filter(Amount > 0) %>%
                   separate(col = "Account", into = c("Account", "Initiative"), sep = "_", extra = "merge")
           }
           x <- x %>%
               mutate(Date = as_date(Date))
           
           write_csv(x, "data/donations.csv")
       }
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

