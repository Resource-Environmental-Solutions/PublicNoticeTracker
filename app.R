#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(leaflet)
library(janitor)
library(readxl)
library(dplyr)
library(DT)
library(ggplot2)
library(shinyauthr)
library(googlesheets4)
library(lubridate)


#set up userinformation
user_base <- tibble::tibble(
  user = c("RES1", "RES2"),
  password = c("stream1", "stream2"),
  name = c("User 1", "User 2")
)

#tell google sheets to not require an authorization (global link)
gs4_deauth()

#load in data and edit names to R-friendly formatting
#pns <- read_xlsx("R:/Sales and Business Analysis/Demand/Public Notices/PublicNoticeReport_Data_Master_2024.xlsx")
sa <-"https://docs.google.com/spreadsheets/d/1UHV_bhJq5MC5J6gGEBKz5k5nYG1hktZQCnCX-hGypsg/edit?usp=sharing"
pns <- as.data.frame(read_sheet(sa, col_types = "cccccTcccddcccccccccdcccccddcnccccccccc", na = c("", "NA", "NULL", NULL)))
pns <- clean_names(pns, case = "snake")

ss <- "https://docs.google.com/spreadsheets/d/1wY7qwfGulrR3oP5OmFpt7YTy13aqRdahn3a__A4jyDg/edit?usp=sharing"
banks <- as.data.frame(read_sheet(ss, col_types = "cccccTccccddccccccdcTTccccnnnc",na = c("","NA","NULL",NULL)))

#banks <- read_xlsx("R:/Sales and Business Analysis/Demand/Public Notices/PendingMitigationBank_Data_Master.xlsx")
banks <- clean_names(banks, case = "snake")

#Create impact resource color palete
resource_colors <- colorFactor(palette = c("deepskyblue4", "chartreuse3", "brown", "pink", "blue"), 
                               levels = c("Stream", "Wetland", "Other - AC", "Other - LF", "Open Water"))

#Create bank timing color palette
timing_colors <-colorFactor(palette = c("gold", "dodgerblue3", "firebrick3"), 
                            levels = c("New this month", "Pending", "Approved in last year"))

#Public Notice grouping and cleaning
# Convert 'Date Posted' to POSIXlt format
pns <- pns %>%
  mutate(date_posted = as.Date(date_posted))
banks <- banks %>%
  mutate(pn_posted = as.Date(pn_posted),
         approval_date = as.Date(approval_date),
         first_credit_release = x1st_credit_release_date)


#create a vector of all dates in last year
last_year <- as.Date(seq(today(), today() - years(1), by = "-1 day"))

#create a vector of all dates in last month
last_month <- as.Date(seq(today(), today() - months(1), by = "-1 day"))

#add a column specifying bank timing
banks <- banks %>%
  mutate(timing = case_when(pn_posted %in% last_month ~ "New this month",
                            approval_date %in% last_year ~ "Approved in last year",
                            approval_date < as.Date(today() - years(1)) ~ "Approved", 
                            .default = "Pending"
                            ))

#filter banks to just those not approved

banks <- banks %>%
  filter(ribits_status != "Withdrawn",
         ribits_status != "Terminated",
         timing != "Approved")

#consolidate information for each bank
banks <- banks %>%
  group_by(permit_number) %>%
  summarize(id = first(id), 
            initial = first(initial), 
            district = first(district),
            state = first(state), 
            permit_number = first(permit_number),
            pn_posted = first(pn_posted),
            huc = first(huc),
            watershed = first(watershed),
            county = first(county),
            latitude = first(latitude),
            longitude = first(longitude),
            bank_name = first(bank_name),
            bank_sponsor = first(bank_sponsor),
            consultant = first(consultant),
            bank_description = first(bank_description),
            habitat_credit_type = paste(unique(habitat_credit_type), collapse = ", "),
            mitigation_type = paste(mitigation_type, collapse = ", "),
            acres_lf = paste(acres_lf, collapse = ", "),
            ribits_status = first(ribits_status),
            approval_date = first(approval_date),
            first_credit_release = first(first_credit_release),
            notes = paste(unique(notes), collapse = ", "),
            primary_service_area = first(primary_service_area),
            secondary_service_area = first(secondary_service_area),
            tertiary_service_area = first(tertiary_service_area),
            year = first(year),
            month = first(month),
            region = first(region),
            service_area = first(service_area),
            timing = first(timing)
  )  


# Convert Latitude and Longitude to numeric and filter out invalid values
pns <- pns %>%
  mutate(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  ) %>%
  filter(
    !is.na(latitude) & !is.na(longitude) &
      latitude >= -90 & latitude <= 90 &
      longitude >= -180 & longitude <= 180
  )

banks <- banks %>%
  mutate(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  ) %>%
  filter(
    !is.na(latitude) & !is.na(longitude) &
      latitude >= -90 & latitude <= 90 &
      longitude >= -180 & longitude <= 180
  )

#Alter locations slightly to avoid overlapping markers

pns <- pns %>%
  mutate(
    jittered_lat = jitter(latitude, amount = 0.001),
    jittered_lng = jitter(longitude, amount = 0.001)
  )

banks <- banks %>%
  mutate(
    jittered_lat = jitter(latitude, amount = 0.001),
    jittered_lng = jitter(longitude, amount = 0.001)
  )

# Define UI for application that draws a map
ui <- fluidPage(
  # add logout button UI
  #div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
  # add login panel UI function
  shinyauthr::loginUI(id = "login"),
  
  div(
    id = "show-page",
    #add title panel to all tabs
    titlePanel("Public Notice and Pending Bank Tracker"),
    #set a bootswatch theme to save on extra formatting
    theme = bslib::bs_theme(bootswatch = "sandstone"),
    #create a set of tabs
    tabsetPanel(
      #create the first tab
     tabPanel("Impact Public Notices",
      #use a sidebar layout with a sidebar panel
      sidebarLayout(
        sidebarPanel(
          #create the list of inputs
         dateRangeInput("date", "Date Posted", start = "2024-01-01"),
         selectInput("agency", "USACE District or State Agency",
                      choices = sort(as.character(unique(pns$agency))), multiple = TRUE),
          selectInput("state", "State", choices = sort(as.character(unique(pns$state))), multiple = TRUE),
          selectInput("service_area", "Service Area", 
                     choices = sort(as.character(unique(pns$service_area))), multiple = TRUE), 
          checkboxGroupInput("resource", "Resource Type", 
                             choices = sort(as.character(unique(pns$impact_resource)))),
          checkboxGroupInput("industry", "Project Industry", 
                      choices = sort(as.character(unique(pns$project_industry))))
        ), 
      #create a main panel
        mainPanel(
          #in the main panel put a new row, width 12, that will contain the leaflet map
        fluidRow(
          column(12,
            leafletOutput("pn_map")
          )
        ),
        #in the main panel put another new row, width 12, that will contain the output table
        fluidRow(
          column(12,
              DT::dataTableOutput("pnTable")
              )
        )
      ) #close main panel
      ) #close sidebar layout framework
    ), #close first tab
    
    #create second tab, much like the first
    tabPanel("Pending Banks",
        sidebarLayout(
          sidebarPanel(
            dateRangeInput("bankdate", "Date Posted", start = "2018-01-01"),
            selectInput("status", "Bank Status", 
                        choices = sort(as.character(unique(banks$timing))), multiple = TRUE),
            selectInput("bankagency", "USACE District or State Agency",
                        choices = sort(as.character(unique(banks$district))), multiple = TRUE),
            selectInput("bankservice_area", "Service Area", 
                        choices = sort(as.character(unique(banks$service_area))), multiple = TRUE), 
            selectInput("bankstate", "State", choices = sort(as.character(unique(banks$state))), multiple = TRUE),
          ), 
        mainPanel(
          fluidRow(
            column(12,
                   leafletOutput("bank_map")
                   )
          ),
          fluidRow(
            column(12,
                   DT::dataTableOutput("bank_table")
                   )
          )
        )
       )
      ), 
    
    #create third tab, use preset HTML tag functions to format printed text
    tabPanel("Info",
             #header style 4
            h4("Welcome to the RES Public Notice Tracker"),
            #header row
            hr(),
            #paragraph
            p("RES Analysts track mitigation public notices from across USACE Districts and State agencies on a regular basis."),
            #blank row
            br(),
            p("This tracker has one tab for impact public notices and a second tab for banks on public notice."),
            br(),
            p("You can use the filters to narrow down data.The table below the map will update to the filters you apply.
              In the table, you can also view or hide columns, sort columns, and download the filtered data as an Excel or CSV file."),
            br(),
            p("This tracker was developed by Jacqueline Clarke (jclarke@res.us) and Matti Baron (mbaron@res.us) using Shiny. 
              Please reach out to us for questions on the tracker or additional data needs.")
      ) #close tab
      ) #close tabset panel
  ) %>% shinyjs::hidden(),#close login div
) #close ui
  

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  shiny::observe({
    shiny::req(credentials()$user_auth)
    shinyjs::show(id = "show-page")
  })
  
 # call login module supplying data frame,
  #user and password cols and reactive trigger
   credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password
  )
  
  # call the logout module with reactive trigger to hide/show
  #logout_init <- shinyauthr::logoutServer(
  # id = "logout",
  #  active = reactive(credentials()$user_auth)
  #)
  
  # a reactive expression that returns the pns that meet the input criteria
  filtered_pns <- reactive({
    sel_agency <- if(is.null(input$agency)) unique(pns$agency) else input$agency
    sel_state <-if(is.null(input$state)) unique(pns$state) else input$state
    sel_service_area <- if(is.null(input$service_area)) unique(pns$service_area) else input$service_area
    sel_resource <- if(is.null(input$resource)) unique(pns$impact_resource) else input$resource
    sel_industry <- if(is.null(input$industry)) unique(pns$project_industry) else input$industry
    
    pns %>%
      filter((date_posted >= input$date[1] & date_posted <= input$date[2]),
             agency %in% sel_agency, state %in% sel_state, service_area %in% sel_service_area, 
             impact_resource %in% sel_resource, project_industry %in% sel_industry)
  })
  
  output$pn_map <- renderLeaflet({
    leaflet(data = filtered_pns()) %>%
      addProviderTiles(providers$OpenStreetMap) %>% 
      addCircleMarkers(lng = ~jittered_lng, lat = ~jittered_lat,
                       radius = 5, 
                       stroke = TRUE, 
                       fillOpacity = 0.5, 
                       color = ~resource_colors(impact_resource),
                       popup = ~paste0(
                         "<strong>Date Posted:</strong>", date_posted, "<br>",
                         "<strong>Watershed:</strong>", watershed_name, "<br>",
                         "<strong>Service Area:</strong>", service_area, "<br>",
                         "<strong>Permit Number:</strong>", permit_number, "<br>",
                         "<strong>Permittee:</strong>", permittee, "<br>",
                         "<strong>Project Industry:</strong>", project_industry, "<br>",
                         "<strong>Impact Type:</strong>", impact_resource, "<br>",
                         "<strong>Impact Resource:</strong>", impact_resource_classification, "<br>",
                         "<strong>Impact Amount (acres):</strong>", impact_amount),
                       label = ~paste(permit_number, ",", impact_resource, ",", impact_amount)) %>%
      leaflet::addMeasure() %>%
      addLegend(
        pal = resource_colors,
        values = ~impact_resource,
        title = "Impact Resource",
        opacity = 1,
        position = "bottomright"
      )
    })
  
  output$pnTable = DT::renderDataTable({
    DT::datatable(filtered_pns(), 
                  extensions = c(
                    "Buttons",  # add download buttons, etc
                    "Scroller"  # for scrolling down the rows rather than pagination
                  ),
                  rownames = FALSE,  # remove rownames
                  style = "bootstrap",
                  class = "compact",
                  width = "100%",
                  options = list(
                    dom = "Blrtip",  # specify content (search box, etc)
                    deferRender = TRUE,
                    scrollY = 300,
                    scrollX = TRUE,
                    scroller = TRUE,
                    columnDefs = list(
                      list(
                        visible = FALSE,
                        targets = c(0,1,9,10,13,14,21:37,39,40)
                      )
                    ), 
                    buttons = list(
                      I("colvis"),  # turn columns on and off
                      "csv",  # download as .csv
                      "excel"  # download as .xlsx
                    )
                  ),
                  colnames = c(
                    "Permit Number" = "permit_number",
                    "Agency" = "agency",
                    "State" = "state",
                    "Date Posted" = "date_posted",
                    "Analyst initial" = "initial",
                    "HUC8" = "huc"
                  )
    )
  })
  
  filtered_banks <- reactive({
    bsel_agency <- if(is.null(input$bankagency)) unique(banks$district) else input$bankagency
    bsel_status <- if(is.null(input$status)) unique(banks$timing) else input$status
    bsel_state <-if(is.null(input$bankstate)) unique(banks$state) else input$bankstate
    bsel_service_area <- if(is.null(input$bankservice_area)) unique(banks$service_area) else input$bankservice_area
    
    
    banks %>%
      filter((pn_posted >= input$bankdate[1] & pn_posted <= input$bankdate[2]),
             timing %in% bsel_status, district %in% bsel_agency, state %in% bsel_state, service_area %in% bsel_service_area
             )
  })
  
  output$bank_map <- renderLeaflet({
    leaflet(data = filtered_banks()) %>%
      leaflet::addProviderTiles(providers$OpenStreetMap) %>% 
      addCircleMarkers(
        radius = 5, 
        stroke = TRUE, 
        color = ~timing_colors(timing),
        fillOpacity = 0.5,
        popup = ~paste0(
          "<strong>Bank Name:</strong>", bank_name, "<br>",
          "<strong>Date Posted:</strong>", pn_posted, "<br>",
          "<strong>Watershed:</strong>", watershed, "<br>",
          "<strong>Service Area:</strong>", service_area, "<br>",
          "<strong>Permit Number:</strong>", permit_number, "<br>",
          "<strong>Bank Sponsor:</strong>", bank_sponsor, "<br>",
          "<strong>Consultant:</strong>", consultant, "<br>",
          "<strong>Habitat/Credit Type:</strong>", habitat_credit_type, "<br>",
          "<strong>Acres/LF:</strong>", acres_lf, "<br>",
          "<strong>Primary Service Area (acres):</strong>", primary_service_area
        ), # end popup()
        label = ~bank_name,
      )%>%   # end addAwesomeMarkers()
      leaflet::addMeasure() %>%
      addLegend(
      pal = timing_colors,
      values = ~timing,
      title = "Bank Status",
      opacity = 1,
      position = "bottomright"
    )
    
  })
  
  output$bank_table = DT::renderDataTable({
    DT::datatable(filtered_banks(), 
                  extensions = c(
                    "Buttons",  # add download buttons, etc
                    "Scroller"  # for scrolling down the rows rather than pagination
                  ),
                  rownames = FALSE,  # remove rownames
                  style = "bootstrap",
                  class = "compact",
                  width = "100%",
                  options = list(
                    dom = "Blrtip",  # specify content (search box, etc)
                    deferRender = TRUE,
                    scrollY = 300,
                    scrollX = TRUE,
                    scroller = TRUE,
                    columnDefs = list(
                      list(
                        visible = FALSE,
                        targets = c(0,1,2)
                      )
                    ), 
                    buttons = list(
                      I("colvis"),  # turn columns on and off
                      "csv",  # download as .csv
                      "excel"  # download as .xlsx
                    )
                  ),colnames = c(
                    "Permit number" = "permit_number",
                    "Date posted" = "pn_posted"
                  )
    )
  })  
  
  #text output for info tab
  output$info <- renderText("")
  
}

# Run the application 
shinyApp(ui = ui, server = server)
