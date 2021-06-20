#########################
# load shiny packages
#########################
# required packages:
# c("shiny", "bsplus", "dplyr", "tibble", "data.table", "DT",
# "shinycssloaders", "stringr", "fst")
library(shiny)
library(bsplus) # for pop-up tooltip
# usually i would load 'tidyverse'
# but since i am running this on a virtual server
# just load the necessary packages: dplyr and tibble
library(dplyr)
library(tibble)
# library(DT) # this package is required but is not loaded by convention (?)
library(data.table)
# library(devtools)
# devtools::install_github("MatthewSmith430/CompaniesHouse")
# library(CompaniesHouse)
# no longer need this package as I've manually
# sourced the two functions I need
# load electoral commission donations data
# (originally I tried to scrape results as needed
# using the search box and RSelenium. However, unclear
# if this can be implemented via Shiny...)
library(shinycssloaders)
library(stringr)
library(fst)

donations <- read.fst("data/donations_data.fst") %>% data.frame()

mp_interests <- read.fst("data/mp_register_combined.fst") %>%
  data.frame() %>%
  select(c("Party", "Description", "URL_end"))

# load R script with a slightly amended version of the
# director search function (that returns NULL with 0 results)
source("CompaniesHouse_mod.R")

#########################
# define layout
#########################

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Crony Connect (BETA)"),
    br(),

    sidebarLayout(

        sidebarPanel(
        HTML("<p>Crony Connect allows you to identify politically-connected individuals, using data from <a target='_blank' href='https://find-and-update.company-information.service.gov.uk/'>Companies House</a>, the <a target='_blank' href='http://search.electoralcommission.org.uk/?currentPage=1&rows=10&sort=AcceptedDate&order=desc&tab=1&et=pp&et=ppm&et=tp&et=perpar&et=rd&isIrishSourceYes=true&isIrishSourceNo=true&prePoll=false&postPoll=true&register=gb&register=ni&register=none&optCols=Register&optCols=CampaigningName&optCols=AccountingUnitsAsCentralParty&optCols=IsSponsorship&optCols=IsIrishSource&optCols=RegulatedDoneeType&optCols=CompanyRegistrationNumber&optCols=Postcode&optCols=NatureOfDonation&optCols=PurposeOfVisit&optCols=DonationAction&optCols=ReportedDate&optCols=IsReportedPrePoll&optCols=ReportingPeriodName&optCols=IsBequest&optCols=IsAggregation'>Electoral Commission</a> and the <a target='_blank' href='https://www.parliament.uk/mps-lords-and-offices/standards-and-financial-interests/parliamentary-commissioner-for-standards/registers-of-interests/register-of-members-financial-interests/'>MP's Register of Financial Interests</a>.</p>"),
        HTML("<p>How does it work? When you enter the name of an individual, <b>Crony Connect</b> looks for any companies linked to that individual in the Companies House database. Then it searches for the individual <em>and any of their associated companies</em> in the donations and financial interests databases.</p>"),
        p("We entered some info already — click 'Search' to see the results!"),
            textInput("ind_name", "Name of individual", value="Andrew Feldman") %>%
          shinyInput_label_embed(
              icon("info") %>%
                bs_embed_tooltip(title = "N.B.: Individuals may be listed under various names (e.g. 'John / Johnathan')")
            ),
        selectInput("year", "Year of birth", c("Unknown" = NA, (2011:1901)), selected=1966) %>%
          shinyInput_label_embed(
            icon("info") %>%
              bs_embed_tooltip(title = "If you don't know their DOB, set to 'Unknown' to see all matches.")
          ),
        selectInput("month", "Month of birth", c("Unknown" = NA,
                                         "January" = 1,
                                         "February" = 2,
                                         "March" = 3,
                                         "April" = 4,
                                         "May" = 5,
                                         "June" = 6,
                                         "July" = 7,
                                         "August" = 8,
                                         "September" = 9,
                                         "October" = 10,
                                         "November" = 11,
                                         "December" = 12), selected=2),
            actionButton("button", "Search"),
        br(),
        br(),
        HTML("<em><p>Thanks to <a target='_blank' href='https://twitter.com/SmithMatt90'>Matthew Smith</a> for creating an R wrapper for the Companies House API, and to <a target='_blank' href='https://twitter.com/MPInterests'>Members Interests</a> for creating an API for the register.</em>")
        ),

        mainPanel(
            tabsetPanel(type = "tabs",

            # tab 1: Companies House - ID
            tabPanel("ID",
              h4(textOutput("tab1_text")),
              br(),
              DT::dataTableOutput("ind_name_results") %>%
                shinycssloaders::withSpinner()
              ),

            # tab 2: Companies House - Companies
            tabPanel("Companies",
              h4(textOutput("tab2_text")),
              br(),
              DT::dataTableOutput("ind_company_results") %>%
                shinycssloaders::withSpinner()
              ),

            # tab 3: Electoral Commission - Donations
            tabPanel("Donations",
              h4(textOutput("tab3_text")),
              br(),
              DT::dataTableOutput("donations_results") %>%
              shinycssloaders::withSpinner()
              ),

            # tab 4: Members Interests
            tabPanel("MP Interests",
              h4(textOutput("tab4_text")),
              br(),
              DT::dataTableOutput("mp_interests") %>%
                shinycssloaders::withSpinner()
            )
            )
        )

        ),

    # activate tooltips, popovers, and MathJax
    use_bs_tooltip(),
    use_bs_popover(),
    withMathJax()
)

#########################
# server logic
#########################

server <- function(input, output) {

    # save Companies House API Key as "mkey"
    # N.B. it is not good practice to paste your
    # API keys in public code! :0
    # But I can't figure out how to make the repo public except
    # for this one file...
    # Anyway, if you want to run your own searches, get your
    # own API key (for free!) by creating an account
    # and registering an "application" here:
    # https://developer-specs.company-information.service.gov.uk/guides/authorisation
   mkey <- "2o_3Jar_scUk6Uxefp-ArOuNymv5OHwbEEyK5jRC"


    # run code if button is clicked
    results <- eventReactive(input$button, {

      if (is.null(DirectorSearch_limit_mod(input$ind_name, mkey))){
        return(NULL)
      }

      else {

      if (input$year=="NA" & input$month=="NA"){
        # if both missing, no filter
        temp <- DirectorSearch_limit_mod(input$ind_name, mkey)
      } else if (input$year=="NA" & input$month!="NA"){
        # year missing
        temp <- DirectorSearch_limit_mod(input$ind_name, mkey) %>%
          filter(month.of.birth==as.numeric(input$month))

      } else if (input$year!="NA" & input$month=="NA"){
        # month missing
        temp <- DirectorSearch_limit_mod(input$ind_name, mkey) %>%
          filter(year.of.birth==as.numeric(input$year))

      } else {
        temp <- DirectorSearch_limit_mod(input$ind_name, mkey) %>%
          filter((month.of.birth==as.numeric(input$month) &
                   year.of.birth==as.numeric(input$year)))
      }
        if (nrow(temp)==0){
          return(NULL)
        }
        else{

      temp %>% mutate(address.snippet = addess.snippet,
                      temp.link = paste0("https://find-and-update.company-information.service.gov.uk/officers/",
                                         director.id, "/appointments"),
                      director.name = paste0("<a target='_blank' href='", temp.link,"'>", person.name, "</a>")) %>%
        select(director.name, month.of.birth, year.of.birth, address.snippet, director.id)
        }
      }

    })


    # render results as table
    output$ind_name_results <- DT::renderDataTable({
      DT::datatable(
        results(),
        escape=FALSE)
        })

    count1 <- reactive({
      if (is.null(results())){
        0
      }
      else {
      nrow(results())
        }
      })

    output$tab1_text <- renderText({
      if( lengths(strsplit(input$ind_name, "\\W+"))<2 ){
        paste("Please enter a full name ('FIRSTNAME LASTNAME') to search the Companies House database")
      }
      else {

        paste("There are", count1(), "individual(s) with this name/DOB in Companies House (max. 20 results).")
      }
    })

    results2 <- reactive({

      if (is.null(results())){
        NULL
      }
      else{

        director_ids <-  results()$director.id
        temp <- list()
        for (id in director_ids){
            temp[[id]] <- indiv_ExtractDirectorsData_mod(id, mkey)
        }

        do.call("rbind", temp) %>% tibble::remove_rownames()
      }
    })

    # render results as table
    output$ind_company_results <- DT::renderDataTable({
      if (is.null(results2())){
        DT::datatable(results2())
      }
      else {
      DT::datatable(
        results2() %>%
            mutate(company.name = comapny.name,
                   temp = paste0("https://find-and-update.company-information.service.gov.uk/company/", company.id),
                   company.name = paste0("<a target='_blank' href='", temp,"'>", company.name, "</a>")) %>%
            select(company.name, directors, start.date, end.date,
                   occupation, role, birth.year, birth.month, company.id),
      escape=FALSE, # don't escape the HTML for the hyperlink
      rownames= FALSE
      )
      }
        })

    count2 <- reactive({
      if (is.null(results2())){
        0
        }
      else {
        nrow(results2())
      }
        })

    output$tab2_text <- renderText({
      if( lengths(strsplit(input$ind_name, "\\W+"))<2 ){
        paste("Please enter a full name ('FIRSTNAME LASTNAME') to search the Companies House database")
      }
      else {

        paste("There are", count2(), "companies linked to this individual(s) in Companies House")
      }
    })


    # clean up company names
    company_names <- reactive({

        temp <- gsub("Limited", "", results2()$comapny.name, ignore.case=T)
        temp <- gsub("\\s*\\([^\\)]+\\)","", temp, ignore.case=T)
        temp <- gsub("llp","", temp, ignore.case=T)
        temp <- gsub("partners","", temp, ignore.case=T)
        temp <- gsub("associates","", temp, ignore.case=T)
        temp <- gsub("holdings","", temp, ignore.case=T)
        temp <- gsub("group","", temp, ignore.case=T)
        temp <- gsub("foundation","", temp, ignore.case=T)
        temp <- gsub("the","", temp, ignore.case=T)
        temp <- gsub("of","", temp, ignore.case=T)
        temp <- gsub("and","", temp, ignore.case=T)
        temp <- gsub("&","", temp, ignore.case=T)
        temp <- gsub("ltd","", temp, ignore.case=T)

    })

    results3 <- reactive({
      search_terms <- c(input$ind_name, company_names())
      search_terms_regex <- paste(search_terms, collapse="|")
      temp <- donations[stringr::str_detect(toupper(donations$DonorName), toupper(search_terms_regex)),] %>%
        select(RegulatedEntityName, DonorName,
               Value, DonationType, AcceptedDate, ReportedDate)
      if (nrow(temp)==0){
        return(temp)
      }
      else {
      matches <- stringr::str_extract_all(tolower(temp$DonorName), tolower(search_terms_regex))

      for (i in 1:length(matches)){
        for (j in 1:length(matches[[i]])){
          temp$DonorName[i] <- stringr::str_replace_all(toupper(temp$DonorName[i]),
                                                          toupper(matches[[i]][j]),
                                                          paste0("<mark><b>", toupper(matches[[i]][j]), "</b></mark>"))
        }
      }

      return(temp)
      }
    })



     # render results as table
      output$donations_results <- DT::renderDataTable({
          DT::datatable(results3(), rownames= FALSE, escape=FALSE)
      })

      count3 <- reactive({

        if (is.null(results3())){
          0
        } else {
        nrow(results3())
        }
          })

      total <- reactive({

        if (is.null(results3())){
          0
        } else {
          temp <- gsub("£", "", results3()$Value)
          temp <- gsub(",", "", temp)
          temp <- as.numeric(temp)
          paste0("£", (prettyNum(sum(temp), big.mark=",")))
        }

      })

      output$tab3_text <- renderText({
          paste("There are", count3(), "political donation records linked to this individual(s) and their associated companies, worth a total of ", total())
      })


      ##########################
      # TAB: MEMBERS INTERESTS #
      ##########################

      # define function to look up company names in donations database


      # apply
      results4 <- reactive({
        search_terms <- c(input$ind_name, company_names())
        search_terms_regex <- paste(search_terms, collapse="|")

        temp <- mp_interests[stringr::str_detect(toupper(mp_interests$Description), toupper(search_terms_regex)),]
        if (nrow(temp)==0){
          return(temp)
        }
        else {

        matches <- stringr::str_extract_all(tolower(temp$Description), tolower(search_terms_regex))

        for (i in 1:length(matches)){
          for (j in 1:length(matches[[i]])){
            temp$Description[i] <- stringr::str_replace_all(toupper(temp$Description[i]),
                                                            toupper(matches[[i]][j]),
                                                            paste0("<mark><b>", toupper(matches[[i]][j]), "</mark></b>"))
          }
        }

        temp$Register_date <- substr(temp$URL_end, 1, 6)
        temp$Name <- str_match(temp$URL_end, "\\/\\s*(.*?)\\s*\\.")[,2]
        temp$Name1 <- str_split_fixed(temp$Name, "_", n=2)[,2] %>% str_to_title()
        temp$Name2 <- str_split_fixed(temp$Name, "_", n=2)[,1] %>% str_to_title()
        temp$MP <- paste0(temp$Name1, " ", temp$Name2)
        temp$URL <- paste0("https://www.publications.parliament.uk/pa/cm/cmregmem/", temp$URL_end)
        temp$URL <- paste0("<a target='_blank' href=", temp$URL, ">Link</a>")
        temp <- temp %>% select(MP, Party, URL, Description)
        rownames(temp) <- NULL
        return(temp)
        }
      })


      # render results as table
      output$mp_interests <- DT::renderDataTable({
        DT::datatable(
          results4(),
          escape = FALSE, rownames= FALSE)
      })

      count4 <- reactive({

        if (is.null(results4())){
          0
        } else {
          nrow(results4())
        }
      })

      output$tab4_text <- renderText({
        paste("There are", count4(), "entries in the Register of Members Financial Interests matching these search terms")
      })

}


# Run the application
shinyApp(ui = ui, server = server)

