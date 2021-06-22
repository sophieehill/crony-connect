# scripts from Companies House R package by Matthew Smith
# with minor modifications by me (SH)
# https://github.com/MatthewSmith430/CompaniesHouse/tree/master/R

CheckNulls <- function(x) {
  if(is.null(x)==TRUE) {
    return(NA)
  } else {
    return(x)
  }
}

DirectorSearch_limit_mod <- function(director_name,mkey) {
  firmNAME<-gsub(" ", "+",director_name)
  firmNAME<-gsub("&","%26",firmNAME)
  #FIRMurl<-paste0("https://api.companieshouse.gov.uk/search/companies?q=",firmNAME)
  FIRMurl<-paste0("https://api.company-information.service.gov.uk/search/officers?q=",firmNAME)

  firmTEST<-httr::GET(FIRMurl, httr::authenticate(mkey, ""))
  firmTEXT<-httr::content(firmTEST, as="text", encoding="UTF-8")
  JLfirm<-jsonlite::fromJSON(firmTEXT, flatten=TRUE)
  # check if there are 0 results
  if (JLfirm$total_results==0){
    return(NULL)
  }
  else {
  DFfirm<-as.data.frame(JLfirm)
  DFfirm<-unique(DFfirm)
  #suppressWarnings(purrr::map_df(DFfirmL,data.frame))


  DFfirmNAMES<-DFfirm$items.title
  DFfirmADDRESS<-DFfirm$items.address_snippet
  #DFfirmCOUNTRY<-DFfirm$items.address.country
  DFfirmLOCAL<-DFfirm$items.address.locality
  DFmonth<-DFfirm$items.date_of_birth.month
  DFyear<-DFfirm$items.date_of_birth.year

  if (is.null(DFmonth) | is.null(DFyear)){
    return(NULL)
  }

  else{

  LINK1<-DFfirm$items.links.self
  LINK2<-gsub("/officers/","",LINK1)
  LINK3<-gsub("/appointments","",LINK2)

  myDf <- data.frame(
    id.search.term = director_name,
    director.id=LINK3,
    person.name=DFfirmNAMES,
    addess.snippet = DFfirmADDRESS,
    locality= DFfirmLOCAL,
    month.of.birth=DFmonth,
    year.of.birth=DFyear
  )

  return(myDf)
  }
  }
}

#############################################
#############################################
#############################################


indiv_ExtractDirectorsData_mod <- function(director_id,mkey){
  #murl<-paste0("https://api.companieshouse.gov.uk/officers/",director_id,"/appointments")
  murl<-paste0("https://api.company-information.service.gov.uk/officers/",director_id,"/appointments")
  dirlist <- httr::GET(murl, httr::authenticate(mkey, "")) #returns an R list object
  dirtext<-httr::content(dirlist, as="text",
                         encoding="UTF-8")
  dirtextPARSED <- httr::content(dirlist,
                                 as="parsed",
                                 encoding="UTF-8")
  TR<-dirtextPARSED$total_results

  murl2 <- paste0("https://api.company-information.service.gov.uk/officers/",director_id,"/appointments","?items_per_page=",TR)
  dirlist2 <- httr::GET(murl2, httr::authenticate(mkey, "")) #returns an R list object
  dirtext2<-httr::content(dirlist2, as="text",
                          encoding="UTF-8")
  #dirtextPARSED2<-httr::content(dirlist2, as="parsed")
  JLdir2<-jsonlite::fromJSON(dirtext2,flatten=TRUE)
  DFdir<-data.frame(JLdir2)

  DFdirNAMES<-DFdir$items.name
  DFdirSTART<-DFdir$items.appointed_on
  DFdirEND<-DFdir$items.resigned_on
  DFdirROLE<-DFdir$items.officer_role
  DFdirOCCUPATION<-DFdir$items.occupation
  DFdirRESIDENCE<-DFdir$items.country_of_residence
  DFdirPOSTCODE<-DFdir$items.address.postal_code
  DFdirNATIONALITY<-DFdir$items.nationality
  DFdirBIRTH_YEAR<-DFdir$date_of_birth.year
  DFdirBIRTH_MONTH<-DFdir$date_of_birth.month
  DFdirFORENAME<-DFdir$items.name_elements.forename
  DFdirSURNAME<-DFdir$items.name_elements.surname
  DFdirCOMPANYname<-DFdir$items.appointed_to.company_name
  DFdirCOMPANYid<-DFdir$items.appointed_to.company_number
  DFdirAPP<-DFdir$kind
  DFdirdownload<-format(Sys.time(), "%d/%m/%Y  %X")
  DFdirDOWNLOADDATE<-DFdirdownload

  myDf <- data.frame(
    company.id = CheckNulls(DFdirCOMPANYid),
    comapny.name=CheckNulls(DFdirCOMPANYname),
    director.id = CheckNulls(director_id),
    directors = CheckNulls(DFdirNAMES),
    director.forename=CheckNulls(DFdirFORENAME),
    director.surname=CheckNulls(DFdirSURNAME),
    start.date = CheckNulls(DFdirSTART),
    end.date = CheckNulls(DFdirEND),
    occupation = CheckNulls(DFdirOCCUPATION),
    role = CheckNulls(DFdirROLE),
    residence = CheckNulls(DFdirRESIDENCE),
    postcode = CheckNulls(DFdirPOSTCODE),
    nationality=CheckNulls(DFdirNATIONALITY),
    birth.year=CheckNulls(DFdirBIRTH_YEAR),
    birth.month=CheckNulls(DFdirBIRTH_MONTH),
    appointment.kind=CheckNulls(DFdirAPP),
    download.date=CheckNulls(DFdirDOWNLOADDATE)
  )

  return(myDf)
}
