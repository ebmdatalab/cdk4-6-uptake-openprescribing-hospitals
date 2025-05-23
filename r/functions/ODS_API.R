# This is a simple function to call the NHS ODS API for an ODS code and return data for an organisation

require(httr)
require(NHSRtools)
require(janitor)

ods_get_trusts_by_icb <- function (ods_code) {

  # This code is derived from the NHSRtools methodology
  
  # limit of number of rows we can get at any time
  LIMIT <- NHSRtools::ods_api_results_limit()
  
  # url is built up of api and the parameters. We need to keep a copy of the
  # initial url to compare to later: the api will fail to run if no parameters
  # are passed
  
  url <- init_url <- paste0(NHSRtools:::ODS_API_ENDPOINT, "organisations?RelTypeId=RE8&TargetOrgId=",ods_code,"&RelStatus=active&Limit=", LIMIT)
  
  
  res <- httr::GET(url)
  if (httr::status_code(res) != 200) {
    stop("unable to load data from API")
  }
  
  # find out how many records there are in total to load
  total_count <- as.numeric(httr::headers(res)[["x-total-count"]])
  
  # convert the results into a tibble
  organisations <- dplyr::bind_rows(httr::content(res)[["Organisations"]])
  
  # if we have more results than we can load in one go
  offset <- 0
  while (total_count > nrow(organisations)) {
    # load the next page of results
    offset <- offset + LIMIT
    res <- httr::GET(paste0(url, "&Offset=", offset))
    
    # again, make sure call was successful
    if (httr::status_code(res) != 200) {
      stop("unable to load data from API")
    }
    
    # update the organisations data to include next page of data
    organisations <- dplyr::bind_rows(
      organisations,
      httr::content(res)[["Organisations"]]
    )
  }
  # return the organisations data
  janitor::clean_names(organisations)

}


ods_get_icb_by_region <- function (ods_code) {
  
  # This code is derived from the NHSRtools methodology
  
  # limit of number of rows we can get at any time
  LIMIT <- NHSRtools::ods_api_results_limit()
  
  # url is built up of api and the parameters. We need to keep a copy of the
  # initial url to compare to later: the api will fail to run if no parameters
  # are passed
  
  url <- init_url <- paste0(NHSRtools:::ODS_API_ENDPOINT, "organisations?RelTypeId=RE2&TargetOrgId=",ods_code,"&RelStatus=active&Limit=", LIMIT)
  
  
  res <- httr::GET(url)
  if (httr::status_code(res) != 200) {
    stop("unable to load data from API")
  }
  
  # find out how many records there are in total to load
  total_count <- as.numeric(httr::headers(res)[["x-total-count"]])
  
  # convert the results into a tibble
  organisations <- dplyr::bind_rows(httr::content(res)[["Organisations"]])
  
  # if we have more results than we can load in one go
  offset <- 0
  while (total_count > nrow(organisations)) {
    # load the next page of results
    offset <- offset + LIMIT
    res <- httr::GET(paste0(url, "&Offset=", offset))
    
    # again, make sure call was successful
    if (httr::status_code(res) != 200) {
      stop("unable to load data from API")
    }
    
    # update the organisations data to include next page of data
    organisations <- dplyr::bind_rows(
      organisations,
      httr::content(res)[["Organisations"]]
    )
  }
  # return the organisations data
  janitor::clean_names(organisations)
  
}


ods_get_icb_by_trust <- function (ods_code) {
  
  # This code is derived from the NHSRtools methodology
  
  # limit of number of rows we can get at any time
  LIMIT <- NHSRtools::ods_api_results_limit()
  
  # url is built up of api and the parameters. We need to keep a copy of the
  # initial url to compare to later: the api will fail to run if no parameters
  # are passed
  
  url <- init_url <- paste0(NHSRtools:::ODS_API_ENDPOINT, "organisations?RelTypeId=RE5&TargetOrgId=",ods_code,"&RelStatus=active&Limit=", LIMIT)
  
  
  res <- httr::GET(url)
  if (httr::status_code(res) != 200) {
    stop("unable to load data from API")
  }
  
  # find out how many records there are in total to load
  total_count <- as.numeric(httr::headers(res)[["x-total-count"]])
  
  # convert the results into a tibble
  organisations <- dplyr::bind_rows(httr::content(res)[["Organisations"]])
  
  # if we have more results than we can load in one go
  offset <- 0
  while (total_count > nrow(organisations)) {
    # load the next page of results
    offset <- offset + LIMIT
    res <- httr::GET(paste0(url, "&Offset=", offset))
    
    # again, make sure call was successful
    if (httr::status_code(res) != 200) {
      stop("unable to load data from API")
    }
    
    # update the organisations data to include next page of data
    organisations <- dplyr::bind_rows(
      organisations,
      httr::content(res)[["Organisations"]]
    )
  }
  # return the organisations data
  janitor::clean_names(organisations)
  
}




ods_get_org_icb <- function (ods_code) {
  
  url <- init_url <- paste0(NHSRtools:::ODS_API_ENDPOINT, "organisations/",ods_code)
  res <- httr::GET(url)
  if (httr::status_code(res) != 200) {
    stop("unable to load data from API")
  }

  # convert the results into a tibble
  relations <- dplyr::bind_rows(httr::content(res)[["Organisation"]][["Rels"]])
  
  relations |>
    filter(Status == "Active") |> 
    filter (id == "RE5") -> active_relations
  
  active_relations$Target$OrgId$extension
}


ods_get_org_successor <- function (ods_code) {
  
  url <- init_url <- paste0(NHSRtools:::ODS_API_ENDPOINT, "organisations/",ods_code)
  res <- httr::GET(url)
  if (httr::status_code(res) != 200) {
    stop("unable to load data from API")
  }
  
  # convert the results into a tibble
  success <- dplyr::bind_rows(httr::content(res)[["Organisation"]][["Succs"]])
  
  success |>
    filter(Type == "Successor") -> success
  
  success$Target$OrgId$extension
}
