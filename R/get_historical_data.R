get_historical_data <- function(
  fund_type = c("YAT","EMK","BYF"),
  start_date,
  end_date,
  fund_code = NULL
){

  fund_type <- match.arg(fund_type)

  if (!inherits(start_date,"Date") | !inherits(end_date,"Date")) {
    stop("start_date and end_date must be Date objects.")
  }

  request_form_data <- list(
    fontip = fund_type,
    bastarih = as.character(format(start_date-5,"%d.%m.%Y")),
    bittarih = as.character(format(end_date,"%d.%m.%Y")),
    fonkod = fund_code
  )

  request <- httr::POST(
    url = "https://www.tefas.gov.tr/api/DB/BindHistoryInfo",
    httr::user_agent(agent = "RTefas"),
    httr::accept("application/json, text/javascript, */*; q=0.01"),
    httr::content_type("application/x-www-form-urlencoded; charset=UTF-8"),
    body = request_form_data,
    encode = "form")

  request_content <- content(request)

  request_data <- do.call(rbind,lapply(request_content$data,as.data.frame))
  request_data$TARIH <- as.POSIXct(as.numeric(request_data$TARIH)/1000,origin="1970-01-01")

  return(content(request))
}
