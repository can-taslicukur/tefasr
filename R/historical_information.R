#' Get historical information for investing funds
#'
#' @param information_type Type of the information. Must be one of the following:
#' * `"BindHistoryInfo"` : General information about a fund such as price, total value, number of traders
#' * `"BindHistoryAllocation"` : Breakdown of a portfolio in percentage
#' @param fund_type Type of the fund. Must be one of the following:
#' * `"YAT"` : Securities Mutual Funds
#' * `"EMK"` : Pension Funds
#' * `"BYF"` : Exchange Traded Funds
#' @param start_date Start date for the historical data. Must be a Date object.
#' @param end_date End date for the historical data. Must be a Date object.
#' @param fund_code (optional) Abbreviation for the desired fund. If `NULL`, all of the funds for the given time period are returned.
#' Complete list of funds traded in TEFAS can be found in [takasbank website](https://www.takasbank.com.tr/en/resources/tefas-mutual-funds)
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' historical_information(
#'   information_type = "BindHistoryInfo",
#'   fund_type = "YAT",
#'   start_date = as.Date("2021-10-01"),
#'   end_date = as.Date("2021-10-01")
#' )
historical_information <- function(information_type = c("BindHistoryInfo", "BindHistoryAllocation"),
                                   fund_type = c("YAT", "EMK", "BYF"),
                                   start_date,
                                   end_date,
                                   fund_code = NULL) {
  information_type <- match.arg(information_type)
  fund_type <- match.arg(fund_type)

  if (!inherits(start_date, "Date") | !inherits(end_date, "Date")) {
    stop("start_date and end_date must be Date objects.")
  }

  if (start_date > end_date) {
    stop("start_date must be less than or equal to end_date!")
  }

  domain <- "https://www.tefas.gov.tr/api/DB/"
  endpoint <- paste0(domain, information_type)

  # tefas doesnt allow more than 90 days between start and and date so we can get data by chunks to bypass this
  date_chunks <- unique(c(seq(start_date, end_date, 90), end_date))
  if (length(date_chunks) == 1) {
    date_chunks <- rep(date_chunks, 2)
  }

  data_chunks <- list()

  for (i in 1:(length(date_chunks) - 1)) {
    request_form_data <- list(
      fontip = fund_type,
      bastarih = as.character(format(date_chunks[i], "%d.%m.%Y")),
      bittarih = as.character(format(date_chunks[i + 1], "%d.%m.%Y")),
      fonkod = fund_code
    )

    request <- httr::POST(
      url = endpoint,
      httr::user_agent(agent = "https://github.com/can-taslicukur/tefasr"),
      httr::accept("application/json, text/javascript, */*; q=0.01"),
      httr::content_type("application/x-www-form-urlencoded; charset=UTF-8"),
      body = request_form_data,
      encode = "form"
    )

    request_content <- httr::content(request)

    request_data <- lapply(
      request_content$data,
      function(x) {
        as.list(
          sapply(x, function(y) {
            ifelse(is.null(y), NA, y)
          })
        )
      }
    )

    request_data <- do.call(rbind, lapply(request_data, as.data.frame))

    data_chunks <- append(data_chunks, list(request_data))
  }

  data <- do.call(rbind, data_chunks)

  return(data)
}
