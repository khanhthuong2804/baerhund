#' baerhund

#' @docType data

#' Create custom filename with certain year#'
#' @param year A string of lenght four which gives the year added to the filename
#' @return This function returns a string with the filename customized with given year
#' @examples
#' \dontrun{
#' make_filename(2015)
#' }
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' fars_read
#' @param filename A character string of the name of the file to be read
#' @return This function returns a tibble of the data file

#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' \dontrun{
#' fars_read("accident_2015.csv.bz2")
#' }
#' @export
fars_read <- function(filename) {
  system.file("extdata", filename, package = "dienfar")
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' fars_read_years
#' @param years A string of length 4 which gives the year(s),in the fars filename with month and year attributes.
#' @return returns a list with the month and year columns from the file
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select
#' @examples
#' \dontrun{
#' fars_read_years(2013:2015)
#' }
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}
#' fars_summarize_years
#' @param years A string of length 4 which gives the year(s),in the fars filename with month and year attributes.
#' @return returns a list of the summarized data for one or more years
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' @examples
#' \dontrun{
#' fars_summarize_years(years = c(2013, 2014,2015))
#' }
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' fars_map_state
#' @param state.num An integer representing each US state
#' @param year A string of length 4 which gives the year
#' @return returns a map of the given state and the datapoints
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' @example
#' \dontrun{
#' fars_map_state(6, 2015)
#' }
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)
  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}


