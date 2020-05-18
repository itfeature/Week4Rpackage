#  COURSERA: BUILDING R PACKAGES
#  Peer-graded Assignment (Week2)

#' Read file with FARS data
#'
#' \code{fars_read()} function reads data from a \code{*.csv} file, stored on disk,
#' from the \strong{US National Highway Traffic Safety Administration's}
#' \emph{Fatality Analysis Reporting System} (FARS), which is a nationwide census,
#' providing the American public yearly data, regarding fatal injuries suffered in
#' motor vehicle traffic crashes. If the filename does not exist the function will
#' stop and an error will be thrown.
#'
#' @details For more information, see:
#' \itemize{
#'   \item{\url{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}}
#'   \item{\url{https://en.wikipedia.org/wiki/Fatality_Analysis_Reporting_System}}
#' }
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @param filename A character string with the name of the file to read, see
#'   notes.
#'
#' @return A data frame with data readed from the csv file, or an error if the
#'   file does not exists.
#'
#' @examples
#' \dontrun{fars_read("accident_2014.csv.bz2")}
#' library(readr)
#' \dontrun{fars_read("data/accident_2014.csv.bz2")}
#'
#' @note To generate file name use: \code{\link{make_filename}}
#' @seealso \link{make_filename}
#' @export

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Creates a filename based on year input
#'
#'\code{make_filename} will create a character vector filename with an assocaited year input
#'
#' \code{make_filename()} takes a year input and creates a filename with this year.
#' If the year is does not pass \code{as.integer} it will have a value of NA, and the function
#' will throw an error after being passed to sprintf.
#'
#' @param year The year to be in the name of the file.
#'
#' @note This package does not depends on any extensions
#'
#' @return A character vector filename
#'
#' @examples
#' \dontrun{
#'   make_filename(2013)
#' }
#'
#' @export

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read in multiple files by year
#'
#'\code{fars_read_years()} takes a vector of years it iterates over them to
#'create filenames to read in and return with only the MONTH and year columns
#'selected The function will create a filename depending on the year input, if the
#'file does not exist an error will be thrown. If it does exist, it will attempt to
#'read them in, mutate a new column with the year and then select the columns MONTH and year.
#'
#' @param years A vector of years to read in
#'
#' @importFrom dplyr mutate select %>%
#'
#' @note this function depends on dplyr mutate and select functions
#'
#' @return returns a list of dataframes with columns MONTH and year, NULL when an error is encountered
#'
#' @examples
#' \dontrun{
#'   fars_read_years(c(2013, 2014))
#' }
#'
#'
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

#' Read in multiple files by year
#'
#'\code{fars_read_years} takes a vector of years it iterates over them to
#'create filenames to read in and return with only the MONTH and year columns
#'selected. The function will create a filename depending on the year input, if the
#'file does not exist an error will be thrown. If it does exist, it will attempt to
#'read them in, mutate a new column with the year and then select the columns MONTH
#'and year.
#'
#' @param years A vector of years to read in
#'
#' @importFrom dplyr mutate select %>%
#'
#' @note this function depends on dplyr mutate and select functions
#'
#' @return returns a list of dataframes with columns MONTH and year, NULL when an error is encountered
#'
#' @examples
#' \dontrun{
#'   fars_read_years(c(2013, 2014))
#' }
#'
#'
#' @export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' Plots points where accidents for given state.num
#'
#' \code{fars_map_state} plots the state given as an integer
#' argument and plots accidents for a given year
#'
#' The function will take a year and state and attempt to plot accidents for a given
#' state number. If there is no data for a state, the function will stop and throw
#' an error. If there are no accidents a message will be returning indicating this.
#'
#' @param state.num state number to plot accidents for
#' @param year year of data to plot accidents that occured
#'
#' @importFrom maps map
#' @importFrom graphics points
#' @importFrom dplyr filter
#'
#' @note This function depends on map and points functions from the maps and graphics package respectively.
#'
#' @return a data.frame of filtered data, if there are no rows then returns invisible(NULL)
#'
#' @examples
#' \dontrun{
#'   fars_map_state(1, 2013)
#' }
#'
#'
#' @export
#'
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
