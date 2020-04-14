#' Function fars_read
#'
#' This function gets as input a file name (of csv format), and
#' and reads it to a data frame object, which is what the function returns.
#'
#' @importFrom dplyr tbl_df
#' @importFrom readr read_csv
#'
#' @param filename A character string with the file name to read.
#'      Either full path should be given, or path relative to local working directory.
#'      If file name does not exist - an Error will be returned.
#'
#' @return This function returns a data frame object of the filename that was read.
#'
#' @examples
#' fars_read("accident_2015.csv")
#' fars_read("C:\\coursera\\R_packages\\data\\accident_2015.csv")
#'
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Function make_filename
#'
#' This function gets as input a year number (in any format),
#' and outputs the relevant data file name for that year
#'
#' @param year Any format of 4 digits year number.
#'
#' @return This function returns a string of the relevant file name fo the specific year.
#'
#' @examples
#' make_filename("2015")
#' fars_read(2014)
#'
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Function fars_read_years
#'
#' This function gets a list of years, and returns a list of tables for each year
#' in the input argument. the table for each year include the year and the months from the
#' relevant file of that year.
#' The function also checks for valid data files for those years.
#' A valid data file is a file which exists for this specific year.
#'
#' @importFrom dplyr mutate select
#'
#' @param years A vector of the years to be read.
#'
#' @return This function returns a list of tables - one table per year.
#'          Each table has 2 columns: MONTH and year taken from original file of that year.
#'
#' @examples
#' fars_read_years(c("2014","2015"))
#' fars_read_years(c(2015,2013))
#'
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
#' Function fars_summarize_years
#'
#' This function gets as input a list of years, and gives a summary of how many motor crashes
#' there were in each month of each year in USA (taken from relevant files for those years).
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @param years A vector of the years to be read.
#'
#' @return This function returns data frame with a "MONTH" column,
#' and one columns per each year in the "years" input argument.
#' The values under each year column - is the number of motor crashes
#' that happeded in that year - for this specific month.
#'
#' @examples
#' fars_summarize_years(c("2014","2015"))
#' fars_summarize_years(c(2015,2013))
#'
#' @export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Function fars_map_state
#'
#' This function gets as input a state number (in USA) and a year.
#' Its output is a map of this state with points located at the geographical
#' location of motor crashes that happened in that state in that year.
#' Note that invalid year (where no file exists for that year), or invalid state number
#' will give an appropriate error.
#'
#' @importFrom maps map
#' @importFrom graphics points
#' @importFrom dplyr filter
#'
#' @param state.num The state number in USA.
#' @param year The year number
#'
#' @return This function returns a map of the specific state with all motor crash locations
#'         That happened in the specific year in that state. All data is taken from relevant file.
#'
#' @examples
#' fars_map_state(1,2015)
#' fars_map_state(18,"2013")
#'
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
