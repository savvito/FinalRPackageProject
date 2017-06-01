#' Read in fars file
#'
#' This is a simple function that uses the `readr` package to read in a csv file
#' and then uses `dplyr` to convert the data from the csv into a data frame.
#' @param filename character vectors, containing file names or paths.  In addiition
#' to csv files, the function will take compressed csv files with the file extensions
#' \code{.gz .bz2 .xz .zip}
#' @return a data frame containing the parsed file data.
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @examples \dontrun{fars_read('myFile.csv')}
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Create filename
#'
#' This function creates a file string for an accident file based on the inputted year
#' @param year a non-negative number that can be coerced into an integer
#' @return a file string reading \code{"accident_[YEAR].csv.bz2"}
#' @examples
#' \dontrun{ make_filename("2009")}
make_filename <- function(year) {
        year <- as.integer(year)
        filename <- sprintf("accident_%d.csv.bz2", year)
        system.file("extdata",filename, package = "rfars")
}

#' Read multiple fars files
#'
#' This function attempts to read in data files given a vector of years.
#' @param years a vector of years in YYYY format corresponding to files to be parsed.
#' @return a data frame containing data from the files that have been parsed.
#'
#' @examples
#' \dontrun{fars_read_years(c(2013, 2014, 2015))}
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

#' Summarize fars data
#'
#' Summarizes fars data by creating a table with years as columns and counts of
#' observations for each month in each row.
#' @param years a vector of years from which to extract data from flat files.
#' @return a list with years as columns and counts of accidents as observations.
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @examples
#' \dontrun{fars_read_years(c(2013, 2014, 2015))}
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Draw a state map with fars points
#'
#' This function creates a state map with points representing accidents
#' overlaid on the plotted map.
#' @param state.num an integer state number
#' @param year an integer year
#' @importFrom graphics points
#' @importFrom maps map
#' @importFrom dplyr filter
#' @examples
#' \dontrun{fars_map_state(1, 2013)}
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
