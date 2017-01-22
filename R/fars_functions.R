#' @description Function to read FARS file.
#'
#' This function uses readr and dplyr packages.
#' It takes a CSV file as an input filename parameter.
#' If the file does not exist in the working directory, this function stops with an output message that the file does not exist.
#' If the file exists, this function reads it into dataframe object.
#' It suppresses messages and progress bar while reading the file.
#'
#' @import tidyr
#' @import dplyr
#' @import maps
#' @import mapdata


#' @name fars_read
#' @title Function to read fras files
#' @description Function to load a CSV dataset into a dataframe
#' @details Dataset must accomplish the FARS structure
#'
#' @param filename Path to file
#'
#' @return The dataframe generated
#'         Error if file doesn't exist
#'
#'
fars_read <- function(filename) {
        fname <- filename
        if(!file.exists(filename)) {
                stop("file '", filename, "' does not exist")
        }
        data <- suppressMessages({
                readr::read_csv(fname, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' @name make_filename
#' @title Mount the fullpath for a filename
#'
#' @description Generates a file name according the naming criteria of FARS
#'
#' @param year The year of data in format YYYY
#'
#' @return An string with the name generated
#'         Error if year is not numeric
#'
# #' @examples
# #' make_filename(2013)
#'
make_filename <- function(year) {
        year <- as.integer(year)
        name <- sprintf("accident_%d.csv.bz2", year)
}

#' Print "fars_read_years"
#' @name fars_read_years
#' @title Obtains years into file
#'
#' @description Load the data files associated to each year and extract month and year
#'
#' @param years A list or vector with desired years in format YYYY
#'
#' @return A list with an element for each file including MONTH and year
#'         Error if doesn't exist the file associated to year
#'
# #' @examples
# #' fars_read_years(c(2013,2014,2015))
#'
# #' @export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)

                        dplyr::mutate(dat, year = year)  %>%
                                dplyr::select("MONTH", year)
                }, error = function(e) {
                        stop("Invalid year: ", year)
#                        return(NULL)
                })
        })

}
#' Print "fars_summarize_years"
#'
#' @name fars_summarize_years
#' @title Summarize years
#' @description Extract the total amount of accidents by year group by months
#'
#' @param years A list or vector with desired years in format YYYY
#'
#' @return A data frame with the amount of accidents for year by month
#'
# #' @examples
# #' fars_summarize_years(c(2013,2014,2015))
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by("year", "MONTH") %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread("year", n)
}

#' Print "fars_map_state"
#'
#' @name fars_map_state
#' @title Plot te accidents
#' @description Plot the accidents occured into an state along one year
#'
#' @param state.num The code of the state
#' @param year      The desired year in YYYY format
#'
#' @return A plot with the accidents occurred by latitude and longitude
#'
# #' @examples
# #' fars_map_state(1,2013)
#'
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, "STATE" == state.num)
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
