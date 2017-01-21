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
#'
#' @description Function to load a CSV dataset into a dataframe
#' @details Dataset must accomplish the FARS structure
#'
#' @param filename Path to file
#'
#' @return The dataframe generated
#'         Error if file doesn't exist
#'
#' @examples
#' fars_read('filename')
#' fars_read('dir1/dir2/filename')
#'
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Print "make_filename"
#'
#' @description Generates a file name according the naming criteria of FARS
#'
#' @param year The year of data in format YYYY
#'
#' @return An string with the name generated
#'         Error if year is not numeric
#'
#' @examples
#' make_filename(2016)
#'
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Print "fars_read_years"
#'
#' @description Load the data files associated to each year and extract month and year
#'
#' @param years A list or vector with desired years in format YYYY
#'
#' @return A list with an element for each file including MONTH and year
#'         Error if doesn't exist the file associated to year
#'
#' @examples
#' fars_read_years(2016)
#' fars_read_years(c(2013,2014,2015))
#'

fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)

                        dplyr::mutate(dat, year = year)  %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}
#' Print "fars_summarize_years"
#'
#' @description Extract the total amount of accidents by year group by months
#'
#' @param years A list or vector with desired years in format YYYY
#'
#' @return A data frame with the amount of accidents for year by month
#'
#' @examples
#' fars_read_years(c(2013,2014,2015))
#' return a data frame like this:
#'    MONTH `2013` `2014` `2015`
#'*  <int>  <int>  <int>  <int>
#'1      1   2230   2168   2368
#'2      2   1952   1893   1968
#'3      3   2356   2245   2385
#'4      4   2300   2308   2430
#'5      5   2532   2596   2847
#'6      6   2692   2583   2765
#'7      7   2660   2696   2998
#'8      8   2899   2800   3016
#'9      9   2741   2618   2865
#'10    10   2768   2831   3019
#'11    11   2615   2714   2724
#'12    12   2457   2604   2781
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Print "fars_map_state"
#'
#' @description Plot the accidents occured into an state along one year
#'
#' @param state.num The code of the state
#' @param year      The desired year in YYYY format
#'
#' @return A plot with the accidents occurred by latitude and longitude
#'
#' @examples
#' fars_map_state(1,2013))
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
