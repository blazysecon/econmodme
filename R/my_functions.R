if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

#' Add industry and sector columns to Passport industry tree
#'
#' @param df data frame with Euromonitor Passport Tree with the following columns: categoryid, productname, categoryparentid, indentlevel, projectid, islowestlevel
#' @return data frame with the following extra columns: industryid, industry, sectorlev1id, sectorlev1
#'
#' @export
#' @import dplyr
#' @importFrom stats na.omit
#' @importFrom stringr str_replace
#' @importFrom tidyr gather
#'
#' @examples
#' \dontrun{df %>% get_extended_tree()}
get_extended_tree <- function(df) {

  df_tree_pairs <- df %>%
    dplyr::select(.data$categoryid, .data$categoryparentid, .data$projectid) %>%
    na.omit()

  df_tree_names <- df %>%
      select(.data$projectid, .data$categoryid, .data$productname) %>%
      distinct()

  if (any(duplicated(df_tree_pairs %>% dplyr::select(.data$categoryid, .data$projectid)))) stop('[df_tree] should have no duplicates in [categoryid, projectid]')

  # This assumes that tree has no duplicates and no exceptions
  df_tree_wide <- df %>%
      dplyr::filter(.data$indentlevel == 0) %>%
      dplyr::select(prod_lev0 = .data$categoryid) %>%
      left_join(df_tree_pairs %>% dplyr::rename(prod_lev0 = .data$categoryparentid, prod_lev1 = .data$categoryid)) %>%
      left_join(df_tree_pairs %>% dplyr::rename(prod_lev1 = .data$categoryparentid, prod_lev2 = .data$categoryid)) %>%
      left_join(df_tree_pairs %>% dplyr::rename(prod_lev2 = .data$categoryparentid, prod_lev3 = .data$categoryid)) %>%
      left_join(df_tree_pairs %>% dplyr::rename(prod_lev3 = .data$categoryparentid, prod_lev4 = .data$categoryid)) %>%
      left_join(df_tree_pairs %>% dplyr::rename(prod_lev4 = .data$categoryparentid, prod_lev5 = .data$categoryid)) %>%
      left_join(df_tree_pairs %>% dplyr::rename(prod_lev5 = .data$categoryparentid, prod_lev6 = .data$categoryid)) %>%
      left_join(df_tree_pairs %>% dplyr::rename(prod_lev6 = .data$categoryparentid, prod_lev7 = .data$categoryid))
  # Identify lowest level categories
  df_tree_wide$category_ll <- NA
  for (lev in seq_len(6)) {
      df_tree_wide <- df_tree_wide %>%
        dplyr::mutate(category_ll=ifelse(is.na(.data$category_ll) &
                                        !is.na(get(paste0("prod_lev", lev))) &
                                        is.na(get(paste0("prod_lev", lev+1))),
                                    get(paste0("prod_lev", lev)),
                                    .data$category_ll))
  }
  df_tree_wide <- df_tree_wide %>%
      dplyr::mutate(category_ll=ifelse(is.na(.data$category_ll),
                                       .data$prod_lev6, .data$category_ll))

  # Create long category tree
  temp_df_tree_long <- df_tree_wide %>%
      tidyr::gather("level", "categoryid", -.data$category_ll, -.data$projectid) %>%
      mutate(level = as.integer(str_replace(.data$level, "prod_lev", ""))) %>%
      dplyr::select(.data$categoryid, .data$level, .data$category_ll, .data$projectid) %>%
      dplyr::filter(!is.na(.data$categoryid)) %>%
      left_join(df_tree_wide %>% dplyr::select(.data$category_ll, .data$projectid, industryid = .data$prod_lev0)) %>%
      left_join(df_tree_names %>% dplyr::select(industryid = .data$categoryid,
                                                .data$projectid, industry = .data$productname))

  temp_df_tree_long_p2 <- temp_df_tree_long %>%
    dplyr::filter(.data$level>0) %>%
      left_join(df_tree_wide %>% dplyr::select(.data$category_ll, .data$projectid, sectorlev1id = .data$prod_lev1)) %>%
      left_join(df_tree_names %>% dplyr::select(sectorlev1id = .data$categoryid, .data$projectid,
                                                sectorlev1 = .data$productname))

  df_tree_long <- temp_df_tree_long %>%
    dplyr::filter(.data$level==0) %>%
    bind_rows(temp_df_tree_long_p2)

  # Create extended category parent relational file
  df_tree_ext <- df %>%
    inner_join(df_tree_long %>%
                dplyr::select(.data$industry, .data$industryid, .data$sectorlev1id,
                              .data$projectid, .data$categoryid, .data$sectorlev1) %>%
                distinct(), by = c("categoryid", "projectid")) %>%
    dplyr::select(.data$categoryid, .data$projectid, .data$productname,
                  .data$categoryparentid, .data$indentlevel, .data$industryid,
                  .data$industry, .data$sectorlev1id, .data$sectorlev1, .data$islowestlevel)
  # # Add "Not in Passport" and "Not Sure" cases
  # df_tree_ext[nrow(df_tree_ext) + 1,] = list(132, -2L, -2L, "Not in Passport",
  #                                            0L, -2L, "Not in Passport", -2L, "Not in Passport", 1)
  # df_tree_ext[nrow(df_tree_ext) + 1,] = list(132, -1L, -1L, "Not Sure",
  #                                            0L, -1L, "Not Sure", -1L, "Not Sure", 1)

  df_tree_ext

}


#' Add comma formatting to DT::datatable columns
#'
#' @param table DT::datatable
#' @param columns A vector with column names
#'
#' @return Formatted DT::datatable
#'
#' @export
#'
#' @importFrom DT formatCurrency
#' @import dplyr
#'
#' @examples
#' \dontrun{table %>% add_commas("n_sku")}
add_commas <- function(table, columns = c('Raw SKUs')) {
    table %>%
        formatCurrency(
            columns, currency = '', digits = 0,
            interval = 3, mark = ',', before = FALSE
        )
}


#' Create a DT::datatable with export data buttons and nice other defaults
#'
#' @param df Final table in a data frame format
#' @param page_length Number of rows to show at a time
#'
#' @return Formatted DT::datatable
#'
#' @export
#'
#' @importFrom DT datatable
#'
#' @examples
#' \dontrun{df %>% plot_table_btns()}
plot_table_btns <- function(df, page_length = 20) {
    df %>%
        datatable(
            extensions = 'Buttons', options = list(
                pageLength = page_length,
                dom = 'Bfrtip',
                buttons =
                    list('copy', list(
                        extend = 'collection',
                        buttons = c('csv', 'excel', 'pdf'),
                        text = 'Download'
                    ))), filter = 'bottom', rownames = FALSE)
}


#' Convert empty string values "" to NA
#'
#' @param input Text field
#'
#' @return Text field
#'
#' @export
#'
#' @examples
#' load(system.file("extdata", "string_vector.rda", package = "econmodme"))
#' empty_to_na(string_vector)
#'
#' \dontrun{df %>% mutate_if(is.character, funs(ifelse(. == "", NA, .)))}
empty_to_na <-
    function(input)
        ifelse(test = input == "", yes = NA, no = input)


#' Remove leading or trailing whitespace
#'
#' @param x Text field
#'
#' @return Text field
#'
#' @export
#'
#' @examples
#' load(system.file("extdata", "string_vector.rda", package = "econmodme"))
#' trim(string_vector)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)


#' Remove special symbols
#'
#' @param x Text field
#'
#' @return Text field
#'
#' @export
#'
#' @examples
#' load(system.file("extdata", "string_vector.rda", package = "econmodme"))
#' remove_special_symbols(string_vector)
remove_special_symbols <- function (x) {
    x <- iconv(enc2utf8(x),sub="byte")
    x <- gsub("\xe9|<e9>", "e", x)
    x <- gsub("\xe8|<e8>", "e", x)
    x <- gsub("\xf2|\xf3|\xf4|\xf5|\xf6|<f2>|<f3>|<f4>|<f5>|<f6>", "o", x)
    x <- gsub("\xd2|\xd3|\xd4|\xd5|\xd6|<d2>|<d3>|<d4>|<d5>|<d6>", "O", x)
    x <- gsub("\xe1|\xe2|\xe3|\xe4|\xe5|<e1>|<e2>|<e3>|<e4>|<e5>", "a", x)
    x <- gsub("\xE1|\xE2|\xE3|\xE4|\xE5|<E1>|<E2>|<E3>|<E4>|<E5>", "A", x)
    x <- gsub("\xed|<ed>", "i", x)
    x <- gsub("-|,|\\.|/", " ", x)
    x <- gsub("'", "", x)
    x <- gsub("\xfc|\xfa|<fc>|<fa>", "u", x)
    x <- gsub("\x8a|<8a>", "S", x)
    x <- gsub("\x9a|<9a>", "s", x)
    x <- gsub("\x9e|<9e>", "z", x)
    x <- gsub("\x8e|<8e>", "Z", x)
    x <- gsub("\xf1|<f1>", "n", x)
    x <- gsub("<fc>", "u", x)
    x <- gsub("<dc>", "U", x)
    x <- gsub("\\<[Tt]he\\>", "", x)
    trim(gsub("\\s+", " ", x))
}

#' Special sum
#'
#' Add variables which are nonmissing
#'
#' @param x numeric field
#' @param y numeric
#'
#' @return numeric
#'
#' @export
#'
#' @examples
#' x %+na% y
`%+na%` <-
    function(x, y) {
        ifelse(is.na(x), y, ifelse(is.na(y), x, x + y))
    }


#' Special average
#'
#' Take an average of variables which are nonmissing
#'
#' @param x numeric field
#' @param y numeric
#'
#' @return numeric
#'
#' @export
#'
#' @examples
#' x %+ana% y
`%+ana%` <-
    function(x, y) {
        ifelse(is.na(x), y, ifelse(is.na(y), x, (x + y) / 2))
    }


#' Create a nice summary table with number of observations, and other statitics
#'
#' @param df data frame
#' @param detailed should we show additional statistics default is FALSE
#' @param text_len the length of text in min and max calculation, default is 6
#'
#' @return numeric
#'
#' @export
#' @importFrom stats median sd
#'
#' @examples
#' sumstats(df, TRUE, 12)
sumstats = function(df, detailed = FALSE, text_len = 6) {
    obs.k = function(x) { # number of nonmissing observations
        if (is.numeric(x))
            sum(as.integer(!is.na(x)))
        else
            sum(as.integer(!(as.character(x) %in%
                                 c(""," ","NA","NAN",
                                   "NULL","na","nan","NaN") | is.na(x))))
    }
    miss.k = function(x) { # number of missing observations
        if (is.numeric(x))
            sum(as.integer(is.na(x)))
        else
            sum(as.integer((as.character(x) %in%
                                c(""," ","NA","NAN",
                                  "NULL","na","nan","NaN") | is.na(x))))
    }
    uniq.k = function(x) { # number of unique values
        length(unique(x[!is.na(x)]))
    }
    mean.k = function(x) {
        if (is.numeric(x))
            suppressWarnings(pretty_digits(mean(x, na.rm = TRUE)))
        else
            "N*N"
    }
    median.k = function(x) {
        if (is.numeric(x))
            suppressWarnings(pretty_digits(median(x, na.rm = TRUE)))
        else
            "N*N"
    }
    sd.k = function(x) {
        if (is.numeric(x))
            suppressWarnings(pretty_digits(sd(x, na.rm = TRUE)))
        else
            "N*N"
    }
    min.k = function(x, text = text_len) {
        if (is.numeric(x))
            suppressWarnings(pretty_digits(min(x, na.rm = TRUE)))
        else
            suppressWarnings(substr(min(as.character(x), na.rm = TRUE), start=1, stop=text))
    }
    max.k = function(x, text = text_len) {
        if (is.numeric(x))
            suppressWarnings(pretty_digits(max(x, na.rm = TRUE)))
        else
            suppressWarnings(substr(max(as.character(x), na.rm = TRUE), start=1, stop=text))
    }
    # Display convenient number of digits
    pretty_digits <- function(x, # format long numbers as scientific
                              dig1 = 5, dig2 = 3, thresh = 1e6) {
        if (is.na(x) || !is.numeric(x)) {
            x
        } else if (abs(x) > thresh/100 & abs(x) <= thresh) {
            prettyNum(x, digits = dig1+1)
        } else if (abs(x) > 0.1 & abs(x) <= thresh/100) {
            prettyNum(x, digits = dig1)
        } else if (abs(x) > (100 / thresh) & abs(x) <= 0.1) {
            prettyNum(x, digits = 2)
        } else if (abs(x) > (1 / thresh) & abs(x) <= (100 / thresh)) {
            prettyNum(x, digits = 1)
        } else {
            prettyNum(x, digits = dig2, scientific = TRUE)
        }
    }
    if ("data.frame" %in% class(df) && nrow(df)>0) {
        sumtable = cbind(
            sapply(df, obs.k),
            if (detailed) sapply(df, miss.k),
            if (detailed) sapply(df, uniq.k),
            if (detailed) substr(sapply(df, class), start=1, stop=3),
            sapply(df, mean.k),
            sapply(df, median.k),
            sapply(df, sd.k),
            sapply(df, min.k),
            sapply(df, max.k)
        )
        sumtable <- as.data.frame(sumtable)
        if (detailed) {
            names(sumtable) = c("Obs", "Miss", "Uniq.Val", "Type",
                                "Mean", "Median", "Std.Dev", "Min", "Max")
        } else {
            names(sumtable) = c("Obs", "Mean", "Median", "Std.Dev", "Min", "Max")
        }
        return(sumtable)
    } else {
        return(names(df))
    }
}

