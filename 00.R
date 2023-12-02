library(httr)
library(tidyverse)

# get cookie from dev tools in browser -> storage
get_aoc_input <- function(year, day) {
  fn <- sprintf("%02d/%02d.txt", day, day)
  if(file.exists(fn)) {
    out <- read_lines(fn)
  } else {
    url <- sprintf("https://adventofcode.com/%s/day/%s/input", year, day)
    sc <- "53616c7465645f5fa3f23f6d0c662b375fd4daf248a23016b843d86435f2b7451339b3e91f001ba3b9af12c0909f25bc157196a17d5466cd52702079e3b634d8"
    GET(url, set_cookies(session = sc)) %>% 
      content %>%
      strsplit("\n") %>%
      pluck(1) -> 
      out
    write_lines(out, fn)
  }
  out
}

get_aoc_test_input <- function(year, day) {
  f <- file(sprintf("%02d/%02d_test.txt", day, day))
  df <- readLines(f)
  close(f)
  df
}
  
input2df <- function(input) {
  input %>% 
    strsplit(" ") %>%
    do.call(rbind, .) %>%
    as.data.frame()
}

input2array <- function(input, FUN = as.numeric) {
  input %>%
    strsplit("") %>%
    do.call(rbind, .) %>%
    apply(2, FUN)
}

#' Apply function to array element with bounds checking
#' 
#' @param fun the function, should take three parameters: arr, i, j and return modified arr
#' @param arr the array
#' @param i the row index
#' @param j the col index
apply_fx_w_bounds <- function(fun, arr, i, j) {
  if(i <= NROW(arr) && 
     j <= NCOL(arr) &&
     i > 0 &&
     j > 0 &&
     length(arr[i, j]) > 0) {
    arr <- fun(arr, i, j)
  }
  arr
}

#' Apply fun locally
#' 
apply_fx_local <- function(fun, arr, i, j, wdiag = TRUE) {
  rows <- c(i,     i,     i + 1, i - 1)
  cols <- c(j - 1, j + 1, j,     j    )
  
  if(wdiag) {
    rows <- c(rows, c(i + 1, i + 1, i - 1, i - 1))
    cols <- c(cols, c(j - 1, j + 1, j - 1, j + 1))
  }
  reduce2(rows, cols, fun, .init = arr)
}

#' Apply fun locally
#' 
apply_fx_local <- function(fun, arr, i, j, wdiag = TRUE) {
  rows <- c(i,     i,     i + 1, i - 1)
  cols <- c(j - 1, j + 1, j,     j    )
  
  if(wdiag) {
    rows <- c(rows, c(i + 1, i + 1, i - 1, i - 1))
    cols <- c(cols, c(j - 1, j + 1, j - 1, j + 1))
  }
  reduce2(rows, cols, fun, .init = arr)
}

#' Apply function to array element with bounds checking
#' 
#' @param fun the function, should take three parameters: arr, i, j and return modified arr
#' @param arr the array
#' @param i the row index
#' @param j the col index
read_w_bounds <- function(arr, i, j) {
  if(i <= NROW(arr) && 
     j <= NCOL(arr) &&
     i > 0 &&
     j > 0 &&
     length(arr[i, j]) > 0) {
    arr[i, j]
  } else NA
}

#' Read local values
#' 
read_local <- function(arr, i, j, wdiag = TRUE) {
  rows <- c(i,     i,     i + 1, i - 1)
  cols <- c(j - 1, j + 1, j,     j    )
  
  if(wdiag) {
    rows <- c(rows, c(i + 1, i + 1, i - 1, i - 1))
    cols <- c(cols, c(j - 1, j + 1, j - 1, j + 1))
  }
  map2(rows, cols, read_w_bounds, arr = arr)
}


read_local_df <- function(arr, i, j, wdiag = TRUE) {
  rows <- c(i,     i,     i + 1, i - 1)
  cols <- c(j - 1, j + 1, j,     j    )
  
  if(wdiag) {
    rows <- c(rows, c(i + 1, i + 1, i - 1, i - 1))
    cols <- c(cols, c(j - 1, j + 1, j - 1, j + 1))
  }
  data.frame(
    r = rows,
    c = cols,
    v = map2_dbl(rows, cols, read_w_bounds, arr = arr)
  )
}
