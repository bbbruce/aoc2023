source("00.R")

input <- get_aoc_input(2023, 9)

d <- strsplit(input, " ")

rdiff <- function(x) {
    x[[length(x) + 1]] <- diff(x[[length(x)]])
    if(all(x[[length(x)]] == 0))
        x
    else
        rdiff(x)
}

map(d, as.numeric) %>%
    map(list) %>%
    map(~ rdiff(.)) %>%
    map(~ map_dbl(., ~ tail(., 1))) %>%
    map_dbl(sum) %>%
    sum()

mydiff <- function(x, y) y - x

map(d, as.numeric) %>%
    map(list) %>%
    map(~ rdiff(.)) %>%
    map(~ map_dbl(., ~ head(., 1))) %>%
    map(rev) %>%
    map_dbl(~ Reduce(mydiff, .)) %>%
    sum()