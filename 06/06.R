source("00.R")

input <- get_aoc_input(2023, 6)


ss <- strsplit(input, "[: \t]+") 
map(ss, ~ as.numeric(tail(., -1))) %>%
    as.data.frame() %>%
    set_names(map_chr(ss, 1)) ->
    d

roots <- function(time, dist) {
    polyroot(c(- (dist + 0.5), time, -1)) %>%
        sort() ->
        out
    out[1] <- ceiling(Re(out[1]))
    out[2] <- floor(Re(out[2]))
    out
}

win_ways <- function(time, dist) {
    as.numeric(diff(roots(time, dist))) + 1
}

d %>%
    rowwise() %>%
    mutate(ww = win_ways(Time, Distance)) %>%
    pull(ww) %>%
    prod()
