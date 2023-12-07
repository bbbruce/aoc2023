source("00.R")

input <- get_aoc_input(2023, 7)

input2df(input) %>%
    set_names("hand", "bid") ->
    d

d

d$hand %>%
    strsplit("") %>%
    map(table) %>%
    map(sort) %>%
    map(rev) %>%
    map_chr(~ sprintf("%-5s", paste(., collapse = ""))) ->
    d$handrank

d$hand %>%
    map(~ chartr("AKQJT98765432", "EDCBA98765432", .)) ->
    d$lexohand

d %>%
    mutate(handsort = paste0(handrank, lexohand)) %>%
    arrange(desc(handsort)) %>%
    mutate(rank = rev(1:NROW(.)),
        winnings = rank * as.numeric(bid)) %>%
        pull(winnings) %>%
        sum()
