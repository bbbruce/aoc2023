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
    map(~ {
        if(!is.na(.["J"]))
            if(names(.)[1] == "J") {
                if(.[1] == 5) {
                    names(.)[1] <- "A"
                } else {
                    .[2] <- .[1] + .[2]
                }
            } else {
                .[1] <- .[1] + .["J"]
            }
            .["J"] <- 0
        .}) %>%
    map(~ keep(., . > 0)) %>%
    map_chr(~ sprintf("%-5s", paste(., collapse = ""))) ->
    d$handrank

d$hand %>%
    map(~ chartr("AKQJT98765432", "EDC1A98765432", .)) ->
    d$lexohand

d %>%
    mutate(handsort = paste0(handrank, lexohand)) %>%
    arrange(desc(handsort)) %>%
    mutate(rank = rev(1:NROW(.)),
        winnings = rank * as.numeric(bid)) ->
    d
    
d %>%
        pull(winnings) %>%
        sum()

# 249089358 low