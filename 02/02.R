source("00.R")

input <- get_aoc_input(2023, 2)

head(input)

game_processor <- function(x, max_red = 12, max_green = 13, max_blue = 14) {
    x <- strsplit(x, ":")
    red <- 0
    green <- 0
    blue <- 0
    game <- as.numeric(sub("Game ", "", x[[1]][1]))
    game
    shows <- strsplit(x[[1]][2], ";")
    shows <- strsplit(shows[[1]], ",")
    shows <- map(shows, ~ map(., ~ strsplit(trimws(.), " ")))
    for(show in shows) {
        for(item in show) {
            item <- item[[1]]
            if (as.numeric(item[1]) > get(item[2])) {
                assign(item[2], as.numeric(item[1]))
            }
        }
    }
    if(red <= max_red && green <= max_green && blue <= max_blue) {
        game
    } else {
       NA
    }
}

sum(map_dbl(input, game_processor), na.rm = TRUE)

game_processor2 <- function(x, max_red = 12, max_green = 13, max_blue = 14) {
    x <- strsplit(x, ":")
    red <- 0
    green <- 0
    blue <- 0
    game <- as.numeric(sub("Game ", "", x[[1]][1]))
    game
    shows <- strsplit(x[[1]][2], ";")
    shows <- strsplit(shows[[1]], ",")
    shows <- map(shows, ~ map(., ~ strsplit(trimws(.), " ")))
    for(show in shows) {
        for(item in show) {
            item <- item[[1]]
            if (as.numeric(item[1]) > get(item[2])) {
                assign(item[2], as.numeric(item[1]))
            }
        }
    }
    red * green * blue
}

sum(map_dbl(input, game_processor2), na.rm = TRUE)
