source("00.R")

input <- get_aoc_input(2023, 8)

dir <- strsplit(input[1], "")[[1]]

e <- new.env(parent = emptyenv())
for (i in 3:length(input)) {
    x <- strsplit(input[i], "[ =(),]")[[1]] %>% keep(~ (nchar(.) > 0))
    assign(x[1], x[2:3], envir = e)
}

where <- ls(envir = e) %>% grep("A$", ., value = TRUE)

process_it <- function(where, end = "Z$", d = dir) {
    dir <- d
    step <- 0
    i <- 1
    while(TRUE) {
        lr <- map(where, ~ get(., envir = e))
        if (dir[i] == "L") {
            where <- map_chr(lr, 1)
        } else {
            where <- map_chr(lr, 2)
        }
        step <- step + 1
        if (all(grepl(end, where))) {
            break
        }
        i <- i + 1
        if (i > length(dir)) {
            i <- 1
        }
    }
    step
}

to_zs <- map_int(where, process_it)

library(gmp)
map(to_zs, as.bigz) -> bz
bzlcm <- bz[[1]]
for (i in 2:length(bz)) {
    bzlcm <- lcm.bigz(bzlcm, bz[[i]])
}

# 16722109789192057789770047 <- too big
# 13289612809129 <- correct!
# brute force had only made it to step 191804702 after many hours... 