source("00.R")

input <- get_aoc_input(2023, 8)

dir <- strsplit(input[1], "")[[1]]

e <- new.env(parent = emptyenv())
for (i in 3:length(input)) {
    x <- strsplit(input[i], "[ =(),]")[[1]] %>% keep(~ (nchar(.) > 0))
    assign(x[1], x[2:3], envir = e)
}

where <- ls(envir = e) %>% grep("A$", ., value = TRUE)
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
    if (all(grepl("Z$", where))) {
        break
    }
    i <- i + 1
    if (i > length(dir)) {
        i <- 1
    }
}

step