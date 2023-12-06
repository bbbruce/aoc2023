source("00.R")

input <- get_aoc_input(2023, 4)

strsplit(input, "[:|]") %>%
    map(~ strsplit(., " ")) ->
    d

d2 <- lapply(d, function(l) list(na.omit(ifelse(l[[2]] == "", NA, as.numeric(l[[2]]))),
    na.omit(ifelse(l[[3]] == "", NA, as.numeric(l[[3]])))))

z <- sapply(d2, function(x) sum(map_lgl(x[[1]], ~ . %in% x[[2]])))
z <- 2^(z - 1)
sum(ifelse(z < 1, 0, z))

z2 <- sapply(d2, function(x) sum(map_lgl(x[[1]], ~ . %in% x[[2]])))

ncards <- rep(1, length(z2))

for(i in 1:length(ncards)) { #length(ncards)) {
    if(z2[i] > 0) {
        ncards[(i + 1):(i + z2[i])] <-  ncards[(i + 1):(i + z2[i])] + ncards[i]
    }
}
ncards
sum(ncards, na.rm = TRUE)
