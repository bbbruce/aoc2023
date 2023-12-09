source("00.R")

input <- get_aoc_input(2023, 5)

seeds_ranges <- as.numeric(tail(strsplit(input[1], " ")[[1]], -1))

seeds <- data.frame(start = numeric(),
                    end = numeric())

for (i in seq(1, length(seeds_ranges), by = 2)) {
    seeds <- rbind(seeds, data.frame(start = seeds_ranges[i],
        end = seeds_ranges[i] + seeds_ranges[i + 1] - 1))
}

seeds <- arrange(seeds, start)

maps <- list()
input <- c(input, "") # to get last map inserted

for (l in input[3:length(input)]) {
    if (l == "") {
        map <- map %>% arrange(start)
        maps[[length(maps) + 1]] <- map
    } else if (grepl("map", l)) {
        print(l)
        map <- data.frame(start = numeric(), end = numeric(), diff = numeric())
    } else {
        mm <- setNames(as.numeric(strsplit(l, " ")[[1]]), c("d", "s", "l"))
        map <- rbind(map, data.frame(start = mm["s"], 
            end = mm["s"] + mm["l"] - 1, 
            diff = mm["d"] - mm["s"]))
    }
}

find_map_rows <- function(m, s) {
    m %>%
        filter((start >= s$start &
                start <= s$end) | 
                (end >= s$start &
                 end <= s$end) |
                (start <= s$start &
                 end >= s$end))   
}

apply_map <- function(seeds, map) {
    outseeds <- data.frame(start = numeric(), end = numeric())
    for(i in 1:NROW(seeds)) {
        s <- seeds[i, ]
        rr <- find_map_rows(map, s)
        if(NROW(rr) > 0) {
            if(s$start >= rr[1, "start"]) {
                rr[1, "start"] <- s$start
            } else {
                rr <- rbind(data.frame(start = s$start,
                    end = rr[1, "start"] - 1, diff = 0), rr)
            }
            if(s$end <= rr[NROW(rr), "end"]) {
                rr[NROW(rr), "end"] <- s$end
            } else {
                rr <- rbind(rr, data.frame(start = rr[NROW(rr), "end"] + 1,
                    end = s$end, diff = 0))
            }
            os <- data.frame(start = rr$start + rr$diff,
                             end = rr$end + rr$diff)
            outseeds <- rbind(outseeds, os)
        } else {
            outseeds <- rbind(outseeds, seeds[i, ])
        }
    }
    outseeds %>%
        arrange(start) ->
        out
    print(out)
    out
}

count_seeds <- function(seeds) {
    seeds %>%
        mutate(n = end - start + 1) %>%
        pull(n) %>%
        sum()
}

final_seeds <- Reduce(apply_map, maps, seeds)
# check no seeds were lost or created
stopifnot(count_seeds(seeds) == count_seeds(final_seeds))

print(head(final_seeds))
