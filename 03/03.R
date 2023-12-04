source("00.R")

input <- get_aoc_input(2023, 3)

d <- input2array(input, as.character)

part_nums <- numeric()
not_part_nums <- numeric()

for (x in 1:NROW(d)) {
    num <- ""
    is_part_num <- FALSE
    for (y in 1:NCOL(d)) {
        c <- read_w_bounds(d, x, y)
        if(grepl("[0-9]", c)) {
            num <- paste0(num, c)
            if(grepl("[^.0-9]", paste(na.omit(read_local_chr(d, x, y)), collapse = ""))) {
                is_part_num <- TRUE
            }
        } else {
            if(nchar(num) > 0) {
                if(is_part_num) {
                    part_nums <- c(part_nums, num)
                } else {
                    not_part_nums <- c(not_part_nums, num)
                }
                num <- ""
                is_part_num <- FALSE
            }
        }
    }
    if(nchar(num) > 0) {
                if(is_part_num) {
                    part_nums <- c(part_nums, num)
                } else {
                    not_part_nums <- c(not_part_nums, num)
                }
            }
}

gear_builder <- list()
for (x in 1:NROW(d)) {
    num <- ""
    maybe_gear <- FALSE
    for (y in 1:NCOL(d)) {
        c <- read_w_bounds(d, x, y)
        if(grepl("[0-9]", c)) {
            num <- paste0(num, c)
            if(grepl("[*]", paste(na.omit(read_local_chr(d, x, y)), collapse = ""))) {
                maybe_gear <- TRUE
                read_local_df_chr(d, x, y) %>%
                    filter(v == "*") ->
                    gear_rc
            }
        } else {
            if(nchar(num) > 0) {
                if(maybe_gear) {
                    gear_rc$v <- num
                    gear_builder <- rbind(gear_builder, gear_rc)
                }
                num <- ""
                maybe_gear <- FALSE
            }
        }
    }
    if(nchar(num) > 0) {
                if(maybe_gear) {
                    gear_rc$v <- num
                    gear_builder <- rbind(gear_builder, gear_rc)
                }
                num <- ""
                maybe_gear <- FALSE
            
    }
}

inner_join(gear_builder, gear_builder, by = join_by(r, c), multiple = "first") %>%
    filter(v.x != v.y) %>%
    mutate(v = as.numeric(v.x) * as.numeric(v.y)) %>%
    pull(v) %>%
    sum()

