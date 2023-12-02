source("00.R")

input <- get_aoc_input(2023, 1) 

input2df(input) %>%
    mutate(V2 = gsub("[^0-9]", "", V1)) %>%
    mutate(V3 = gsub("^(.)(.*)(.)$", "\\1\\3", V2)) %>%
    mutate(V4 = ifelse(nchar(V3) == 1, paste0(V3, V3), V3)) %>%
    pull(V4) %>%
    as.numeric() %>%
    sum()

d <- input2df(input)


s1 <- function(x) gsub("([0-9]|one|two|three|four|five|six|seven|eight|nine|zero)(.*)", "\\1", x)
s2 <- function(x) gsub("^(.*)([0-9]|one|two|three|four|five|six|seven|eight|nine|zero)(.*?)$", "\\2", x)

getdigit <- function(x) {
    ifelse(nchar(x) == 1,
        x,
        as.character(match(x, c("zero", "one", "two", "three", 
        "four", "five", "six", "seven", "eight", "nine")) - 1))
}

sum(as.numeric(paste0(getdigit(s2(s1(d$V1))), getdigit(s2(d$V1)))))
