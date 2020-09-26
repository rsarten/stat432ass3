
dataset <- data.frame(
  size = c(2, 3, 2, 7, 5, 5, 3, 2, 6, 10, 3, 2, 2, 5, 6, 4, 4, 5, 4, 4, 6, 5, 4, 2),
  village = rep(c("A", "B", "C"), c(9, 8, 7))
)

N <- 1000

set.seed(150)
s <- 1:N %>% 
  lapply(function(i) sample(dataset$village)) %>%
  lapply(function(groups) lm(dataset$size ~ groups)) %>% 
  lapply(summary)

calc_F <- function(input)
  input[["fstatistic"]]["value"]

calc_p <- function(input)
  pf(input$fstatistic, input$df[1]-1, input$df[2], lower.tail = FALSE)["value"]

calc_r2 <- function(input)
  input[["r.squared"]]

obs_F <- lm(size ~ village, data = dataset) %>% summary() %>% calc_F() %>% round(digits = 3)
F_ <- sapply(s, calc_F) %>% round(digits = 3)
sum(F_ >= obs_F)/N

obs_r2 <- lm(size ~ village, data = dataset) %>% summary() %>% calc_r2() %>% round(digits = 3)
r2 <- sapply(s, calc_r2) %>% round(digits = 3) 
sum(obs_r2 <= r2)/N

obs_p <- lm(size ~ village, data = dataset) %>% summary() %>% calc_p() %>% round(digits = 3)
p_ <- sapply(s, calc_p) %>% round(digits = 3)
sum(obs_p >= p_)/N

ggplot() + 
  geom_histogram(data = data.frame(x = p_), 
                 aes(x = x), breaks = seq(from = 0, to = max(p_), length.out = 10), 
                 fill = "white", colour = "grey") +
  geom_vline(xintercept = obs_p, colour = "firebrick", linetype = "dashed") +
  ggtitle("Histogram of permutation values") +
  xlab("test statistic")

ggplot() + 
  geom_histogram(data = data.frame(x = r2), 
                 aes(x = x), breaks = seq(from = 0, to = max(r2), length.out = 10), 
                 fill = "white", colour = "grey") +
  geom_vline(xintercept = obs_r2, colour = "firebrick", linetype = "dashed") +
  ggtitle("Histogram of permutation values") +
  xlab("test statistic")

hist(p_)
seq(from = 0, to = max(p_), by = 0.1)
?seq
seq(from = 0, to = max(p_), length.out = 10)
