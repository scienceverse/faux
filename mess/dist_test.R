#devtools::install_github("scienceverse/faux")
library(faux)

# compare the correlations between simulated normally distributed data and the likert-scale version
check_likert <- function(n = 100, r = 0.5, 
                         prob = c(.05, .1, .2, .3, .2, .1, .05)) {
  df <- faux::rnorm_multi(n, 2, 0, 1, r) %>%
    dplyr::mutate(
      L1 = norm2likert(X1, prob, 0, 1),
      L2 = norm2likert(X2, prob, 0, 1)
    )
  list(norm = cor(df$X1, df$X2), 
       likert = cor(df$L1, df$L2))
}

# compare the correlations between simulated normally distributed data and the truncated version
check_trunc <- function(n = 100, mu = 0, sd = 1, r = 0.5,
                        min = -Inf, max = Inf) {
  df <- rnorm_multi(n, 2, mu, sd, r) %>%
    dplyr::mutate(
      T1 = norm2trunc(X1, min, max, mu, sd),
      T2 = norm2trunc(X2, min, max, mu, sd)
    )
  list(norm = cor(df$X1, df$X2), 
       trunc = cor(df$T1, df$T2))
}

# symmetric normal-ish likert distribution
x <- purrr::map_df(1:1000, ~check_likert())
plot(x$norm, x$likert)
cor(x$norm, x$likert)

# symmetric low-n likert
x <- purrr::map_df(1:1000, ~check_likert(prob = c(.1, .2, .4, .2, .1)))
plot(x$norm, x$likert)
cor(x$norm, x$likert)

# asymmetric likert
x <- purrr::map_df(1:1000, ~check_likert(prob = c(.05, .1, .15, .2, .2, .25, .05)))
plot(x$norm, x$likert)
cor(x$norm, x$likert)

# asymmetric low-n likert
x <- purrr::map_df(1:1000, ~check_likert(prob = c(.1, .2, .3, .4)))
plot(x$norm, x$likert)
cor(x$norm, x$likert)

# truncated to 1 SD
x <- purrr::map_df(1:1000, ~check_trunc())
plot(x$norm, x$trunc)
cor(x$norm, x$trunc)

# truncated to N(3.5, 2) to 1-7
x <- purrr::map_df(1:1000, ~check_trunc(100, 3.5, 2, 0.5, 1, 7))
plot(x$norm, x$trunc)
cor(x$norm, x$trunc)


## check limits of typical rtruncnorm by n
library(ggplot2)

ns <- c(seq(10, 90, 10), seq(100, 1000, 100), seq(2000, 10000, 1000))
y <- purrr::map_df(rep(ns, 1000), function(n) {
  x <- truncnorm::rtruncnorm(n)
  list(n = n, min = min(x), max = max(x))
})

y %>%
  dplyr::group_by(n) %>%
  dplyr::summarise(
    mean_min = mean(min),
    sd_min = sd(min),
    mean_max = mean(max),
    sd_max = sd(max)
  ) %>%
  tidyr::gather(stat, val, mean_min:sd_max) %>%
  tidyr::separate(stat, c("stat", "minmax")) %>%
  ggplot(aes(n, val, color = minmax)) +
  geom_line() +
  facet_wrap(~stat)

y %>%
  tidyr::gather(stat, val, min:max) %>%
  ggplot(aes(log2(n), val, color = stat)) +
  stat_summary(
    fun = mean,
    fun.max = function(x) {mean(x) + sd(x)},
    fun.min = function(x) {mean(x) - sd(x)},
    geom="pointrange"
  ) +
  ylim(-4, 4)

lm(max ~ log2(n), data = y) %>% coef()
lm(min ~ log2(n), data = y) %>% coef()

n <- 100
mu <- 0
sd <- 1
min <- mu - (1.5*sd + 0.22*sd*log2(n))
max <- mu + (1.5*sd + 0.22*sd*log2(n))
