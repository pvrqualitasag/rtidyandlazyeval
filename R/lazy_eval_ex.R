#' ---
#' title: Working Through Examples in JB's Lazy Evaluation Talk
#' date:  "`r Sys.Date()`"
#' ---
#' 
#' ## Disclaimer
#' The content is all taken from the talk under https://resources.rstudio.com/rstudio-conf-2019/lazy-evaluation
#' 
#' 
#' ## Examples With `starwards` Dataset
#+ first-starwars
library(tidyverse)
library(gapminder)

starwars %>%
  filter(homeworld == "Tatooine") %>%
  arrange(height) %>%
  select(name, ends_with("color"))

#' Doing a plot with ggplot
#+ ggplot-cars
ggplot(mpg, aes(displ, hwy, color = class)) + geom_point()


#' ## Base-R cases
#' Example cited by JB from her teaching material of STAT 545
#+ stat-545-ex
lm(lifeExp ~ year, weights = pop, data = gapminder)
subset(gapminder, country == "Chad", select = year:pop)
transform(gapminder, GDP = gdpPercap * pop)
with(gapminder, lifeExp[country == "Chad" & year < 1980])

#' Example that is tried to be transformed into a function
#+ nse-lm
lm(lifeExp ~ poly(I(year - 1952), degree = 2), data = gapminder)

#' A first version of automatisation using apply-functionality
#+ apply-version-by
fit_fun <- function(df){
  lm(lifeExp ~ poly(I(year - 1952), degree = 2), data = df)
}

by(gapminder, gapminder$country, fit_fun)

#' Letting also the lm-formula to vary inside fit_fun, then we have to do
#+ variable-lm-formula
wow <- function(df, y, x){
  lm_formula <- substitute(
    y ~ poly(x, degree),
    list(y = substitute(y), x = substitute(x), degree = 2)
  )
  eval(lm(lm_formula, data = df))
}

wow(gapminder, y = lifeExp, x = year - 1952)
wow(gapminder, y = gdpPercap, x = year - 1952)
wow(gapminder, y = lifeExp, x = gdpPercap)


#' ## The same with tidyverse
#' The functionality is provided with package `rlang`. First, we have a scenario, where 
#' `rlang` is not needed by using the three dots.
#+ three-dots
library(dplyr)
grouped_height <- function(df, ...){
  df %>%
    group_by(...) %>%
    summarise(avg_height = mean(height, na.rm = TRUE))
}

grouped_height(starwars, homeworld)
grouped_height(starwars, species)

#' In slightly more complicated scenarios the functions `enquo()` and `!!`
#' can be used.
#+ enquo-excl
grouped_mean <- function(df, group_var, summary_var){
  group_var <- enquo(group_var)
  summary_var <-  enquo(summary_var)
  
  df %>% 
    group_by(!!group_var) %>%
    summarise(mean = mean(!!summary_var, na.rm = TRUE))
}

grouped_mean(starwars, homeworld, height)
grouped_mean(starwars, homeworld, mass)

#' ## Question: How can this be encapsulated into a function
#' 