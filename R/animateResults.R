# Purpose: Generate a animated gif of the main message

# Load data
load('Outputs/sc1.rda')
load('Outputs/sc5.rda')

# Libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(gganimate)
library(ggtext)

# Start dates for each phase @ 80,000 vaccines per day
dates1 <- sc1$dataOut %>%
    mutate(phaseX = substr(phase, 1, 2)) %>%
    group_by(phaseX) %>%
    filter(started & !finished) %>%
    summarise(starts = min(date)) %>%
    pivot_longer(cols = c(starts), names_to = 'desc', values_to = 'date') %>%
    mutate(lab = paste("Phase", phaseX))

# Start dates for each phase @ 200,000 vaccines per day
dates2 <- sc5$dataOut %>%
    mutate(phaseX = substr(phase, 1, 2)) %>%
    group_by(phaseX) %>%
    filter(started & !finished) %>%
    summarise(starts = min(date)) %>%
    pivot_longer(cols = c(starts), names_to = 'desc', values_to = 'date') %>%
    mutate(lab = paste("Phase", phaseX))

# calculate the population denominators
denom1 <- sc1$dataOut %>% group_by(phase) %>% summarise(eligible = first(eligible)) %>% summarise(n1 = sum(eligible)) %>% as.numeric()
denom2 <- sc5$dataOut %>% group_by(phase) %>% summarise(eligible = first(eligible)) %>% summarise(n1 = sum(eligible)) %>% as.numeric()

# Calculate cumulative percent complete over time @ 80,000 vaccines per day
ts1 <- sc1$dataOut %>%
    mutate(phaseX = substr(phase, 1, 2)) %>%
    filter(started & !finished) %>%
    group_by(date) %>%
    summarise(n = sum(n)) %>%
    mutate(cumln = cumsum(n)) %>%
    full_join(dates1, by = 'date') %>%
    fill(lab) %>%
    select(-c(phaseX, desc, n)) %>%
    mutate(speed = 'a',
           cumlPct = cumln/denom1)

# Calculate cumulative percent complete over time @ 200,000 vaccines per day
ts2 <- sc5$dataOut %>%
    mutate(phaseX = substr(phase, 1, 2)) %>%
    filter(started & !finished) %>%
    group_by(date) %>%
    summarise(n = sum(n)) %>%
    mutate(cumln = cumsum(n)) %>%
    full_join(dates2, by = 'date') %>%
    fill(lab) %>%
    select(-c(phaseX, desc, n)) %>%
    mutate(speed = 'b',
           cumlPct = cumln/denom2)

# Combine the tieseries data
ts <- bind_rows(ts1, ts2) %>%
        mutate(lab = ifelse(is.na(lab), "", lab),
               label = paste0('**', format(date, '%B'), ":", '** <br>', lab))

# Create the animation
anim <- ggplot(ts,
               aes(x = date,
                   y = cumlPct,
                   group = speed,
                   label = label)
               ) +
    geom_line(aes(color = speed), size = 2) +
    geom_richtext() +
    scale_y_continuous(name = "Percentage",
                       labels = scales::percent, limits = c(0,1.0)) +
    scale_x_date(name = "Date",
                 breaks = seq(as.Date("2021/03/01"), as.Date("2022/12/31"), by = 'quarter'),
                 date_labels = "%b '%y") +
    scale_colour_discrete(name = "Daily vaccinations",
                          labels = c("80,000", "200,000")) +
    theme_dark() +
    theme(legend.position = 'bottom') +
    labs(title = 'Percent of population vaccinated with two doses',
         subtitle = "Date: {frame_along}") +
    transition_reveal(date)

# Animate
animate(anim,
        nframes = 400,
        fps = 20,
        renderer = gifski_renderer(loop = FALSE))

# Save
anim_save('Outputs/gif-1.gif')

# ### JAGO ### #
