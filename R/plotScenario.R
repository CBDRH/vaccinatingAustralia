# Function to plot scenario data
library(ggplot2)
library(dplyr)
library(RColorBrewer)

plotScenario <- function(scenario, title) {
dates <- scenario$dataOut %>%
    mutate(phaseX = substr(phase, 1, 2)) %>%
    group_by(phaseX) %>%
    filter(started & !finished) %>%
    summarise(x1 = min(date),
              x2 = max(date)) %>%
    mutate(y1 = seq(0.4, 1.0, by = 0.15),
              y2 = seq(0.5, 1.10, by = 0.15),
           phase = paste('Phase', phaseX))

phases <- scenario$doses %>%
    group_by(date, dose) %>%
    summarise(n=sum(n), denom = sum(eligible)/2) %>%
    group_by(dose) %>%
    mutate(cumlN = cumsum(n),
           cumlPct = cumlN/denom,
           schedule = factor(dose,
                             levels = c('2', '1'),
                             labels = c('Second dose', 'First dose')))

p <- ggplot() +
    geom_rect(data = dates,
              aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2),
              fill = brewer.pal(9, 'Reds')[1:5]) +
    geom_text(data=dates,
              aes(x = x1, y = y1, label = phaseX),
              size = 4,
              nudge_x = -20,
              nudge_y = .05,
              color = brewer.pal(9, 'Reds')[1:5]) +
    geom_bar(data = phases,
             aes(x = date, y = cumlPct/2, group = schedule, fill = schedule),
             stat = 'identity', position = 'stack', width = 2) +
    scale_y_continuous(name = 'Percent', labels = scales::percent, limits = c(0,1.1),
                       breaks = seq(0, 1, by = 0.25), minor_breaks = NULL) +
    scale_x_date(name = 'Date', date_labels = '%b%y',
                 breaks = seq(as.Date('2021/01/01'), as.Date('2023/07/31'), by = 'quarter'),
                 minor_breaks = 'month',
                 limits = c(as.Date('2021/01/01'), as.Date('2023/07/31'))) +
    scale_fill_manual(name=NULL,
                      values = c(brewer.pal(9, 'YlOrRd')[1],
                                 brewer.pal(9, 'YlOrRd')[3])) +
    labs(subtitle = title) +
    theme_dark()

return(p)
}
