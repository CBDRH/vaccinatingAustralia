##########################################################################################################
# Vaccinating Australia: How long will it take?

# Purpose:  Run vaccine rollout projections and generate figures (main article and supplementary)

# Inputs:   R/vaccinateAustraliaExt.R

# Outputs:  1. Figure 1
#           2. Figure 2
#           3. Supplementary Figure 1
#           4. Supplementary Figure 2

##########################################################################################################

# Load libraries
library(tidyverse)
library(RColorBrewer)
library(kableExtra)

# Load vaccine distribution function
source('R/vaccinateAustraliaExt.R')

# Import the vaccination data for Aus
dailyVac <- readr::read_csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv') %>%
    filter(location == 'Australia' & daily_vaccinations > 0) %>%
    mutate(day = row_number())

# Five priority phases
pop <- c(678000, 6139000, 6570000, 6643000, 5670000)

# Schedule for phase opening
openLate <- c(1, 29, 71, 162, 253) # 22 Feb, 22 Mar, 3 May, 2 Aug, 1 Nov
openEarly <- c(1, 29, 71, 134, 134) # 22 Feb, 22 Mar, 3 May, 5 July, 5 July

# Propensitt to be available in each hesitancy group
propensity <- c(0.1, 0.05, 0.005, 0.001)

# High level of vaccinations
hiVac <- function(t){
        case_when(
            t <= 100 ~ t*200000/100,
            t > 100 ~ 200000
        )
}

# Low level of vaccinations
loVac <- function(t){
    case_when(
        t <= 100 ~ t*80000/100,
        t > 100 ~ 80000
    )
}

# Very low level of vaccinations
superloVac <- function(t){
    case_when(
        t <= 100 ~ t*50000/100,
        t > 100 ~ 50000
    )
}

# Visualise vaccine dose levels
data.frame(day = as.Date('2021-02-22') + 1:150,
           lo = loVac(1:150),
           hi = hiVac(1:150)) %>%
    pivot_longer(cols = c('lo', 'hi')) %>%
    ggplot(aes(x = day, y = value, color = name)) +
    geom_line()


# Compile dataframe of hesitancy levels
dfHesitancy <- tibble(
    hp = c(c(0.9, 0.1, 0, 0), rep(c(0.60, 0.28, 0.07, 0.05), 4), c(0.9, 0.1, 0, 0, 0.43, 0.36, 0.13, 0.08, 0.30, 0.40, 0.20, 0.10), rep(c(0.43, 0.36, 0.13, 0.08), 2)),
    level = factor(rep(c(1, 2), each = 20), labels = list('1' = 'More vaccine willingness', '2' = 'More vacccine hesitancy')),
    phase = rep(rep(c('1a', '1b', '2a', '2b', '3'), each = 4), 2),
    category = factor(rep(c(1:4), 10), labels = list('1' = 'Definitely will',
                                                     '2' = 'Probably will',
                                                     '3' = 'Probably won\'t',
                                                     '4' = 'Definitely won\'t'))
)

# Visualise hesitancy levels
dfHesitancy %>%
ggplot(aes(y = hp, x = phase, fill = rev(category))) +
    geom_col() +
    facet_wrap(~level, nrow = 1) +
    scale_fill_manual('Vaccine attitude',
                      values = brewer.pal(4, 'RdYlGn'),
                      labels = rev(levels(dfHesitancy$category))) +
    scale_x_discrete('Scheduled phase') +
    scale_y_continuous('Proportion of phase', labels = scales::percent)



#######################
### Run simulations ###
#######################

days <- 800 # number of simulation days

# Hesitancy settings
moreWilling <- dfHesitancy$hp[1:20]
moreHesitant <- dfHesitancy$hp[21:40]

# Calculate rollout using aus_allocation function (cf vaccinateAustraliaExt.R)
scn1 <- aus_allocation(loVac, pop, days, openEarly, moreWilling, propensity)
scn2 <- aus_allocation(loVac, pop, days, openEarly, moreHesitant, propensity)
scn3 <- aus_allocation(loVac, pop, days, openLate, moreWilling, propensity)
scn4 <- aus_allocation(loVac, pop, days, openLate, moreHesitant, propensity)
scn5 <- aus_allocation(hiVac, pop, days, openEarly, moreWilling, propensity)
scn6 <- aus_allocation(hiVac, pop, days, openEarly, moreHesitant, propensity)
scn7 <- aus_allocation(hiVac, pop, days, openLate, moreWilling, propensity)
scn8 <- aus_allocation(hiVac, pop, days, openLate, moreHesitant, propensity)

# Compile scenario details
dfScenarios <-
    data.frame(
        scenario = c(1:8),
        capacity = rep(c('80,000', '200,000'), each = 4),
        opening = rep(rep(c('Earlier opening', 'Later opening'), each = 2), 2),
        hesitancy = rep(c('Less hesitant', 'More hesitant'), 4)
    )

# Compile administered first and second doses
dfDose1 <- bind_rows(
    scn1$doseOneGiven,
    scn2$doseOneGiven,
    scn3$doseOneGiven,
    scn4$doseOneGiven,
    scn5$doseOneGiven,
    scn6$doseOneGiven,
    scn7$doseOneGiven,
    scn8$doseOneGiven,
    .id = 'scenario'
)

dfDose2 <- bind_rows(
    scn1$doseTwoGiven,
    scn2$doseTwoGiven,
    scn3$doseTwoGiven,
    scn4$doseTwoGiven,
    scn5$doseTwoGiven,
    scn6$doseTwoGiven,
    scn7$doseTwoGiven,
    scn8$doseTwoGiven,
    .id = 'scenario'
)

# Compile administered doses by phase
dfPhases <- bind_rows(dfDose1, dfDose2, .id = 'dose') %>%
    group_by(scenario, dose) %>%
    mutate(
        date = as.Date('2021-02-20') + row_number(),
        phase1a = V1+V2+V3+V4,
        phase1b = V5+V6+V7+V8,
        phase2a = V9+V10+V11+V12,
        phase2b = V13+V14+V15+V16,
        phase3 = V17+V18+V19+V20,
        scenario = factor(scenario, levels = c(1:8), labels = paste('Scenario', 1:8)),
        dose = factor(dose,
                      levels = c('2', '1'),
                      labels = c('Second dose', 'First dose'),
                      ordered = TRUE)) %>%
    select(-starts_with('V')) %>%
    pivot_longer(starts_with('phase'),
                 values_to = 'n',
                 names_to = 'phase',
                 names_prefix = 'phase'
                 ) %>%
    relocate(scenario, date, dose, phase, n) %>%
    arrange(scenario, date, dose, phase, n) %>%
    filter(date != as.Date('2021-02-21'))



dfDoses <- dfPhases %>%
    group_by(scenario, date, dose) %>%
    summarise(n = sum(n)) %>%
    group_by(scenario, dose) %>%
    mutate(doseCuml = cumsum(n)) %>%
    group_by(scenario, date) %>%
    mutate(doseAllCuml = sum(doseCuml))


# summarise key dates
dfDates <- dfPhases %>%
    group_by(scenario, date, phase) %>%
    summarise(n = sum(n)) %>%
    arrange(scenario, phase, date) %>%
    group_by(scenario, phase) %>%
    mutate(doseCuml = cumsum(n),
           popSize = case_when(
                    phase == '1a' ~ pop[1],
                    phase == '1b' ~ pop[2],
                    phase == '2a' ~ pop[3],
                    phase == '2b' ~ pop[4],
                    phase == '3' ~ pop[5]),
           pctDone = case_when(
               doseCuml >= popSize*2*0.90 ~ 'd',
               doseCuml >= popSize*2*0.80 ~ 'c',
               doseCuml >= popSize*2*0.70 ~ 'b',
               doseCuml < popSize*2*0.70 ~ 'a')
           ) %>%
    group_by(scenario, phase, pctDone) %>%
    filter(n > 0) %>%
    summarise(start = min(date), end = max(date)) %>%
    arrange(scenario, phase, pctDone)

# Store key datees in a table
dfDatesTable <- dfDates %>%
    group_by(scenario, phase) %>%
    filter(pctDone !='d') %>% # due to hesitancy, 90% is rarely achieved
    summarise(start = min(start), end = max(end)) %>%
    mutate(
        ymin = seq(40, 64, by = 6),
        ymax = seq(43, 67, by = 6),
        phaseLab = paste('Phase', phase)
    )
save(dfDatesTable, file = paste(here::here('Submission/Vaccine/2 - Revision 1/'), lubridate::today(), '-dfDatesTable.Rda'))


dfDatesPlot <- dfDatesTable %>%
    filter(scenario %in% c('Scenario 1', 'Scenario 5')) %>%
    mutate(scenarioLab = case_when(
        scenario == 'Scenario 1' ~ 'A. Scenario 1 (80,000 doses per day)',
        scenario == 'Scenario 5' ~ 'B. Scenario 5 (200,000 doses per day)'
    ))


dfDosesPlot <- dfDoses %>%
    filter(doseAllCuml <= sum(pop)*2*1 & scenario %in% c('Scenario 1', 'Scenario 5')) %>%
    mutate(
        scenarioLab = case_when(
            scenario == 'Scenario 1' ~ 'A. Scenario 1 (80,000 doses per day)',
            scenario == 'Scenario 5' ~ 'B. Scenario 5 (200,000 doses per day)'
        ),
        scenarioCut = case_when(
            scenario == 'Scenario 1' ~ max(dfDatesPlot$end[dfDatesPlot$scenario=='Scenario 1']),
            scenario == 'Scenario 5' ~ max(dfDatesPlot$end[dfDatesPlot$scenario=='Scenario 5'])
        )
    ) %>%
    filter(date <= scenarioCut)


## Revised Figure 1
ggplot() +
    geom_rect(data = dfDatesPlot,
              aes(xmin = start,
                  xmax = end,
                  ymin = ymin,
                  ymax = ymax),
              fill = 'grey40') +
    geom_text(data = dfDatesPlot,
              aes(x = start, y = ymin, label = phaseLab),
              size = 2.0,
              nudge_x = -30,
              nudge_y = 1,
              color = 'grey40') +
    geom_col(data = dfDosesPlot,
             aes(x = date, y = doseCuml/1E6, group = dose, fill = dose),
             width=1,
             alpha = 0.65) +
    geom_line(data = dfDosesPlot,
              aes(x = date, y = doseCuml/1E6, color = dose),
              position = 'stack',
              size = 1.5) +
    scale_y_continuous('Cumulative vaccine doses administered (millions)',
                       labels = scales::comma,
                       limits = c(0,70)) +
    scale_x_date('Date', date_labels = '%b \'%y',
                 date_breaks = '3 months',
                 minor_breaks = 'month',
                 limits = c(as.Date('2021/01/01'), as.Date('2023/03/01'))) +
    scale_fill_manual(NULL,
                      values = c("First dose" = '#fc8d59',
                                 "Second dose"= '#91bfdb'),
                      guide =  guide_legend(reverse = TRUE)) +
    scale_color_manual(NULL,
                       values = c("First dose" = '#fc8d59',
                                  "Second dose"= '#91bfdb'),
                       guide =  guide_legend(reverse = TRUE)) +
    theme(legend.position = 'bottom') +
    facet_wrap(~scenarioLab, ncol = 1, strip.position = 'top', dir = 'v')

ggsave(filename = paste0(here::here('Submission/Vaccine/2 - Revision 1/'), lubridate::today(), '-figure-1.png'),
       width = 6, height = 4, units = 'in')



## Revised Figure 2.

# Data on daily vaccinations sourced from Our World in Data
dailyVac <- readr::read_csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv')
dv1 <- dailyVac %>% filter(location %in% c('United States',
                                           'United Kingdom',
                                           'European Union',
                                           'Australia',
                                           'Canada',
                                           'Israel')) %>%
    filter(date >= as.Date("2021/01/01")) %>%
    group_by(location) %>%
    mutate(label = if_else(date == max(date), as.character(location), NA_character_)) %>%
    ungroup()

# Supplementary Figure 1
ggplot(dv1,
       aes(x = date,
           y = daily_vaccinations_per_million,
           group = location,
           color = location)) +
    geom_hline(aes(yintercept = 7800), size = 3, alpha = .2, color = 'blue') +
    # ggrepel::geom_label_repel(aes(label = label),
    #                  direction = "y", hjust = "left",
    #                  nudge_x = 3,
    #                  na.rm = TRUE) +
    geom_line(size = 1.2) +
    scale_y_continuous(name = 'Daily vaccinations per million', labels = scales::comma_format()) +
    scale_x_date(name = 'Date', date_labels = '%b',
                 breaks = 'month',
                 limits = c(as.Date('2021/01/01'), lubridate::today()  + lubridate::period(1, "weeks"))) +
    theme(legend.position = "none") +
    facet_wrap(~location) +
    labs(caption = "Data source: ourworldindata.org")

ggsave(filename = paste0(here::here('Submission/Vaccine/2 - Revision 1/'), lubridate::today(), '-figure-2.png'))


# Appendix - daily administered doses
tot <- dfDoses %>% ungroup() %>%
    group_by(scenario, date) %>%
    summarise(n = sum(n))

ggplot(tot, aes(x = date, y = n/1000)) +
    geom_line() +
    facet_wrap(~scenario, nrow = 4) +
    scale_y_continuous("Daily administered doses ('000s)") +
    scale_x_date('Date',
                 breaks = c(as.Date('2021-03-01'), as.Date('2022-01-01'), as.Date('2022-09-01')),
                 labels = c("Mar '21", "Jan '22", "Sep '22"),
                 limits = c(as.Date('2021-01-01'), as.Date('2023-01-01')),
                 date_minor_breaks = '1 month')

ggsave(filename = paste0(here::here('Submission/Vaccine/2 - Revision 1/'), lubridate::today(), '-supplementary-fig1.png'),
       width = 4, height = 6, units = 'in')


## Appendix rollout for all scenarios

dfDatesPlotA <- dfDatesTable %>%
    mutate(scenarioLab = case_when(
        scenario == 'Scenario 1' ~ 'A. Scenario 1 (80,000 doses per day)',
        scenario == 'Scenario 5' ~ 'B. Scenario 5 (200,000 doses per day)'
    ))


dfDosesPlotA <- dfDoses %>%
    filter(doseAllCuml <= sum(pop)*2*1) %>%
    mutate(
        scenarioCut = case_when(
            scenario == 'Scenario 1' ~ max(dfDatesPlotA$end[dfDatesPlotA$scenario=='Scenario 1']),
            scenario == 'Scenario 2' ~ max(dfDatesPlotA$end[dfDatesPlotA$scenario=='Scenario 2']),
            scenario == 'Scenario 3' ~ max(dfDatesPlotA$end[dfDatesPlotA$scenario=='Scenario 3']),
            scenario == 'Scenario 4' ~ max(dfDatesPlotA$end[dfDatesPlotA$scenario=='Scenario 4']),
            scenario == 'Scenario 5' ~ max(dfDatesPlotA$end[dfDatesPlotA$scenario=='Scenario 5']),
            scenario == 'Scenario 6' ~ max(dfDatesPlotA$end[dfDatesPlotA$scenario=='Scenario 6']),
            scenario == 'Scenario 7' ~ max(dfDatesPlotA$end[dfDatesPlotA$scenario=='Scenario 7']),
            scenario == 'Scenario 8' ~ max(dfDatesPlotA$end[dfDatesPlotA$scenario=='Scenario 8'])
        )
    ) %>%
    filter(date <= scenarioCut)


## Supplementary Figure 2
ggplot() +
    geom_rect(data = dfDatesPlotA,
              aes(xmin = start,
                  xmax = end,
                  ymin = ymin,
                  ymax = ymax),
              fill = 'grey40') +
    geom_text(data = dfDatesPlotA,
              aes(x = start, y = ymin, label = phase),
              size = 3.5,
              nudge_x = -30,
              nudge_y = 1,
              color = 'grey40') +
    geom_col(data = dfDosesPlotA,
             aes(x = date, y = doseCuml/1E6, group = dose, fill = dose),
             width=1,
             alpha = 0.65) +
    geom_line(data = dfDosesPlotA,
              aes(x = date, y = doseCuml/1E6, color = dose),
              position = 'stack',
              size = 1.5) +
    scale_y_continuous('Cumulative vaccine doses administered (millions)',
                       labels = scales::comma,
                       limits = c(0,70)) +
    scale_x_date('Date', date_labels = '%b %y',
                 breaks = c(as.Date('2021-01-01'),
                            as.Date('2021-07-01'),
                            as.Date('2022-01-01'),
                            as.Date('2022-07-01'),
                            as.Date('2023-01-01'),
                            as.Date('2023-07-01')),
                 minor_breaks = 'month',
                 limits = c(as.Date('2021/01/01'), as.Date('2023/06/01'))) +
    scale_fill_manual(NULL,
                      values = c("First dose" = '#fc8d59',
                                 "Second dose"= '#91bfdb'),
                      guide =  guide_legend(reverse = TRUE)) +
    scale_color_manual(NULL,
                       values = c("First dose" = '#fc8d59',
                                  "Second dose"= '#91bfdb'),
                       guide =  guide_legend(reverse = TRUE)) +
    theme(legend.position = 'bottom') +
    facet_wrap(~scenario, ncol = 2, strip.position = 'top', dir = 'h')

ggsave(filename = paste0(here::here('Submission/Vaccine/2 - Revision 1/'), lubridate::today(), '-supplementary-fig2.png'),
       width = 6, height = 8, units = 'in')






