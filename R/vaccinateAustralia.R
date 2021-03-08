# Function to calculate time to vaccinate given daily units, second dose scheduling dates and proportion hesitant

vaccinateAustralia <- function(unitsMax, startDate, gapStart, gapEnd, hesitancy){

# Define run-in period (will fix this at 6 weeks)
runIn <- 42

# Define hesitant proportion
hesitant <- setNames(as.data.frame(t(hesitancy * dist)), phase)

# Doses administered on day 1 (all go to phase1 priority)
doseOneGiven <- setNames(as.data.frame(t(c(unitsMax/runIn*dist[1:4]/sum(dist[1:4]), rep(0,12)))), phase)
doseTwoGiven <- setNames(as.data.frame(t(rep(0,16))), phase)

# Doses left to be done on day 1
doseOneRemaining <- setNames(as.data.frame(t(dist)), phase) - hesitant - doseOneGiven
doseTwoRemaining <- setNames(as.data.frame(t(dist)), phase) - hesitant

dosesGiven <- doseOneGiven + doseTwoGiven
dosesRemaining <- doseOneRemaining + doseTwoRemaining

i <- 2 # DAy one is distributed above so start from day 2
while (any(floor(doseTwoRemaining[i-1, ]) > 1)) {

    units <- ifelse(i <= runIn, i*unitsMax/runIn, unitsMax)

     # Lower and upper bounds for lookback period
    lower <- ifelse(i <= gapStart, 1, max(1, i - gapEnd + 1))
    upper <- ifelse(i <= gapStart, 1, max(1, i - gapStart))

    needDose2 <- colSums(doseOneGiven[lower:upper, ])/(gapEnd - gapStart)

    # Dose 2s are spread out over the specified gap: gapEnd - gapStart
    alottedDose2 <- ifelse(i <= gapStart, 0,
                           min(
                               sum(needDose2),
                               sum(doseTwoRemaining[i-1, ]), # don't give out more than required
                               units # capped at the number of daily units
                               )
                        )
    # What's left is assigned to dose 1
    alottedDose1 <- units - alottedDose2

    # Calculation to distribute allotted dose two doses

    if (alottedDose2 > 0) {
        distribution2 <- alottedDose2*needDose2/sum(needDose2)
        } else distribution2 <- t(rep(0,16))

    # Calculation to distribute allotted dose one doses
    if (alottedDose1 > 0 & sum(doseOneRemaining[i-1, ]) > 0){ # only assign if there is indeed some doses ones available
    df1 <- data.frame(phase = seq(1, length(dist)),
                     priority = c(rep(1,4), rep(2,6), rep(3,4), 4, 5),
                     full = as.vector(doseOneRemaining[i-1, ] <= 0)
    )

    df2 <- df1 %>% filter(!full)

    df3 <- df2 %>% filter(priority == min(priority))

    df3$new <- pmin(alottedDose1*dist[df3$phase]/sum(dist[df3$phase]),
                    doseOneRemaining[i-1, df3$phase])

    distribution1 <- df1 %>%
        left_join(df3, by = 'phase') %>%
        mutate(new = ifelse(is.na(new), 0, new)) %>%
        select(new) %>%
        t()
    } else distribution1 <- t(rep(0,16)) # Otherwise just set to zero

    # Update data
    doseOneGiven[i, ] <- distribution1
    doseTwoGiven[i, ] <- distribution2
    doseOneRemaining[i, ] <- doseOneRemaining[i-1, ] - doseOneGiven[i, ]
    doseTwoRemaining[i, ] <- doseTwoRemaining[i-1, ] - doseTwoGiven[i, ]
    dosesGiven[i, ] <- doseOneGiven[i, ] + doseTwoGiven[i, ]
    dosesRemaining[i, ] <- doseOneRemaining[i, ] + doseTwoRemaining[i, ]

    # increment i
    i <- i + 1
    cat(i, "\n")
}

    # tidy data
    dataOut <- dosesGiven %>%
        dplyr::mutate(t = row_number()) %>%
        tidyr::pivot_longer(cols = names(dosesGiven),
                     names_to = 'phase',
                     names_prefix = 'phase',
                     values_to = 'n') %>%
        dplyr::arrange(phase, t) %>%
        dplyr::mutate(eligible = rep(dist*2, each = nrow(.)/16),
               date = startDate + t - 1) %>%
        dplyr::group_by(phase) %>%
        dplyr::mutate(cumlN = cumsum(n),
               denom = max(cumlN),
               cumlPct = cumlN/eligible,
               started = cumlN > 0,
               finished = cumlN >= denom) %>%
        dplyr::ungroup()


    doseOne <- doseOneGiven %>%
        dplyr::mutate(t = row_number()) %>%
        tidyr::pivot_longer(cols = names(dosesGiven),
                            names_to = 'phase',
                            names_prefix = 'phase',
                            values_to = 'n') %>%
        dplyr::arrange(phase, t) %>%
        dplyr::mutate(eligible = rep(dist*2, each = nrow(.)/16),
                      date = startDate + t - 1) %>%
        dplyr::group_by(phase) %>%
        dplyr::mutate(cumlN = cumsum(n),
                      denom = max(cumlN),
                      cumlPct = cumlN/eligible,
                      started = cumlN > 0,
                      finished = cumlN >= denom,
                      dose = 1) %>%
        dplyr::ungroup()

    doseTwo <- doseTwoGiven %>%
        dplyr::mutate(t = row_number()) %>%
        tidyr::pivot_longer(cols = names(dosesGiven),
                            names_to = 'phase',
                            names_prefix = 'phase',
                            values_to = 'n') %>%
        dplyr::arrange(phase, t) %>%
        dplyr::mutate(eligible = rep(dist*2, each = nrow(.)/16),
                      date = startDate + t - 1) %>%
        dplyr::group_by(phase) %>%
        dplyr::mutate(cumlN = cumsum(n),
                      denom = max(cumlN),
                      cumlPct = cumlN/eligible,
                      started = cumlN > 0,
                      finished = cumlN >= denom,
                      dose = 2) %>%
        dplyr::ungroup()

    doses <- rbind(doseOne, doseTwo)

return(list(
    dataOut = dataOut,
    doses = doses))

}



#'
#'
#' @param sc
#' @param units
#' @param units_eps
#'
check_results <- function(sc,units,gapEnd,gapStart,units_eps=10) {

    with(sc,{
        # check only vaccinate units per day
        tmp <- dataOut %>%
            group_by(t) %>%
            summarise(n=sum(n),.groups="drop")
        cat("1",all(tmp$n <= (units + units_eps)),"\n") # all
        # check everyone is vaccinated twice
        d1 <- doses %>% filter(dose == 1) %>% .$n %>% sum()
        d2 <- doses %>% filter(dose == 2) %>% .$n %>% sum()
        cat("2", all.equal(d1, d2), "\n")
        # total given 2nd dose at t+gapEnd
        tmp1 <- doses %>%
            filter(dose == 1) %>%
            group_by(t) %>%
            summarise(n = sum(n),.groups="drop") %>%
            summarise(n = cumsum(n),.groups="drop")
        tmp2 <- doses %>%
            filter(dose == 2) %>%
            group_by(t) %>%
            summarise(n = sum(n),.groups="drop") %>%
            summarise(n = cumsum(n),.groups="drop")
        N <- nrow(tmp2)
        cat("3",all(tmp2$n[gapEnd:N] >= tmp2$n[1:(N-gapEnd+1)]),"\n")
        cat("4",all(tmp2$n[gapStart:N] <= tmp1$n[1:(N-gapStart+1)]),"\n")
    })
}


