# Function to calculate time to vaccinate given daily units, second dose scheduling dates and proportion hesitant

vaccinateAustralia <- function(units, startDate, gapStart, gapEnd, hesitancy){

# Define hesitant proportion
hesitant <- setNames(as.data.frame(t(round(hesitancy * dist, digits = 1))), phase)

doseOneRemaining <- setNames(as.data.frame(t(dist)), phase) - hesitant
doseTwoRemaining <- setNames(as.data.frame(t(dist)), phase) - hesitant
doseOneGiven <- setNames(as.data.frame(t(rep(0,16))), phase)
doseTwoGiven <- setNames(as.data.frame(t(rep(0,16))), phase)
dosesGiven <- setNames(as.data.frame(t(rep(0,16))), phase)
dosesRemaining <- setNames(as.data.frame(t(dist)), phase)

i <- 1
while (sum(dosesGiven) < (sum(dist) - sum(hesitant))*2) {
     # Today's units for each dose
    lower <- ifelse(i <= gapStart, 1, max(1, i - gapEnd))
    upper <- ifelse(i <= gapStart, 1, max(1, i - gapStart))
    alottedDose2 <- pmin(round(colSums(doseOneGiven[lower:upper, ])/(gapEnd - gapStart), digits = 0),
                         doseTwoRemaining[i, ])

    alottedDose1 <- units - sum(alottedDose2)

    # Calculation to distribute allotted dose one doses
    df1 <- data.frame(phase = seq(1, length(dist)),
                     priority = c(rep(1,4), rep(2,6), rep(3,4), 4, 5),
                     full = as.vector(doseOneRemaining[i, ] <= 0)
    )

    df2 <- df1 %>% filter(!full)

    df3 <- df2 %>% filter(priority == min(priority))

    df3$new <- pmin(round(alottedDose1*dist[df3$phase]/sum(dist[df3$phase]), digits = 0),
                    doseOneRemaining[i, df3$phase])

    distribution <- df1 %>%
        left_join(df3, by = 'phase') %>%
        mutate(new = ifelse(is.na(new), 0, new)) %>%
        select(new) %>%
        t()

    # Update data
    doseOneGiven[i+1, ] <- round(distribution)
    doseTwoGiven[i+1, ] <- alottedDose2
    doseOneRemaining[i+1, ] <- doseOneRemaining[i, ] - doseOneGiven[i+1, ]
    doseTwoRemaining[i+1, ] <- doseTwoRemaining[i, ] - doseTwoGiven[i+1, ]
    dosesGiven[i, ] <- doseOneGiven[i + 1, ] + doseTwoGiven[i + 1, ]
    dosesRemaining[i, ] <- doseOneRemaining[i + 1, ] + doseTwoRemaining[i + 1, ]

    # increment i
    i <- i + 1
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
        cat("2",d1 == d2,"\n")
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


