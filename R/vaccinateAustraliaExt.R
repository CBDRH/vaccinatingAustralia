# Vaccine allocation function
aus_allocation <- function(n, pop, days, openDay, hesitancyPct, propensity) {
    # n: number of available vaccines
    # pop: phase sizes on day 1
    # openDay: Day the vaccines open for each group
    # hesitancyPct: The proportion of each group falling into the four hesitancy categories (sums to 1)
    # propensity: The probability of vaccination in each hesitancy group

    phase <- c('Phase 1a', 'Phase 1b', 'Phase 2a', 'Phase 2b', 'Phase 3')

    n1 <- length(propensity) # e.g. 4 hesitancy categories
    n2 <- length(phase) # e.g. 5 phases

    startPop <- rep(pop, each = n1)*hesitancyPct

    # Doses administered on day 1 (all go to phase1 priority)
    doseOneGiven <- as.data.frame(t(rep(0, length(hesitancyPct))))
    doseTwoGiven <- as.data.frame(t(rep(0, length(hesitancyPct))))

    doseOneRemaining <- as.data.frame(t(startPop))
    doseTwoRemaining <- as.data.frame(t(startPop))

    for(t in 2:days){
        # A TRUE/FALSE vector indicating which groups are eligible based on the current day
        eligible <- t >= rep(openDay, each = n1)

        # The current unvaccinated population by group and phase
        popUnvaccinated <- startPop - colSums(doseOneGiven[1:t-1, ])

        # The number eligible on day t
        el_t <- eligible*popUnvaccinated

        # The number of unvaccinated population willing to be vaccinated on day t
        keen <- el_t*rep(propensity, n2)

        # Available doses allocated to second shot
        pLag <- 21 # Lag for 2nd Pfizer dose
        azLag <- 84 # Lag for 2nd AstraZeneca dose
        rows1a1b <- t(rep(0,8))
        rows2a <- t(rep(0,4))
        rows2b3 <- t(rep(0,8))

        if(t > pLag) {
            rows1a1b <- doseOneGiven[t-21, 1:8]
            rows2b3 <- doseOneGiven[t-21, 13:20]
        }

        if(t > azLag) {
            rows2a <- doseOneGiven[t-84, 9:12]
        }

        allocatedDose2 <- cbind(rows1a1b, rows2a, rows2b3)

        # The proportional distribution of available doses across keen groups
        doses <- min(n(t) - sum(allocatedDose2), sum(keen))

        allocatedDose1 <- round(pmin(doseOneRemaining[t-1, ], floor(doses*(keen/sum(keen)))))

        # Sometimes the number of people due their second dose exceeds the number of daily doses;
        # In which cse these people will just need to be vaccinated.
        if (doses < 0) {
            allocatedDose1 <- t(rep(0, length(hesitancyPct)))
        }

        doseOneGiven[t, ] <- allocatedDose1
        doseTwoGiven[t, ] <- allocatedDose2
        doseOneRemaining[t, ] <- doseOneRemaining[t-1, ] - doseOneGiven[t, ]
        doseTwoRemaining[t, ] <- doseTwoRemaining[t-1, ] - doseTwoGiven[t, ]

    }


    # Output data
    return(list(
            'doseOneGiven' = doseOneGiven,
            'doseTwoGiven' = doseTwoGiven,
            'doseOneRemaining' = doseOneRemaining,
            'doseTwoRemaining' = doseTwoRemaining,
            'dosesGiven' = doseOneGiven + doseTwoGiven,
            'dosesRemaining' = doseOneRemaining + doseTwoRemaining)
           )
}

