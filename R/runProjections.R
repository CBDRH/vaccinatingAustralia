# Purpose:  Run projections under the eight specified scenarios

# Load the function
source('R/vaccinateAustralia.R')

# Phases based on Australian government rollout strategy
dist <- c(70000, 100000, 318000, 190000, 1045000, 1858000, 953000, 87000, 2000000, 196000, 2650000, 3080000, 387000, 453000, 6643000, 5670000)
phase <- c('1a1', '1a2', '1a3', '1a4', '1b1', '1b2', '1b3', '1b4', '1b5', '1b6', '2a1', '2a2', '2a3', '2a4', '2b', '3')


# Set a fixed start date
day1 <- as.Date("2021/02/15")

# Define two hesitancy vectors
h7 <- 0.07
h13 <- 0.13
hesitancy7 <- c(0, 0, 0, 0,
               h7, h7, 0, h7, 0, 0,
               h7, h7, h7, h7,
               h7,
               h7)

hesitancy13 <- c(0, 0, 0, 0,
                h13, h13, 0, h13, 0, 0,
                h13, h13, h13, h13,
                h13,
                h13)

# Scenario 1
sc1 <- vaccinateAustralia(
        units = 80000,
        startDate = day1,
        gapStart = 3*7,
        gapEnd = 6*7,
        hesitancy = hesitancy7
)
save(sc1, file = 'Outputs/sc1.rda')
beepr::beep(7)


# Scenario 2
sc2 <- vaccinateAustralia(
    units = 80000,
    startDate = day1,
    gapStart = 3*7,
    gapEnd = 6*7,
    hesitancy = hesitancy13
)
save(sc2, file = 'Outputs/sc2.rda')
beepr::beep(7)


# Scenario 3
sc3 <- vaccinateAustralia(
    units = 80000,
    startDate = day1,
    gapStart = 3*7,
    gapEnd = 12*7,
    hesitancy = hesitancy7
)
save(sc3, file = 'Outputs/sc3.rda')
beepr::beep(7)


# Scenario 4
sc4 <- vaccinateAustralia(
    units = 80000,
    startDate = day1,
    gapStart = 3*7,
    gapEnd = 12*7,
    hesitancy = hesitancy13
)
save(sc4, file = 'Outputs/sc4.rda')
beepr::beep(7)


# Scenario 5
sc5 <- vaccinateAustralia(
    units = 60000,
    startDate = day1,
    gapStart = 3*7,
    gapEnd = 6*7,
    hesitancy = hesitancy7
)
save(sc5, file = 'Outputs/sc5.rda')
beepr::beep(7)


# Scenario 6
sc6 <- vaccinateAustralia(
    units = 60000,
    startDate = day1,
    gapStart = 3*7,
    gapEnd = 6*7,
    hesitancy = hesitancy13
)
save(sc6, file = 'Outputs/sc6.rda')
beepr::beep(7)


# Scenario 7
sc7 <- vaccinateAustralia(
    units = 60000,
    startDate = day1,
    gapStart = 3*7,
    gapEnd = 12*7,
    hesitancy = hesitancy7
)
save(sc7, file = 'Outputs/sc7.rda')
beepr::beep(7)


# Scenario 8
sc8 <- vaccinateAustralia(
    units = 60000,
    startDate = day1,
    gapStart = 3*7,
    gapEnd = 12*7,
    hesitancy = hesitancy13
)
save(sc8, file = 'Outputs/sc8.rda')


# Scenario 9
sc9 <- vaccinateAustralia(
    units = 160000,
    startDate = day1,
    gapStart = 3*7,
    gapEnd = 6*7,
    hesitancy = hesitancy7
)
save(sc9, file = 'Outputs/sc9.rda')
beepr::beep(7)

## Check results
check_results(sc1, units = 80000, gapEnd = 6*7, gapStart = 3*7)
check_results(sc2, units = 80000, gapEnd = 6*7, gapStart = 3*7)
check_results(sc3, units = 80000, gapEnd = 12*7, gapStart = 3*7)
check_results(sc4, units = 80000, gapEnd = 12*7, gapStart = 3*7)
check_results(sc5, units = 60000, gapEnd = 6*7, gapStart = 3*7)
check_results(sc6, units = 60000, gapEnd = 6*7, gapStart = 3*7)
check_results(sc7, units = 60000, gapEnd = 12*7, gapStart = 3*7)
check_results(sc8, units = 60000, gapEnd = 12*7, gapStart = 3*7)
check_results(sc9, units = 160000, gapEnd = 12*7, gapStart = 3*7)
beepr::beep(8)
