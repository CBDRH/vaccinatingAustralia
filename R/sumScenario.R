# Function to summarise projection outcomes

sumScenario <- function(scenario){

    dist <- c(70000, 100000, 318000, 190000, 1045000, 1858000, 953000, 87000, 2000000, 196000, 2650000, 3080000, 387000, 453000, 6643000, 5670000)

    dates <- scenario$dataOut %>%
        mutate(phaseX = substr(phase, 1, 2)) %>%
        group_by(phaseX) %>% filter(!finished) %>%
        summarise(date = max(date)) %>%
        mutate(date = format(date, '%d/%m/%y'))

    summary <- data.frame(

        n1 = format(sum(scenario$dataOut$n), big.mark = ','),
        n2 = format(sum(scenario$dataOut$n/2), big.mark = ','),
        pct = format(100*sum(scenario$dataOut$n/2)/sum(dist), nsmall = 1, digits=1),
        done1a = dates[[1, 2]],
        done1b = dates[[2, 2]],
        done2a = dates[[3, 2]],
        done2b = dates[[4, 2]],
        done3  = dates[[5, 2]]

    )

return(summary)

}



