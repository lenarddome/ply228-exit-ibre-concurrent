library(catlearn)
library(DEoptim)
library(data.table)
source("ibreGenericExit.R")
source("ibreGeneric.R")


## Set training array
## set human
expected <- fread("expected.csv")


minimize <- function(params) {
    train <- ibreGeneric(blocks = 5, subjs = 100, ctxt = TRUE, seed = params[8])
    observed <- data.table(ibreGenericExit(params, tr = train))
    sse <- ssecl(observed[order(stim, resp)]$prob, expected[order(stim, resp)]$prob)
    if (is.na(sse)) sse <- 99
    return(sse)
}


out <- DEoptim(minimize,
    lower = c(.000001, 1, 1, .000000, .0000001, .0000001, .000001, 1),
    upper = c(50, 50, 50, 50, 1, 50, 1, 100000),
    DEoptim.control(itermax = 1000, strategy = 6, parallelType = 'foreach', p = 0.5, c = 0.75)
)

save(out, file = 'simulation.RData')

parameters <- out$optim$bestmem
parameters_no_attention <- parameters
parameters_no_attention[c(4, 6)] <- c(0, 0)
ibreGenericExit(parameters, train)
ibreGenericExit(parameters_no_attention, train)
