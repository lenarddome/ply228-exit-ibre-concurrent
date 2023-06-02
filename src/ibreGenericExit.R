#' ibreGenericExit runs a model simulation with given parameters.
#' @param params vector of parameters
#' @param tr training matrix compatible with state-list processors in catlearn
#' @param ind whether to return group-average or individual level
#' @return probability of responses for all test items, or if ind = TRUE return it for each ppt
ibreGenericExit <- function(params = c(2.87, 2.48, 4.42, 4.42, .222, 1.13), tr,
                            ind = FALSE) {

    ## Set state
    exemplars <- rbind(
        c(1, 1, 0, 1),
        c(1, 0, 1, 1)
    )

    w_exemplars <- exemplars
    w_exemplars[] <- 0
    nFeat <- 4
    nCat <- 2
    w_in_out <- matrix(0, nCat, nFeat)

    st <- list(
        nFeat = nFeat, nCat = nCat,
        phi = params[3], c = params[1], P = params[2],
        l_gain = params[4], l_weight = params[5], l_ex = params[6],
        sigma = c(rep(1, 3), params[7]),
        iterations = 10,
        exemplars = exemplars,
        w_exemplars = w_exemplars,
        w_in_out = w_in_out
    )

    ## Run simulation
    tout <- slpEXIT(st, data.matrix(tr), xtdo = FALSE)
    predics <- tout$p
    colnames(predics) <- c("common", "rare")
    predics <- data.table(cbind(tr[, 1:4], predics))
    predics <- melt(predics[ctrl == 2],
        measure.vars = c("common", "rare"),
        variable.name = "resp"
    )
    predics[, mean(value), by = .(stim, resp)]
    if (ind == TRUE) {
        out <- predics[, c("ppt", "stim", "resp", "value")][order(ppt, stim, resp)]
    } else {
        out <- predics[, .(prob = mean(value)), by = .(stim, resp)]
    }
    return(out[order(stim, resp)])
}
