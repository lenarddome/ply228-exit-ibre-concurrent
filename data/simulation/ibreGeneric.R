ibreGeneric <- function(blocks = 5, subjs = 56, ctxt = FALSE, seed = 1) {
    set.seed(seed)

    ## note "x7"=bias cue, which is always on

    sr <- rbind( # A  B  C  X common rare
        c(1, 1, 0, 1, 1, 0),
        c(1, 1, 0, 1, 1, 0),
        c(1, 1, 0, 1, 1, 0),
        c(1, 0, 1, 1, 0, 1)
    )

    trainingitems <- data.frame(cbind("", sr))

    colnames(trainingitems) <- c(
        "stim", "x1", "x2", "x3", "x4", "t1", "t2"
    )

    trainingitems[, "stim"] <- c(rep("AB", 3), "AC")

    trainingitems <- rbind(trainingitems, trainingitems)

    testitems <- rbind( # A  B  C  X common rare
        c(1, 0, 0, 1, 0, 0),
        c(0, 1, 0, 1, 0, 0),
        c(0, 0, 1, 1, 0, 0),
        c(1, 1, 0, 1, 0, 0),
        c(1, 0, 1, 1, 0, 0),
        c(0, 1, 1, 1, 0, 0)
    )

    teststim <- c("A", "B", "C", "AB", "AC", "BC")

    bigtr <- NULL
    for (subj in 1:subjs) {
        tr <- NULL
        for (i in 1:blocks) {
            training_phase <- trainingitems[sample(nrow(trainingitems)), ]
            tr <- rbind(tr, cbind(0, subj, i, training_phase))
        }

        tr[1, 1] <- 1

        colnames(tr) <- c("ctrl", "ppt", "block", colnames(trainingitems))
        testrials <- data.frame(2, subj, blocks + 1, teststim, testitems)

        colnames(testrials) <- colnames(tr)
        traintrials <- nrow(tr)
        tr <- rbind(tr, testrials)
        tr[(traintrials + 1):(nrow(tr)), "ctrl"] <- 2
        bigtr <- rbind(bigtr, tr)
    }

    if (!ctxt) bigtr <- bigtr[, -7]
    return(bigtr)
}
