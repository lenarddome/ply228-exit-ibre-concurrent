message(paste(format(Sys.time(), "%Y. %X"), ': loading/installing packages'))
## load all packages or install them if missing
packages <- c("data.table",
              "BayesFactor",
              "ggplot2",
              "ggthemes",
              "knitr",
              "kableExtra",
              "catlearn",
              "forcats",
              "papaja",
              "doMC",
              "effectsize",
              "cowplot",
              "wordcountaddin")

## Now load or install&load all
## User will need some instruction.
package.check <- lapply(
    packages,
    FUN = function(x) {
      if (!(x %in% (pak::lib_status()$package))) {
        pak::pkg_install(x, dependencies = TRUE, ask = FALSE)
        suppressMessages(library(x, character.only = TRUE))
      } else {
        suppressMessages(library(x, character.only = TRUE))
      }
    }
)

message(paste(format(Sys.time(), "%Y. %X"), ': rerun simulations with EXIT'))

source("simulation/ibreGeneric.R")     ## a training matrix for the model
source("simulation/ibreGenericExit.R") ## a simulation
load("simulation/simulation.RData")    ## simulation data (parameters, etc.)

parameters <- out$optim$bestmem
tr <- ibreGeneric(blocks = 5, subjs = 100, ctxt = TRUE, seed = parameters[8])

tests <- cbind(c(parameters[4], parameters[4]/2),
               c(parameters[6], parameters[6]/3))

exitplore <- data.table()
for (i in seq_len(nrow(tests))) {
  params <- parameters
  params[4] <- tests[i, 1]
  params[6] <- tests[i, 2]
  out <- ibreGenericExit(params, tr, ind = TRUE)
  exitplore <- rbind(exitplore, cbind(i, data.table(tests[i, ]), out))
}

pal <- calc_pal()(2)

exitplore[, stim := as.factor(stim)]
exitplore[, condition := as.factor(V1)]
levels(exitplore$condition)  <- c("concurrent", "control", "concurrent", "control")

exit00plot <- ggplot(exitplore[resp == 'rare'],
       aes(x = condition, y = value)) +
  # geom_jitter(size = 1,  alpha = 0.5) +
  stat_summary(aes(color = condition), fun = median, linewidth = 1, size = 1) +
  facet_wrap(stim ~ ., nrow = 1) +
  scale_colour_calc(name = "Condition") +
  scale_fill_calc(name = "Condition") +
  xlab("") +
  ylab("P(rare)") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black",
                                    fill = NA,
                                    size = 1),
        text = element_text(size = 16),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.text.x = element_text(size = 10),
        legend.position = "none",
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 20)))
