setwd("./KMUTT")

dat <- read.table("agaricus-lepiota.data.txt", sep = ",", stringsAsFactors = FALSE, na.strings = "?")

names(dat) <- c("classes","cap-shape","cap-surface","cap-color","bruises?",
                "odor","gill-attachment","gill-spacing","gill-size","gill-color",
                "stalk-shape","stalk-root","stalk-surface-above-ring",
                "stalk-surface-below-ring","stalk-color-above-ring",
                "stalk-color-below-ring","veil-type","veil-color","ring-number",
                "ring-type","spore-print-color","population","habitat")

source("Entropy.R")

entro <- RankEntro(dat, "classes", "e")

dat2 <- split(dat, dat$odor)

choice <- names(dat2)

lapply(dat2, function(x) table(x$classes))

entro2 <- RankEntro(dat2$n, "classes", "e")

induc2 <- dat2$n

# debug(RankEntro)
entro2 <- RankEntro(induc2, "classes", "e")

dat3 <- split(induc2, induc2$`spore-print-color`)

names(dat3)

RankEntro(dat3$w, "classes", "e")

