install.packages("readxl")
library(readxl)
getwd()
grep("xlsx$", list.files(), value = TRUE)

s <- read_excel("Survey.xlsx", sheet = 5)

r <- s[, grepl("rank", names(s))]

rm(s)

df <- data.frame(A = NA, B = NA, C = NA, 
                 D = NA, E = NA, F = NA)

for(i in 1:nrow(r)) {
  v <- c(rep(0, 6))
  for(j in 1:3){
    c <- as.character(r[i,j])
    switch(j, 
           {
             idx <- which(names(df) ==  c)
             v[idx] <- v[idx] + 1
           },
           {
             idx <- which(names(df) ==  c)
             v[idx] <- v[idx] + 0.5
           },
           {
             idx <- which(names(df) ==  c)
             v[idx] <- v[idx] + 0.33333
           })
  }
  df <- rbind(df, v)
}

d <- df[-1,]

adj <- 0.205555556

d[d$A == 0, 'A'] <- adj
d[d$B == 0, 'B'] <- adj
d[d$C == 0, 'C'] <- adj
d[d$D == 0, 'D'] <- adj
d[d$E == 0, 'E'] <- adj
d[d$F == 0, 'F'] <- adj

r <- sapply(d, mean)
r.df <- data.frame(t(r))
r.df <- r.df[, order(r, decreasing = TRUE)]
