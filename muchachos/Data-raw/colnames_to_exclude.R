

# On cherche a enlever les colonnes constantes
# On detecte les colonnes constantes dans le train
# Pour les sauvergarder dans R/sysdata.rda

file <- "Data/train.csv"
data <- read_csv(file, col_names = TRUE,
                 col_types = paste(c("c", rep("d", 4992)), collapse = ""))
cm <- mapply(function(x, y) x - y, data[, -c(1, 2)], colMeans(data[, -c(1, 2)]))
w <- which(colMeans(cm) == 0) # 261
cols_const_to_remove <- names(w)

save(cols_const_to_remove, file = "R/sysdata.rda")
