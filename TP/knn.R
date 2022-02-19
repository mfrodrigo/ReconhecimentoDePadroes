library(readr)

data <- read_csv("treino.csv")
y <- data[[41]]
x <- as.matrix(data[2:40])
data <- cbind(x,y)

# N <- 30
# trans <- prcomp(x)
# PC <- predict(trans, x)
# x <- PC[,1:N]
data <- data.frame(data)

knn.cross <- tune.knn(x = full.data, y = full.dir,
                      k = 1:20,
                      tunecontrol=tune.control(sampling = "cross"),
                      cross=10)



