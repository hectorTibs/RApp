

# Eighth code snippet
installations <- read.csv("C:\\Users\\V Tibs\\Desktop\\Recom.csv")
head(installations)
names(installations) <- c("Package", "User", "installed")

# Ninth code snippet
library('reshape')

user.package.matrix <- cast(installations, User ~ Package, value = 'installed')

user.package.matrix[, 1]


user.package.matrix[, 2]


row.names(user.package.matrix) <- user.package.matrix[, 1]

user.package.matrix <- user.package.matrix[, -1]

# Correlacion entre los usuarios y lo instalados
similarities <- cor(user.package.matrix)

# numro de filas  y columnas para realizar la corrlacion
nrow(similarities)
ncol(similarities)

similarities[1, 1]
#[1] 1
similarities[1, 2]
#[1] -0.04822428

# Se obtiene  la distancia entre cada producto
distances <- -log((similarities / 2) + 0.5)


# Twelfth code snippet
k.nearest.neighbors <- function(i, distances, k = 25)
{
  return(order(distances[i, ])[2:(k + 1)])
}



# Thirteenth code snippet
installation.probability <- function(user, package, user.package.matrix, distances, k = 25)
{
  neighbors <- k.nearest.neighbors(package, distances, k = k)
  
  return(mean(sapply(neighbors, function (neighbor) {user.package.matrix[user, neighbor]})))
}

installation.probability(1, 1, user.package.matrix, distances)
#[1] 0.76


# Fourteenth code snippet
most.probable.packages <- function(user, user.package.matrix, distances, k = 25)
{
  return(order(sapply(1:ncol(user.package.matrix),
                      function (package)
                      {
                        installation.probability(user,
                                                 package,
                                                 user.package.matrix,
                                                 distances,
                                                 k = k)
                      }),
               decreasing = TRUE))
}

user <- 1

listing <- most.probable.packages(user, user.package.matrix, distances)

colnames(user.package.matrix)[listing[1:5]]
