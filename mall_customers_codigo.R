# Paquetes necesarios
library(ggplot2)
library(cluster)
library(dendextend)

# 1. Carga de datos
# Cambia el path si necesitas
dataset <- read.csv("Mall_Customers.csv")

# 2. Exploración de datos
str(dataset)
summary(dataset)

# Renombrar columnas para facilitar el manejo
colnames(dataset) <- c("CustomerID", "Gender", "Age", "Annual_Income", "Spending_Score")

# 3. Limpieza de datos
# Codificar 'Gender' como variable numérica
dataset$Gender <- ifelse(dataset$Gender == "Male", 1, 0)

# Normalización de variables numéricas (Ingreso y Puntuación)
dataset_scaled <- scale(dataset[, c("Annual_Income", "Spending_Score")])

# 4. Clustering con K-Means
set.seed(123)
# Determinar el número óptimo de clusters usando el método del codo
wss <- sapply(1:10, function(k) {
  kmeans(dataset_scaled, centers = k, nstart = 25)$tot.withinss
})

# Gráfico del método del codo
plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Número de Clusters", ylab = "Suma de cuadrados dentro de los clusters")

# Aplicar K-Means con 5 clusters (por ejemplo)
kmeans_model <- kmeans(dataset_scaled, centers = 5, nstart = 25)

# Agregar resultados al dataset original
dataset$Cluster_KMeans <- kmeans_model$cluster

# 5. Clustering jerárquico
# Calcular distancias y aplicar método jerárquico
distance_matrix <- dist(dataset_scaled, method = "euclidean")
hclust_model <- hclust(distance_matrix, method = "ward.D")

# Dendrograma
dend <- as.dendrogram(hclust_model)
plot(dend, main = "Dendrograma")

# Cortar el dendrograma en 5 clusters
dataset$Cluster_Hierarchical <- cutree(hclust_model, k = 5)

# 6. Evaluación de los modelos
# Calcular la métrica de silueta para K-Means
silhouette_kmeans <- silhouette(kmeans_model$cluster, dist(dataset_scaled))
avg_silhouette_kmeans <- mean(silhouette_kmeans[, 3]) # Promedio de la silueta

# Calcular la métrica de silueta para Clustering jerárquico
silhouette_hierarchical <- silhouette(cutree(hclust_model, k = 5), dist(dataset_scaled))
avg_silhouette_hierarchical <- mean(silhouette_hierarchical[, 3]) # Promedio de la silueta

cat("Promedio de silueta K-Means:", avg_silhouette_kmeans, "\n")
cat("Promedio de silueta Jerárquico:", avg_silhouette_hierarchical, "\n")

# 7. Visualización de resultados
# Scatter plot para los clusters de K-Means
ggplot(dataset, aes(x = Annual_Income, y = Spending_Score, color = as.factor(Cluster_KMeans))) +
  geom_point(size = 3) +
  labs(title = "Clusters de K-Means", x = "Ingreso Anual", y = "Puntuación de Gasto") +
  theme_minimal()

# Scatter plot para los clusters jerárquicos
ggplot(dataset, aes(x = Annual_Income, y = Spending_Score, color = as.factor(Cluster_Hierarchical))) +
  geom_point(size = 3) +
  labs(title = "Clusters Jerárquicos", x = "Ingreso Anual", y = "Puntuación de Gasto") +
  theme_minimal()

# 8. Análisis descriptivo de los clusters
aggregate(dataset[, c("Age", "Annual_Income", "Spending_Score")],
          by = list(Cluster_KMeans = dataset$Cluster_KMeans),
          FUN = mean)
