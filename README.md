https://github.com/tu-usuario-de-github/nombre-del-proyecto
# Carga de librerías
library(ggplot2)
library(cluster)

# 1. Carga y Preparación (Peso: 30)
dataset <- read.csv("Mall_Customers.csv")
colnames(dataset) <- c("CustomerID", "Gender", "Age", "Annual_Income", "Spending_Score")

# Codificar Género (1: Male, 0: Female) y Normalizar
dataset$Gender <- ifelse(dataset$Gender == "Male", 1, 0)
dataset_scaled <- scale(dataset[, c("Age", "Annual_Income", "Spending_Score")])

# 2. Modelos de Clustering (Peso: 30)
set.seed(123)
# K-Means con Método del Codo
wss <- sapply(1:10, function(k) kmeans(dataset_scaled, centers = k, nstart = 25)$tot.withinss)
plot(1:10, wss, type="b", main="Método del Codo") # El codo está en k=5

kmeans_model <- kmeans(dataset_scaled, centers = 5, nstart = 25)
dataset$Cluster_KMeans <- kmeans_model$cluster

# Clustering Jerárquico (ward.D y euclídea)
dist_matrix <- dist(dataset_scaled, method = "euclidean")
h_model <- hclust(dist_matrix, method = "ward.D")
plot(h_model, main="Dendrograma (Método Ward.D)")
dataset$Cluster_Hierarchical <- cutree(h_model, k = 5)

# 3. Evaluación con Silueta (Peso: 20)
sil_km <- mean(silhouette(dataset$Cluster_KMeans, dist_matrix)[, 3])
sil_hj <- mean(silhouette(dataset$Cluster_Hierarchical, dist_matrix)[, 3])

cat("Promedio Silueta K-Means:", sil_km, "\n")
cat("Promedio Silueta Jerárquico:", sil_hj, "\n")

# 4. Interpretación y Visualización (Peso: 20)
# Resumen descriptivo por cluster
resumen <- aggregate(dataset[, 3:5], by=list(Cluster=dataset$Cluster_KMeans), mean)
print("Perfiles de los segmentos:")
print(resumen)

# Visualización
ggplot(dataset, aes(x=Annual_Income, y=Spending_Score, color=as.factor(Cluster_KMeans))) +
  geom_point(size=3) + 
  labs(title="Segmentación de Clientes: Ingresos vs Gasto", color="Cluster") +
  theme_minimal()
