library(NbClust)

# 1 - building the input matrix
mat1.data = c(96,122.5,81,95.5,89,116,79,180,63.5,116,73,79,73,83,133.5,89,105,104,81,66.5,81,104,72.5,
               126,270,167.5,65,92,61.5,131,145,91.5,138,95.5,153,139,80,198,123,90.5,126.5,92,121,94.5,60,60,
               75,215.5,140.5,118.5,92.5,87,69.5,122.5,96.5,77,88.5,84.6,135,90.1,256.6,82.9,81.6,101.2,103.8,121.5,80,79.5,68.9,
               62,81.5,243,220.5,78,95,68.5,60,103.5,73.5,64,61,78.5,127,75,83,87.5,72,155,133,70,61.5,71,
               189.5,75,97,73.5,82.5,60.5,65,83.5,60.5,65,72.5,150,134,72,89,61.5,175,233,83.5,71.5,83,123,135)
input_matrix = matrix(mat1.data,nrow=23,ncol=5)

# 2 - Pearson correlation = cov/sqrt(Var(a)*Var(b))
pearsons_cor_matrix = cor(input_matrix, method = c("pearson"))

# 3 - Calculate the correlation matrix’s eigenvectors and eigenvalues (Eigenvalues = % variance captured)
ev = eigen(pearsons_cor_matrix)

eigen_vectors = ev$vectors
eigen_values = ev$values

print(eigen_values)

# 4 - Select the most important PCs: the number of PCs is at least 70% of cumulative percentage of total variation 
total_variance = sum(eigen_values)
threshold = total_variance*0.7

pcs = prcomp(pearsons_cor_matrix)
             
n = 0
for (element in 1:length(eigen_values)){
  if (sum(eigen_values[1:element]) >= threshold){
    n = element
    break
  }
}

# 5 - Derive the new data set
new_dataset = pcs$rotation[,1:n]
print(new_dataset)

# 6 - Calculate Calinski-Harabasz index in the new data set to determine the best number of cluster
number_clusters = NbClust(new_dataset, method = "kmeans",index = "ch")
cs = number_clusters$Best.nc[2]


# 7 - Apply k-means method to new data set
k = kmeans(new_dataset, cs)
print(k)

#TODO: plotting ??