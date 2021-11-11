library(NbClust)
library(biwt)
library(ie2misc)

## script to generate random vars from a pareto distribution (GPD)
## it works following the equation seen on the wikipedia article
## https://en.wikipedia.org/wiki/Generalized_Pareto_distribution#Generating_generalized_Pareto_random_variables
## it is based on a uniformly distributed function whose elements 
## are translated to generated values from a uniformly distributed function
## the parameters 'location', 'scale', 'shape' are used to model 
## the GPD 
rGPD = function(location, scale, shape, rows, cols){
    # create the random uniform distribution
    U = runif(rows*cols, min=.000001, max = 1)

    # calculate the GPD
    X = location + (( scale * ( U**(-shape) - 1) ) / shape)

    # round the values to 1 decimal digit
    X = round(X, digits=1)

    # put it in a matrix
    M = matrix(X, nrow=rows, ncol=cols)

    # return the matrix
    return(M)

}

# 1 - building the input matrix
input_matrix = rGPD(104.8, 54.7, 0.2, 250, 15)

# 2 - Standardize the observation with median and mean absolute deviation (MAD)
standardized = madstat(input_matrix)

# 3 - Set the breakdown point
breakdown_point = 0.4  
  
# 4 - Calculate Tuckey's biweight correlation matrix
tuckeys_biweight_cor_matrix = biwt.cor(standardized, r=breakdown_point)

# 5 - Calculate the correlation matrix's eigenvectors and eigenvalues (Eigenvalues = % variance captured)
ev = eigen(tuckeys_biweight_cor_matrix)

eigen_vectors = ev$vectors
eigen_values = ev$values

print(eigen_values)

# 6 - Select the most important PCs: the number of PCs is at least 70% of cumulative percentage of total variation - 
total_variance = sum(eigen_values)
threshold = total_variance*0.7

n = 0
for (element in 1:length(eigen_values)){
  if (sum(eigen_values[1:element]) >= threshold){
    n = element
    break
  }
}

pcs = prcomp(standarized)

# 7 - Derive the new data set
new_dataset = pcs$rotation[,1:n]

# 8 - Calculate Calinski-Harabasz index in the new data set to determine the best number of cluster
number_clusters = NbClust(new_dataset, method = "kmeans", max.nc= n-1, index = "ch")
print(number_clusters$Best.nc)

# 9 - Apply k-means method to new data set
k = kmeans(new_dataset, number_clusters)
print(k$centers)

#TODO:plotting ??