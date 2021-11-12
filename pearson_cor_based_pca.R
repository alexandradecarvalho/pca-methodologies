library(NbClust)


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

# 2 - Pearson correlation = cov/sqrt(Var(a)*Var(b))
pearsons_cor_matrix = cor(input_matrix, method = c("pearson"))

# 3 - Calculate the correlation matrix's eigenvectors and eigenvalues (Eigenvalues = % variance captured)
ev = eigen(pearsons_cor_matrix)

eigen_vectors = ev$vectors
eigen_values = ev$values

print(eigen_values)

# 4 - Select the most important PCs: the number of PCs is at least 70% of cumulative percentage of total variation 
total_variance = sum(eigen_values)
threshold = total_variance*0.7

pcs = prcomp(input_matrix)
  

# calculate the sum of the eigenvalues until it is more than the threshold          
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
number_clusters = NbClust(new_dataset, method = "kmeans", max.nc= n-1, index = "ch")
cs = number_clusters$Best.nc[2]


# 7 - Apply k-means method to new data set
k = kmeans(new_dataset, cs)
