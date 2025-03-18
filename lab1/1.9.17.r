vec <- sample(1:100, 80, replace = TRUE) 
print("Последовательность чисел:")
print(vec)

mean_value <- mean(vec)
print(paste("Среднее значение:", mean_value))

greater_than_mean <- vec[vec > mean_value]
print("Элементы больше среднего:")
print(greater_than_mean)

matrix_v <- matrix(vec, nrow = 8, ncol = 10)
print("Матрица 8x10:")
print(matrix_v)

even_columns <- matrix_v[, seq(2, ncol(matrix_v), by = 2)]
odd_columns <- matrix_v[, seq(1, ncol(matrix_v), by = 2)]

even_vector <- as.vector(even_columns)
odd_vector <- as.vector(odd_columns)

print("Вектор четных столбцов:")
print(even_vector)

print("Вектор нечетных столбцов:")
print(odd_vector)

