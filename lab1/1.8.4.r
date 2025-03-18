vec3 <- seq(3, 27, by = 3)

values_2_5_7 <- vec3[c(2, 5, 7)]
penultimate_value <- vec3[length(vec3) - 1]
without_penultimate <- vec3[-(length(vec3) - 1)]
without_sixth <- vec3[-6]
hundredth_value <- vec3[100]
without_first_last <- vec3[-c(1, length(vec3))]
between_4_10 <- vec3[vec3 > 4 & vec3 < 10]
less_4_or_more_10 <- vec3[vec3 < 4 | vec3 > 10]

print(values_2_5_7)
print(penultimate_value)
print(without_penultimate)
print(without_sixth)
print(hundredth_value)
print(without_first_last)
print(between_4_10)
print(less_4_or_more_10)
