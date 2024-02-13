# 1. Test for Exact Match of Cities
install.packages("testthat")

library(testthat)
german_cities <- c("Berlin", "Hamburg", "Munich", "Cologne", "Frankfurt", "Rostock")

expected_cities <- c("Berlin", "Hamburg", "Munich", "Cologne", "Frankfurt", "Rostock")

test_that("Dataset contains exactly the expected cities", {
  actual_cities <- sort(unique(german_cities)) # Assuming 'german_cities' is your dataset variable
  expect_equal(actual_cities, sort(expected_cities))
})

# 2. Test for No Missing Cities

test_that("No cities are missing from the dataset", {
  actual_cities <- unique(german_cities)
  missing_cities <- setdiff(expected_cities, actual_cities)
  expect_equal(length(missing_cities), 0, info = paste("Missing cities:", paste(missing_cities, collapse = ", ")))
})

# 3. Test for No Duplicate Cities

test_that("There are no duplicate cities in the dataset", {
  duplicated_cities <- german_cities[duplicated(german_cities)]
  expect_equal(length(duplicated_cities), 0, info = paste("Duplicated cities found:", paste(unique(duplicated_cities), collapse = ", ")))
})