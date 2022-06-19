#TODO
#1. Check reliability score range [0,1]
#2. Check if number of rows is the same as original data
#3. Try to use tapply() [tapply(test_object_fs_multi$fs_f1_se, test_object_fs_multi$school, sd)]
#4. Use different examples to test (e.g., more factors/more groups)


####################################### Test get_fscore function ######################################

# Prepare test objects

myModel <- '
   # latent variables
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + y2 + y3 + y4
   # regressions
     dem60 ~ ind60
'
model = myModel
# model = cfa_3var
data = PoliticalDemocracy
test_object_fs <- get_fs(PoliticalDemocracy, model)
fit <- sqrt(length(unlist(lavInspect(cfa(myModel, PoliticalDemocracy))["psi"])))

########## Testing section ############

# Class of input

test_that("test the model input", {
  expect_type(myModel, "character")
})

test_that("test the data input", {
  expect_s3_class(PoliticalDemocracy, "data.frame")
})

# Class of output

test_that("Test that the function gives an output of data frame", {
  expect_s3_class(test_object_fs, "data.frame")
})

test_that("Test the number of factors is equal", {
  expect_equal(length(test_object_fs), sqrt(length(unlist(lavInspect(cfa(model = myModel, data = PoliticalDemocracy))["psi"])))*3)
})

test_that("Test that the number of rows is the same as the original data", {
  expect_identical(nrow(test_object_fs), nrow(PoliticalDemocracy))
})

# Test reliability scores

test_that("Test that reliability scores for each observation are the same", {
  Names <- colnames(test_object_fs)
  for (j in 1:length(Names[grepl("_rel", Names)])) {
    for(i in 1:length(test_object_fs[Names[grepl("_rel", Names)][j]])) {
      expect_identical(test_object_fs[Names[grepl("_rel", Names)][j]][i], test_object_fs[Names[grepl("_rel", Names)][j]][1])
    }
  }
})

test_that("Test the range of reliability scores", {
  Names <- colnames(test_object_fs)
  for (j in 1:length(Names[grepl("_rel", Names)])) {
    for(i in 1:length(test_object_fs[Names[grepl("_rel", Names)][j]])) {
      expect_gte(test_object_fs[Names[grepl("_rel", Names)][j]][i,], 0)
      expect_lte(test_object_fs[Names[grepl("_rel", Names)][j]][i,], 1)
    }
  }
})

# Test standard error

test_that("Test that standard errors for each observation are the same", {
  Names <- colnames(test_object_fs)
  for (j in 1:length(Names[grepl("_se", Names)])) {
    for(i in 1:length(test_object_fs[Names[grepl("_se", Names)][j]])) {
      expect_identical(test_object_fs[Names[grepl("_se", Names)][j]][i], test_object_fs[Names[grepl("_se", Names)][j]][1])
    }
  }
})

##### Test the multi-group example #####

hs_model <- 'visual  =~ x1 + x2 + x3'
multi_fit <- cfa(hs_model,
                 data = HolzingerSwineford1939,
                 group = "school")
get_fs(HolzingerSwineford1939, hs_model, group = "school")
# Or without the model
test_object_fs_multi <-  get_fs(HolzingerSwineford1939[c("school", "x4", "x5", "x6")],
                                group = "school")

# Class of output

test_that("Test the number of factors is equal", {
  expect_equal(length(test_object_fs_multi),
               (sqrt(length(unlist(lavInspect(cfa(model = hs_model, data = HolzingerSwineford1939))["psi"])))*3 + 1))
})

test_that("Test that the number of rows is the same as the original data", {
  expect_identical(nrow(test_object_fs_multi), nrow(HolzingerSwineford1939))
})

# Test standard error

# test_that("Test that standard errors for each observation are the same within groups", {
#   Names <- colnames(test_object_fs_multi)
#   subsets <- array()
#   # if (is.numeric(test_object_fs_multi[length(test_object_fs_multi)]) == "FALSE") {
#     level_names <- levels(factor(t(test_object_fs_multi[length(test_object_fs_multi)])))
#   # }
#
#   for (n in 1:length(level_names)) {
#     subsets[n] <- list(test_object_fs_multi[test_object_fs_multi[length(test_object_fs_multi)] == level_names[n], ])
#   }
#
#   for (s in 1:length(subsets)) {
#       for (j in 1:length(Names[grepl("_se", Names)])) {
#         for(i in 1:length(as.data.frame(subsets[1])[Names[grepl("_se", Names)][j]])) {
#           expect_identical(as.data.frame(subsets[1])[Names[grepl("_se", Names)][j]][i],
#                            as.data.frame(subsets[1])[Names[grepl("_se", Names)][j]][1])
#         }
#       }
#     }
#   }
# )

test_that("Test that standard errors for each observation are the same within groups", {
  test_se <- tapply(test_object_fs_multi$fs_f1_se, test_object_fs_multi$school, var)
  for (i in length(test_se)) {
    expect_identical(unname(test_se[i]), 0)
  }
})

# Test reliability scores
#
# test_that("Test that reliability scores for each observation are the same within groups", {
#   Names <- colnames(test_object_fs_multi)
#   subsets <- array()
#
#   # If there is a group variable (last column), extract the level names
#   #if (is.numeric(test_object_fs_multi[length(test_object_fs_multi)]) == "FALSE") {
#     level_names <- levels(factor(t(test_object_fs_multi[length(test_object_fs_multi)])))
#   #}
#
#   # Subset data
#   for (n in 1:length(level_names)) {
#     subsets[n] <- list(test_object_fs_multi[test_object_fs_multi[length(test_object_fs_multi)] == level_names[n], ])
#   }
#
#   # Within each group data, test what we want to test
#   for (s in 1:length(subsets)) {
#     for (j in 1:length(Names[grepl("_rel", Names)])) {
#       for(i in 1:length(as.data.frame(subsets[1])[Names[grepl("_rel", Names)][j]])) {
#         expect_identical(as.data.frame(subsets[1])[Names[grepl("_rel", Names)][j]][i],
#                          as.data.frame(subsets[1])[Names[grepl("_rel", Names)][j]][1])
#       }
#     }
#   }
# }
# )

test_that("Test that standard errors for each observation are the same within groups", {
  test_rel <- tapply(test_object_fs_multi$fs_f1_rel, test_object_fs_multi$school, var)
  for (i in length(test_rel)) {
    expect_identical(unname(test_rel[i]), 0)
  }
})
