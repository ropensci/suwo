options(verbose = TRUE)

df1 <- query_gbif(
  species = 'Phaethornis anthophilus',
  all_data = FALSE,
  format = "image"
)

## Xeno-Canto
test_that("Xenocanto Phaethornis anthophilus download all.data  = FALSE", {
  skip_on_cran()
  skip_if_offline()

  skip_if(is.null(df1))

  df2 <- df1[df1$key %in% c("5063794756", "5077057045"), ]
  sxc1 <- df2[!duplicated(df2$key), ][1:2, ]

  a <- download_media(metadata = sxc1, path = tempdir())

  skip_if(is.null(a))

  fls <- list.files(path = tempdir(), pattern = "jpeg$")

  # remove files
  unlink(file.path(tempdir(), fls))

  expected_files <- c(
    "Phaethornis_anthophilus-GBIF5063794756.jpeg",
    "Phaethornis_anthophilus-GBIF5077057045.jpeg"
  )

  # test
  expect_true(all(expected_files %in% fls))

  expect_true(all(a$downloaded_file_name %in% expected_files))

  expect_true(
    all(
      c("download_status", "downloaded_file_name", "file_size") %in% names(a)
    )
  )
})

test_that("remove rows when NAs in repo", {
  skip_on_cran()
  skip_if_offline()
  skip_if(is.null(df1))

  df2 <- df1[df1$key %in% c("5063794756", "5077057045"), ]
  sxc1 <- df2[!duplicated(df2$key), ][1:2, ]

  sxc1$key[1] <- sxc1$repository[1] <- sxc1$file_extension[1] <- NA

  # test
  expect_error(download_media(metadata = sxc1, path = tempdir(), pb = 2))
})


## Xeno-Canto
test_that("Xenocanto Phaethornis anthophilus download folder_by", {
  skip_on_cran()
  skip_if_offline()
  skip_if(is.null(df1))

  df2 <- df1[df1$key %in% c("5063794756", "5077057045"), ]
  sxc1 <- df2[!duplicated(df2$key), ][1:2, ]

  a <- download_media(metadata = sxc1, path = tempdir(), folder_by = "country")

  skip_if(is.null(a))

  fls <- list.files(path = tempdir(), pattern = "jpeg$", recursive = TRUE)

  # remove filess
  unlink(file.path(tempdir(), a$downloaded_file_name))

  expected_files <- c(
    "Panama/Phaethornis_anthophilus-GBIF5063794756.jpeg",
    "Colombia/Phaethornis_anthophilus-GBIF5077057045.jpeg"
  )

  # test
  expect_true(all(expected_files %in% fls))
  expect_true(all(a$downloaded_file_name %in% expected_files))
})

test_that("Xenocanto Phaethornis anthophilus download all.data  = TRUE", {
  skip_on_cran()
  skip_if_offline()
  skip_if(is.null(df1))

  df2 <- df1[df1$key %in% c("5063794756", "5077057045"), ]
  sxc1 <- df2[!duplicated(df2$key), ][1:2, ]

  a <- download_media(metadata = sxc1, path = tempdir())
  skip_if(is.null(a))

  fls <- list.files(path = tempdir(), pattern = "jpeg$")

  # remove files
  unlink(file.path(tempdir(), fls))

  expected_files <- c(
    "Phaethornis_anthophilus-GBIF5063794756.jpeg",
    "Phaethornis_anthophilus-GBIF5077057045.jpeg"
  )

  # test
  expect_true(all(expected_files %in% fls))

  expect_true(all(a$downloaded_file_name %in% expected_files))
})

## wikiaves

test_that("wikiaves Glaucis dohrnii sp download, all.data = TRUE", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not(interactive())

  wa1 <- query_wikiaves(
    species = 'Glaucis dohrnii',
    format = "sound",
    all_data = TRUE
  )

  skip_if(is.null(wa1))

  test_keys <- c("2286824", "4522545")

  swa1 <- subset(wa1, key %in% test_keys)

  a <- download_media(metadata = swa1, path = tempdir())
  skip_if(is.null(a))

  fls <- list.files(path = tempdir(), pattern = "mp3$")

  # remove files
  unlink(file.path(tempdir(), fls[1]))

  # expected files
  expected_files <- c(
    "Glaucis_dohrnii-WA2286824.mp3",
    "Glaucis_dohrnii-WA4522545.mp3"
  )

  # test
  expect_true(all(expected_files %in% fls))

  expect_true(all(a$downloaded_file_name %in% expected_files))
})

test_that("wikiaves Urubitinga solitaria sp download image all.data = FALSE", {
  skip_on_cran()
  skip_if_offline()

  wa2 <- query_wikiaves(
    species = 'Urubitinga solitaria',
    format = "image",
    all_data = FALSE
  )

  skip_if(is.null(wa2))

  test_keys <- c("3227223", "5415237")

  swa2 <- subset(wa2, key %in% test_keys)

  a <- download_media(metadata = swa2, path = tempdir())

  skip_if(is.null(a))

  fls <- list.files(path = tempdir(), pattern = "jpeg$")

  # remove files
  unlink(file.path(tempdir(), fls))

  # expected files
  expected_files <- c(
    "Urubitinga_solitaria-WA3227223.jpeg",
    "Urubitinga_solitaria-WA5415237.jpeg"
  )

  # test
  expect_true(all(expected_files %in% fls))

  expect_true(all(a$downloaded_file_name %in% expected_files))
})

### GBIF
test_that("search GBIF sp download sound all_data = FALSE", {
  skip_on_cran()
  skip_if_offline()

  gb2 <- query_gbif(
    species = 'Glaucis dohrnii',
    format = "sound",
    all_data = FALSE
  )

  skip_if(is.null(gb2))

  test_keys <- c("3863342525", "2243728561")

  sgb2 <- subset(gb2, key %in% test_keys)

  a <- download_media(metadata = sgb2, path = tempdir())

  skip_if(is.null(a))

  fls <- list.files(path = tempdir(), pattern = "mp3$")

  # remove files
  unlink(file.path(tempdir(), fls))

  # expected files
  expected_files <- c(
    "Glaucis_dohrnii-GBIF3863342525.mp3",
    "Glaucis_dohrnii-GBIF2243728561.mp3"
  )

  # test
  expect_true(all(expected_files %in% fls))

  expect_true(all(a$downloaded_file_name %in% expected_files))
})

## iNaturalist

test_that("search inaturalist sp download all_data = TRUE", {
  skip_on_cran()
  skip_if_offline()

  in1 <- query_inaturalist(
    species = 'Agalychnis lemur',
    format = "image",
    all_data = TRUE
  )

  skip_if(is.null(in1))

  test_keys <- c("303641298", "303641290")

  sin1 <- subset(in1, key %in% test_keys)

  sin1 <- sin1[!duplicated(sin1$key), ]

  unlink(list.files(path = tempdir(), pattern = ".jpeg$", ignore.case = TRUE))

  a <- download_media(metadata = sin1, path = tempdir())

  skip_if(is.null(a))

  # expected files
  exp_files <- c(
    "Agalychnis_lemur-INAT303641290.jpeg",
    "Agalychnis_lemur-INAT303641298.jpeg"
  )

  fls <- list.files(path = tempdir(), pattern = ".jpeg$", ignore.case = TRUE)

  expect_true(all(exp_files %in% fls))
  expect_true(all(a$downloaded_file_name %in% exp_files))

  # remove files
  unlink(file.path(tempdir(), fls[2]))

  sin1$file_url[1] <- "asdasd"

  a <- download_media(metadata = sin1, path = tempdir())

  skip_if(is.null(a))

  expect_length(unique(a$download_status), 2)

  fls <- list.files(path = tempdir(), pattern = ".jpeg$", ignore.case = TRUE)

  # remove files
  unlink(file.path(tempdir(), fls))
})


test_that("search inaturalist sp download sound all_data = FALSE", {
  skip_on_cran()
  skip_if_offline()

  in2 <- query_inaturalist(
    species = "Rattus rattus",
    format = "sound",
    all_data = FALSE
  )

  skip_if(is.null(in2))

  test_keys <- c("283643216", "281653293")

  sin2 <- subset(in2, key %in% test_keys)

  unlink(list.files(
    path = tempdir(),
    pattern = ".mp3$|m4a$",
    ignore.case = TRUE
  ))

  a <- download_media(
    metadata = sin2,
    path = tempdir(),
    folder_by = "file_extension"
  )

  skip_if(is.null(a))

  fls <- list.files(
    path = tempdir(),
    pattern = ".mp3$|m4a$",
    ignore.case = TRUE,
    recursive = TRUE
  )

  # remove files
  unlink(file.path(tempdir(), fls))

  expected_files <- c(
    "Rattus_rattus-INAT281653293.m4a",
    "Rattus_rattus-INAT283643216.mp3"
  )

  expect_true(all(expected_files %in% basename(fls)))
  expect_true(all(a$downloaded_file_name %in% fls))
})


##  Macaulay
test_that("search macaulay sp download all_data = TRUE", {
  skip_if_not(interactive())
  skip_on_cran()
  skip_if_offline()

  ml1 <- query_macaulay(
    species = 'Glaucis dohrnii',
    format = "sound",
    all_data = TRUE,
    path = tempdir()
  )

  test_keys <- c(627919111, 624733750)

  sml1 <- subset(ml1, key %in% test_keys)

  unlink(list.files(
    path = tempdir(),
    pattern = ".mp3$|m4a$",
    ignore.case = TRUE
  ))

  a <- download_media(metadata = sml1, path = tempdir())

  skip_if(is.null(a))

  fls <- list.files(
    path = tempdir(),
    pattern = ".mp3$|m4a$",
    ignore.case = TRUE
  )

  # remove files
  unlink(file.path(tempdir(), fls))

  expected_files <- c(
    "Glaucis_dohrnii-ML627919111.mp3",
    "Glaucis_dohrnii-ML624733750.mp3"
  )

  expect_true(all(expected_files %in% fls))
  expect_true(all(a$downloaded_file_name %in% fls))
})


test_that("search macaulay Harpia harpyja download sound all_data = FALSE", {
  skip_if_not(interactive())
  skip_on_cran()
  skip_if_offline()

  ml2 <- query_macaulay(
    species = 'Harpia harpyja',
    format = "image",
    all_data = FALSE,
    path = tempdir()
  )

  tab <- table(ml2$key)

  test_keys <- names(tab)[tab == 1][1:2]

  sml2 <- subset(ml2, key %in% test_keys)

  unlink(list.files(path = tempdir(), pattern = ".jpeg$", ignore.case = TRUE))

  a <- download_media(metadata = sml2, path = tempdir())

  skip_if(is.null(a))

  fls <- list.files(path = tempdir(), pattern = "jpeg$", ignore.case = TRUE)

  # remove files
  unlink(file.path(tempdir(), fls))

  expected_files <- paste("Harpia_harpyja-ML", test_keys, ".jpeg", sep = "")

  expect_true(all(expected_files %in% fls))
  expect_true(all(a$downloaded_file_name %in% fls))
})


test_that("search macaulay sp download video all_data = FALSE", {
  skip_if_not(interactive())
  skip_on_cran()
  skip_if_offline()

  ml3 <- query_macaulay(
    species = 'Calypte anna',
    format = "video",
    all_data = FALSE,
    path = tempdir()
  )

  test_keys <- c("639261756", "639255345")

  sml3 <- subset(ml3, key %in% test_keys)

  unlink(list.files(path = tempdir(), pattern = ".mp4$", ignore.case = TRUE))

  a <- download_media(metadata = sml3, path = tempdir())

  skip_if(is.null(a))

  fls <- list.files(path = tempdir(), pattern = ".mp4$", ignore.case = TRUE)

  # remove files
  unlink(file.path(tempdir(), fls))

  expected_files <- c(
    "Calypte_anna-ML639261756.mp4",
    "Calypte_anna-ML639255345.mp4"
  )

  expect_true(all(expected_files %in% fls))
  expect_true(all(a$downloaded_file_name %in% fls))
})

test_that("overwrite and already there", {
  skip_on_cran()
  skip_if_offline()

  df2 <- suwo:::testing_metadata$h_sarapiquensis[1:4, ]

  dir.create(file.path(tempdir(), "downloads"))

  a <- download_media(
    metadata = df2,
    path = file.path(tempdir(), "downloads"),
    cores = 2,
    overwrite = T
  )
  skip_if(is.null(a))

  fls <- list.files(
    path = file.path(tempdir(), "downloads"),
    pattern = ".jpeg$",
    ignore.case = TRUE
  )

  # remove files
  unlink(file.path(tempdir(), "downloads", fls[1:2]))

  df2$file_url[4] <- "adasd"
  a <- download_media(metadata = df2, path = file.path(tempdir(), "downloads"))

  skip_if(is.null(a))

  expect_length(unique(a$download_status), 3)

  df2$file_url[4] <- "adasd"
  a <- download_media(
    metadata = df2,
    path = file.path(tempdir(), "downloads"),
    overwrite = TRUE
  )
  skip_if(is.null(a))

  expect_length(unique(a$download_status), 2)

  unlink(file.path(tempdir(), "downloads"), recursive = TRUE)
})
