test_that("create_base_vectors fails with invalid input", {
  outdir <- withr::local_tempdir()

  expect_snapshot(
    snap_aoi(1, out_dir = fs::path(outdir, "snap")),
    error = TRUE
  )
})

test_that("create_base_vectors works with sf object", {
  skip_if_offline()
  skip_on_cran()

  outdir <- withr::local_tempdir()

  aoi_snapped <- make_test_aoi(outdir)

  out <- create_base_vectors(
    aoi_snapped,
    out_dir = outdir
  )

  expect_equal(outdir, out)
  expect_snapshot(fs::path_file(fs::dir_ls(outdir)))
})

test_that("create_base_vectors works with file", {
  skip_if_offline()
  skip_on_cran()

  outdir <- withr::local_tempdir()

  aoi_snapped <- make_test_aoi(outdir)

  out <- create_base_vectors(
    fs::path(outdir, "snap", "aoi_snapped.gpkg"),
    out_dir = outdir
  )

  expect_equal(outdir, out)
  expect_snapshot(fs::path_file(fs::dir_ls(outdir)))
})
