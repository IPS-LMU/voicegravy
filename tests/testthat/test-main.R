test_that("voice_gravy works as expected", {
  # package has to be installed (CMD/CTRL + SHIFT + B in RStudio) for
  # the data to be available in this location
  path2data = system.file("extdata", package = "voicegravy")
  wavpath = file.path(path2data, "wav_files", "msajc003.wav")

  wrassp::read.AsspDataObj(wavpath)
  result = voice_gravy(wav_path = wavpath)
  # TODO evaluate result
  expect_equal(2 * 2, 4)
})
