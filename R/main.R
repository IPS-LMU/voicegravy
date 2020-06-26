##' Main function of the voice gravy package
##'
##' An attempt of reimplementation of http://www.phonetics.ucla.edu/voicesauce/
##' using R packages
##'
##' The measures currently computed are:
##' \itemize{
##' \item F0
##' \item Formants F1-F4
##' \item Formant bandwidths B1-B4
##' \item H1(*)
##' \item H2(*)
##' \item H4(*)
##' \item A1(*)
##' \item A2(*)
##' \item A3(*)
##' \item 2K(*)
##' \item 5K
##' \item H1(*)-H2(*)
##' \item H2(*)-H4(*)
##' \item H1(*)-A1(*)
##' \item H1(*)-A2(*)
##' \item H1(*)-A3(*)
##' \item H4(*)-2K(*)
##' \item 2K(*)-5K
##' \item Energy
##' \item Cepstral Peak Prominence
##' \item Harmonic to Noise Ratios
##' \item Subharmonic to Harmonic Ratio
##' \item Strength of Excitation
##' }
##' where (*) indicates that the harmonic/spectral amplitudes
##' are reported with and without corrects for formant frequencies and bandwidths.
##' @param wav_path path to wav file that is to be processed. Currently
##' only uncompressed mono .wav files are supported.
##' @param result_type specify type of returned object.
##' Current values are:
##' \itemize{
##' \item tibble (== default)
##' \item AsspDataObj (the main in-memory object of the wrassp package
##' which can be saved to disk using the \code{wrassp::write.AsspDataObj())}
##' function.
##' }
##' @return object of type specified with \code{result_type}
##' @export
voice_gravy <- function(wav_path,
                        result_type = "tibble") {
  # do params checks:
  allowed_result_types = c("tibble", "AsspDataObj")
  '%ni%' <- Negate('%in%')
  if(result_type %ni% allowed_result_types){
    stop("The currently supported result_type values are: ", allowed_result_types)
  }

  f0_vals <- wrassp::ksvF0(wav_path, toFile = F)
  formant_vals <- wrassp::forest(wav_path, toFile = F)
  spectral_vals <- wrassp::dftSpectrum(wav_path, toFile = F)

  # check if all have the same sample rate and start time
  if(!identical(attr(f0_vals, "sampleRate"), attr(formant_vals, "sampleRate"),attr(spectral_vals, "sampleRate")) ||
     !identical(attr(f0_vals, "startTime"), attr(formant_vals, "startTime"),attr(spectral_vals, "startTime"))){
    stop("sampleRate or startTime returned by ksvF0, forest and dftSpectrum are not identical!")
  }

  # calculate frame time vector
  frame_time = seq(attr(f0_vals, "startTime"),
                   by = 1/attr(f0_vals, "sampleRate"),
                   length.out = nrow(f0_vals$F0))

  nyquist_freq = attr(f0_vals, "origFreq") / 2
  freqs_vec = seq(0, nyquist_freq, length.out = ncol(spectral_vals$dft))

  # as we know the size of the resulting object
  # we can pre-allocate it and then fill it up
  # (expand as needed)
  result_tbl = tibble::tibble(frame_time = frame_time,
                              F1 = formant_vals$fm[1],
                              F2 = formant_vals$fm[2],
                              F3 = formant_vals$fm[3],
                              F4 = formant_vals$fm[4],
                              B1 = formant_vals$bw[1],
                              B2 = formant_vals$bw[2],
                              B3 = formant_vals$bw[3],
                              B4 = formant_vals$bw[4],
                              H1 = numeric(nrow(formant_vals$fm)),
                              H2 = numeric(nrow(formant_vals$fm)),
                              H4 = numeric(nrow(formant_vals$fm)),
                              A1 = numeric(nrow(formant_vals$fm)),
                              A2 = numeric(nrow(formant_vals$fm)),
                              A3 = numeric(nrow(formant_vals$fm)),
                              twoK = numeric(nrow(formant_vals$fm)),
                              fiveK = numeric(nrow(formant_vals$fm))
                              )

  # loop through rows and fill up result_tbl
  # with missing vals
  for(i in 1:nrow(result_tbl)){
    # just an example of filtering the freqs in spectral_vals$dft
    # based on a range around a formant value
    cur_f2 = result_tbl$F2[i]
    cur_f2_lower_limit = result_tbl$F2[i] - 0.25 * result_tbl$F2[i]
    cur_f2_upper_limit = result_tbl$F2[i] + 0.25 * result_tbl$F2[i]

    # use freqs_vec to get indices of columns to extract
    col_indices = which(freqs_vec >= cur_f2_lower_limit & freqs_vec <= cur_f2_upper_limit)

    cur_spectral_vals = spectral_vals$dft[i, col_indices]

    ####################################
    # TODO do computational magic here
    # and add to result_tbl in correct field
    # ala:

    # somehow calc val and add to field:
    H1 = max(cur_spectral_vals) # this is obviously BS!
    result_tbl[1,]$H1 = H1

  }


  # dep. on result type
  # build return obj
  if(result_type == "tibble"){
    res = result_tbl
  } else if (result_type == "AsspDataObj"){
    # TODO
    res = convert_resultToAsspDataObj(result_tbl,
                                      attr(f0_vals, "sampleRate"),
                                      attr(f0_vals, "startTime"))
  }
  return(res)

}

# TODO
convert_resultToAsspDataObj <- function (tbl,
                                         sampleRate,
                                         startTime){

  stop("not implemented yet")
}

# TODO
add_gravy_to_emuDB <- function(emuDBhandle){
  # TODO list add tracks and SSFF files to emuDB
  # by looping over every bundle returned by emuR::list_bundles()/emuR::list_files()
  stop("not implemented yet")
}
