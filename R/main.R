
##' Main function of the voice gravy package
##'
##' Bit longer info
##'
##' the measures currently computed are:
##' \itemize{
##' \item F0
##' \item Formants F1-F4
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

  ####################################
  # TODO do computational magic here
  # and create a new matrix/data.frame/tibble
  # that contains the new values

  # dep. on result type
  # build return obj
  if(result_type == "tibble"){
    res = tibble::as.tibble(cbind(f0_vals$F0, formant_vals$fm)) # add newly gen matrix here
  } else if (result_type == "AsspDataObj"){
    # TODO
  }
  return(res)

}

# TODO
add_gravy_to_emuDB <- function(emuDBhandle){
  # TODO list add tracks and SSFF files to emuDB
  # by looping over every bundle returned by emuR::list_bundles()/emuR::list_files()
}
