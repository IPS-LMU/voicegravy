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
  css_vals = wrassp::cssSpectrum(wav_path, toFile = F)

  # check if all have the same sample rate and start time
  if(!identical(attr(f0_vals, "sampleRate"), attr(formant_vals, "sampleRate"),attr(spectral_vals, "sampleRate"),attr(css_vals, "sampleRate")) ||
     !identical(attr(f0_vals, "startTime"), attr(formant_vals, "startTime"),attr(spectral_vals, "startTime"),attr(css_vals, "startTime"))){
    stop("sampleRate or startTime returned by ksvF0, forest, dftSpectrum, and cssSpectrum are not identical!")
  }

  # for cepstral alanysis: quefrencies below 1ms are ignored (Hillenbrand et al., 1994)
  N_ms = round(attr(f0_vals, "origFreq") / 1000)

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
                              F0 = f0_vals$F0[,1],
                              F1 = formant_vals$fm[,1],
                              F2 = formant_vals$fm[,2],
                              F3 = formant_vals$fm[,3],
                              F4 = formant_vals$fm[,4],
                              B1 = formant_vals$bw[,1],
                              B2 = formant_vals$bw[,2],
                              B3 = formant_vals$bw[,3],
                              B4 = formant_vals$bw[,4],
                              H1u = numeric(nrow(formant_vals$fm)),
                              H2u = numeric(nrow(formant_vals$fm)),
                              H4u = numeric(nrow(formant_vals$fm)),
                              A1u = numeric(nrow(formant_vals$fm)),
                              A2u = numeric(nrow(formant_vals$fm)),
                              A3u = numeric(nrow(formant_vals$fm)),
                              H1c = numeric(nrow(formant_vals$fm)),
                              H2c = numeric(nrow(formant_vals$fm)),
                              H4c = numeric(nrow(formant_vals$fm)),
                              A1c = numeric(nrow(formant_vals$fm)),
                              A2c = numeric(nrow(formant_vals$fm)),
                              A3c = numeric(nrow(formant_vals$fm)),
                              twoK = numeric(nrow(formant_vals$fm)),
                              fiveK = numeric(nrow(formant_vals$fm)),
                              CPP = numeric(nrow(formant_vals$fm))
                              )

  # loop through rows and fill up result_tbl
  # with missing vals
  for(i in 1:nrow(result_tbl)){

	# if F0 (and/or formants are undefined
	# better way to assign to multiple vars?
	#if all(result_tbl[i, c("F0", "F1", "F2") == 0]){
	if (result_tbl$F0[i] == 0){
		# JPK: make NA or 0? NA maybe better b/c spectral differences can be less than/equal to zero
		# brittle: need to change if number of values computed changes
		result_tbl[i, 10:23] <- NA
	} else {

		h1 = get_harmonics(i, result_tbl$F0[i]) 
		h2 = get_harmonics(i, 2*result_tbl$F0[i]) 
		h4 = get_harmonics(i, 4*result_tbl$F0[i]) 

		CPP = get_CPP(i)

   	 	####################################
	    # Update results

		result_tbl[i,]$H1u = h1
		result_tbl[i,]$H2u = h2
		result_tbl[i,]$H4u = h4
		result_tbl[i,]$CPP = CPP
	}

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

get_harmonics <- function (i, f_est){
  # find harmonic magnitudes in dB of time signal x around a frequency estimate f_est
  # df_range, optional, default +-df% of f_est
  # VS and PS use df = 0.1 but also have finer spectral resolution

  # search around frequency estimate in steps of df (in Hz)
  df = 0.15
  # search range (in Hz)
  df_range = round(f_est*df)

  f_min = f_est - df_range
  f_max = f_est + df_range

  # use freqs_vec to get indices of columns to extract
  col_indices = which(freqs_vec >= f_min & freqs_vec <= f_max)  
  spectral_vals = spectral_vals$dft[i, col_indices]
  # could get freqs too
  #spectral_freqs = freqs_vec[col_indices]
  #f = spectral_freqs[which.max(spectral_vals)]
  h = max(spectral_vals)
  return(h)

}

get_CPP <- function (i){

  # quefrency below 1ms are ignored as per Hillenbrand
  N_ms = round(attr(css_vals, "origFreq") / 1000)
  # this is a Hz value - want the *index* of freqs_vec whose value is greater than this
  my_range = which(freqs_vec >= N_ms)

  # there are several ways to do this - question is, how to constrain quefrency range:
  # between 1 ms and ... ? or between quefruency range corresponding to some reasonable F0 range?
  # first pass, we'll do it the dumb way, compare w/VS
  vals = css_vals$css[i, my_range]
  	
  # for doing the regression, does it matter if the independent variable is samples or ms? can't see why...
  basefit <- lm(vals ~ c(1:length(vals)))
  # find peak
  p = max(vals)
  #base_val = predict(basefit)[which(vals == max(vals))]
  base_val = (p * basefit$coefficients[2]) + basefit$coefficients[1]
  return(p - base_val)
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
