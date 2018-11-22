#' Twilio - Phone Number lookup
#'
#' This function uses the Twilio phone lookup API.
#' https://www.twilio.com/docs/lookup/api
#'
#'
#' @name twilio_lookup
#' @description Validate the phone number and return country codes and valid phone format.
#' @param phone character The phone number.  Required.
#' @param twilio_sid character The Twilio API SID. Looks for environment variable TWILIO_SID  Required..
#' @param twilio_token character The Twilio API token. Looks for environment variable TWILIO_TOKEN Required.
#' @return a list containing the response from the API
#' @keywords twilio
#' @export
#' @examples
#' twilio_lookup(phone='6178675309')
#'

twilio_lookup = function(phone=NA,
                         api_sid=Sys.getenv("TWILIO_SID"),
                         api_token=Sys.getenv("TWILIO_TOKEN")) {
  ## phone is required, fail if it is not provided
  if(is.na(phone)) {
    stop("the parameter phone is required.")
  }
  ## need to auth to the TextMagic API with name and pass
  ## if they arent there, yell
  if(nchar(api_sid)==0) {
    stop("Twilio SID is req'd")
  }
  if(nchar(api_token)==0) {
    stop("Twilio token is req'd")
  }
  ## the BASE url for the endpoint
  BASE = "https://lookups.twilio.com/v1/PhoneNumbers/%s"
  ## build the EP based on the inputs
  EP = sprintf(BASE, phone)
  ## call the API
  resp = httr::GET(EP, httr::authenticate(api_sid, api_token))
  ## if the status code <> 200, fail gracefully-ish
  if(httr::status_code(resp) != 200) {
    stop("the call the API was not happy and did not return a 200")
  }
  ## extract the content
  api = httr::content(resp)
  ## return the data
  return(api)
}

