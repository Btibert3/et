#' TextMagic - Single Carrier Lookup
#'
#' This function uses the TextMagic carrier lookup API.
#' https://www.textmagic.com/docs/api/carrier-lookup/
#'
#'
#' @name tm_carrier
#' @description Retrieve the carrier for a single phone number, with country
#' optionally provided.
#' @param phone character The phone number.  Required.
#' @param country character The phone number.  Optional.
#' @param tm_user character The TextMagic API username. Looks for environment variable TM_USER.  Required..
#' @param tm_key character The TextMagic API key. Looks for environment variable TM_KEY. Required.
#' @return a list containing the response from the API
#' @keywords textmagic
#' @export
#' @examples
#' tm_carrier(phone='6178675309', country='US')
#'

tm_carrier = function(phone=NA,
                      country=NA,
                      tm_user=Sys.getenv("TM_USER"),
                      tm_key=Sys.getenv("TM_KEY")) {
  ## phone is required, fail if it is not provided
  if(is.na(phone)) {
    stop("the parameter phone is required.")
  }
  ## need to auth to the TextMagic API with name and pass
  ## if they arent there, yell
  if(nchar(tm_user)==0) {
    stop("TextMagice verified API username is req'd")
  }
  if(nchar(tm_key)==0) {
    stop("TextMagice verified API username is req'd")
  }
  ## the BASE url for the endpoint
  BASE = "https://rest.textmagic.com/api/v2/lookups/%s"
  ## build the EP based on the inputs
  EP = ifelse(!is.na(country),
              paste0(sprintf(BASE, phone), "?country=", country),
              paste0(sprintf(BASE, phone)))
  ## call the API
  resp = httr::GET(EP, httr::add_headers(`X-TM-Username` = tm_user,
                                         `X-TM-Key` = tm_key))
  ## if the status code <> 200, fail gracefully-ish
  if(httr::status_code(resp) != 200) {
    stop("the call the API was not happy and did not return a 200")
  }
  ## extract the content
  api = httr::content(resp)
  ## return the data
  return(api)
}

