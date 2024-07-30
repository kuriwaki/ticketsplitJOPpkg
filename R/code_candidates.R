#' Translate abbreviation codes to labels
#'
#' @param vec a character vector of variable labels
#' @param linebreak whether or not to linebreak labels
#'
#' @importFrom dplyr recode
#' @export
recode_abbrv <- function(vec, linebreak = FALSE) {
  recoded <-
    recode(vec,
           PRS =   "President",
           PTY =     "Party Ticket",
           USSEN = "U.S. Senate",
           USS = "U.S. Senate",
           USSEN1 = "U.S. Senate",
           USSEN2 = "U.S. Senate (2)",
           USS1 = "U.S. Senate",
           USS2 = "U.S. Senate (2)",
           GOV =   "Governor",
           LGV =   "Lt. Governor",
           SOS =   "Secretary of State",
           ATG =   "Attorney General",
           SSI =   "School Superintendent",
           AUD = "County Auditor",
           USHOU = "U.S. House",
           USH = "U.S. House",
           SEN =   "State Senate",
           HOU =   "State House",
           SHF =   "County\nSheriff",
           CLR =   "Clerk of Court",
           COR =   "Coroner",
           CCD =   "County Council",
           JPR =   "Probate Judge",
           JPRB =   "Probate Judge",
           CTRES =   "County Treasurer",
           CTR =   "County Treasurer",
           CAP =   "Tax for\nCapital\nProjects",
           SCH =   "Tax for\nSchool\nProjects",
           BND =   "Issue Bonds",
           ALC =   "Alcohol Sale",
           LRCA1 =   "Con. Amendment: Gambling",
           LRCA2 =   "Con. Amendment: Form of Gov"
    )
  if (linebreak)
    return(recoded)
  if (!linebreak)
    return(str_replace_all(recoded, "\n", " "))
}
