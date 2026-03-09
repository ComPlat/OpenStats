\name{upload}
\alias{upload}
\title{upload a file from the ELN}
\usage{
  upload(
    session,
    filepath
  )
}
\arguments{
  \item{session}{A shiny session object}
  \item{folder}{A character string which defines the path to a file where the target file is located}
}
\value{
  Nothing is returned. In case the upload does not work a notification is shown to the user.
}
\description{
  The function uploads a file to the ELN. Thus, it is necessary to store the results of the user to a file before sending it to the ELN.\cr
  As a side note it is only possible to upload a file once to the ELN. As the token is not valid after the upload. The idea is   that the app is closed after the file is uploaded. \cr
  After, the upload the file is deleted automatically within the funciton \emph{upload}.
}
\examples{ 
  \dontrun{
    server <- function(input, output, session) {
          # the variable file is a shiny::reactive value containing 
          # a logical called 'ready' and a filepath called 'file' 
          observeEvent(file$ready, { 
            COMELN::upload(session, file$file)
          })      
      }
    }
  }