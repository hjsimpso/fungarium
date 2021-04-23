#mycoportal_tab helper function - helps navigate to MyCoPortal download window

myswitch <- function (remDr, windowId)
{
  qpath <- sprintf("%s/session/%s/window", remDr$serverURL,
                   remDr$sessionInfo[["id"]])
  remDr$queryRD(qpath, "POST", qdata = list(handle = windowId))
}

