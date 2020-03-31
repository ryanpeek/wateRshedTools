## README

This is a custom package of random scripts mainly used for watershed analysis. It is certainly likely to break, but it may work! To which degree depends on your system and how much chocolate you've consumed. I make no guarantees to support or address anything contained herein. This is largely a personal "play" package I'm using to learn more about package development. It will likely take the place of my `.Rprofile` script in the near future.

### To Install

With `devtools` package:

 - `devtools::install_github("ryanpeek/wateRshedTools")`

OR with the `remotes` package:

 - `remotes::install_github("ryanpeek/wateRshedTools")`


### Package Folder Structure

 - Source scripts to create datasets live in `raw-data`
 - Existing example datasets that you want to use live in `data`

### Loading/Running

`devtools::document(".")`
