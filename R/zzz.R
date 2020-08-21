.onAttach <- function(...) {

    packageStartupMessage('If you find this package useful, please consider supporting the development at
https://ko-fi.com/opisthokonta')

}

.onUnload <- function (libpath) {
  library.dynam.unload("goalmodel", libpath)
}
