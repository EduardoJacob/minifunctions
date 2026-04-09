#' Solve Parabola given Focus(h,f) and Directrix y=d
#'
#' @param h Focus(h,f)
#' @param f Focus(h,f)
#' @param d Directrix y=d
#'
#' @export
#'
#' @examples
#' parabolafromfocus(18,24,34)
parabolafromfocus = function(h,f,d) {
  k = (f + d)/2
  a = 1/(2*f - 2*d)
  b = -2*a*h
  c = a*h^2 + k

  p = polynom::polynomial(c(c,b,a))
  roots = solve(p)
  r1 = roots[1]
  r2 = roots[2]

  message("Parabola in Standard Form : ",p," , Roots: ",r1," , ",r2)
  message("Parabola in Vertex Form   : y = ",a,"( x - ",h," )^2 + ",k)
  message("Vertex : (",h,",",k,")")
  message("Focus  : (",h,",",f,")")
  message("Directrix : y = ",d)
}



