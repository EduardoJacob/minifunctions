#' Solve Parabola given Parameters a, b, c
#'
#' @param a ax^2 + bx + c
#' @param b ax^2 + bx + c
#' @param c ax^2 + bx + c
#'
#' @export
#'
#' @examples
#' parabola(5,-30,49)
parabola = function(a,b,c) {
  h = -b/(2*a)
  k = a*h^2 + b*h + c
  f = k + 1/(4*a)
  d = k - 1/(4*a)

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


