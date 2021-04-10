dew.calculateSlope <- function(x, y) {
  slope <- pi / 2 - atan(y / x)
  if (x > 0 & y > 0) {
    slope <- slope - 0.5 * pi
  } else if (x < 0 & y < 0) {
    slope <- slope + 0.5 * pi
  } else if (x > 0 & y < 0) {
    slope <- slope - 0.5 * pi
  } else if (x < 0 & y > 0) {
    slope <- slope + 0.5 * pi
  }
  slope * 180 / pi + 90
}

dew.getRadialX <- function (a) {
  500 + 400 * cos(a * pi / 180);
}

dew.getRadialY <- function (a) {
  500 + 400 * sin(a * pi / 180);
}