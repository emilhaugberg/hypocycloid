exports.ellipse = function (e) {
  return function (ctx) {
    return function () {
      return ctx.ellipse(e.x, e.y, e.radiusX, e.radiusY, e.rotation, e.startAngle, e.endAngle)
    }
  }
}
