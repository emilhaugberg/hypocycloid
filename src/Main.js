exports.ellipse = function (e) {
  return function (ctx) {
    return function () {
      return ctx.ellipse(e.x, e.y, e.rx, e.ry, e.rotation, e.startAngle, e.endAngle)
    }
  }
}
