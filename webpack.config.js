var path = require('path')

module.exports = {
  entry: './public/index.js',
  output: {
    path: path.join(__dirname, 'public'),
    filename: 'bundle.js'
  }
}
