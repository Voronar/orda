const path = require('path');

module.exports = {
  // mode: 'development',
  mode: 'production',
  target: 'web',
  entry: './main.js',
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'orda.js'
  },
  module: {
    rules: [
      {
        test: /\.(js|jsx)$/,
        exclude: /node_modules/,
        use: {
          loader: "babel-loader"
        }
      }
    ]
  },
  resolve: {
    modules: ['node_modules'],
    extensions: ['.js', '.jsx'], // Automatically resolve certain extensions
    enforceExtension: false, // If true, it will not allow extension-less files
  },
};
