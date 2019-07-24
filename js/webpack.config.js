const path = require('path');

module.exports = {
  // mode: 'development',
  mode: 'production',
  target: 'web',
  entry: './main.js',
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'client-deps.js'
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
    extensions: ['.ts', '.tsx', '.js', '.jsx', '.json'], // Automatically resolve certain extensions
    enforceExtension: false, // If true, it will not allow extension-less files
  },
};
