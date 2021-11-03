var path = require('path');
var webpack = require('webpack');
var { merge } = require('webpack-merge');
var HtmlWebpackPlugin = require('html-webpack-plugin');
var autoprefixer = require('autoprefixer');
var CopyPlugin = require('copy-webpack-plugin');
const UglifyJSPlugin = require('uglifyjs-webpack-plugin')
const MiniCssExtractPlugin = require('mini-css-extract-plugin');

const prod = 'production';
const dev = 'development';

// determine build env
const TARGET_ENV = process.env.npm_lifecycle_event === 'build' ? prod : dev;
const isDev = TARGET_ENV == dev;
const isProd = TARGET_ENV == prod;

// entry and output path/filename variables
const entryPath = path.join(__dirname, 'client/static/index.js');
const outputPath = path.join(__dirname, 'dist');
const outputFilename = isProd ? '[name]-[hash].js' : '[name].js'

console.log('WEBPACK GO! Building for ' + TARGET_ENV);

// common webpack config (valid for dev and prod)
var commonConfig = {
  output: {
    path: outputPath,
    filename: `static/js/${outputFilename}`,
    publicPath: '/'
  },
  resolve: {
    extensions: ['.js', '.elm'],
    modules: ['node_modules']
  },
  module: {
    noParse: /\.elm$/,
    rules: [{
      test: /\.(eot|ttf|woff|woff2|svg)$/,
      use: 'file-loader?publicPath=../../&name=static/css/[hash].[ext]'
    }]
  },
  plugins: [
    new webpack.LoaderOptionsPlugin({
      options: {
        postcss: [autoprefixer()]
      }
    }),
    new HtmlWebpackPlugin({
      template: 'client/index.html',
      inject: 'body',
      filename: 'index.html'
    })
  ]
}

// additional webpack settings for local env (when invoked by 'npm start')
if (isDev === true) {
  module.exports = merge(commonConfig, {
    entry: [
      'webpack-dev-server/client?http://localhost:8083',
      entryPath
    ],
    devServer: {
      // serve index.html in place of 404 responses
      historyApiFallback: true,
      contentBase: './client/static',
      contentBasePublicPath: '/static',
      hot: true,
      port: 8083,
      proxy: {
        '/api/**': 'http://localhost:8084/',
      }
    },
    module: {
      rules: [{
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: [
          {
            loader: 'elm-webpack-loader',
            options: {
              debug: true
            }
          }]
      }, {
        test: /\.sc?ss$/,
        use: ['style-loader', 'css-loader', 'postcss-loader', 'sass-loader']
      }]
    }
  });
}

// additional webpack settings for prod env (when invoked via 'npm run build')
if (isProd === true) {
  // needed for tailwind purge
  process.env.NODE_ENV = "production";

  module.exports = merge(commonConfig, {
    entry: entryPath,
    module: {
      rules: [{
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: 'elm-webpack-loader'
      }, {
        test: /\.sc?ss$/,
        use: [MiniCssExtractPlugin.loader, 'css-loader', 'postcss-loader', 'sass-loader']
      }]
    },
    plugins: [
      new MiniCssExtractPlugin({
        filename: 'static/css/[name]-[hash].css',
      }),
      new CopyPlugin({
        patterns: [
          {
            from: 'client/static/img/',
            to: 'static/img/'
          }
          , {
            from: 'client/static/favicon.ico',
            to: 'static/favicon.ico'
          }
        ]
      }),
    ],
    optimization: {
      // minimizer: [
      //   // extract CSS into a separate file
      //   // minify & mangle JS/CSS
      //   new UglifyJSPlugin({
      //     uglifyOptions: {
      //       minimize: true,
      //       compressor: {
      //         warnings: false
      //       }
      //       // mangle:  true
      //     }
      //   })
      // ],
      splitChunks: {
        cacheGroups: {
          styles: {
            name: 'styles',
            test: /\.css$/,
            chunks: 'all',
            enforce: true,
          },
        },
      },
    }
  });
}
