// Note this only includes basic configuration for development mode.
// For a more comprehensive configuration check:
// https://github.com/fable-compiler/webpack-config-template

var path = require("path");
const HtmlWebpackPlugin = require('html-webpack-plugin');

module.exports = {
    mode: "development",
    entry: "./PacmanWeb.fsproj",
	plugins: [
		new HtmlWebpackPlugin({
			template: path.join(__dirname, './src/index.html')
		})
	],
	output: {
        path: path.join(__dirname, "./wwwroot"),
			filename: "[name].[chunkhash].js",
    },
    devServer: {
        publicPath: "/",
        contentBase: "./wwwroot",
        port: 8080,
    },
    module: {
        rules: [{
            test: /\.fs(x|proj)?$/,
            use: "fable-loader"
        }]
    }
}