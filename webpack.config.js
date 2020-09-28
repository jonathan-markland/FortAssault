// Note this only includes basic configuration for development mode.
// For a more comprehensive configuration check:
// https://github.com/fable-compiler/webpack-config-template

var path = require("path");

module.exports = {
    mode: "development",
    entry: "./FsFortAssaultWeb/FsFortAssaultWeb.fsproj",
    output: {
        path: path.join(__dirname, "./wwwroot"),
        filename: "bundle.js",
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