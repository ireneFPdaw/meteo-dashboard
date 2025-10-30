const path = require("path");
const { HtmlRspackPlugin } = require("@rspack/core");

/** @type {import('@rspack/core').Configuration} */
module.exports = {
  mode: "development",
  entry: { main: "./src/main.ts" },
  devtool: "source-map",

  devServer: {
    static: { directory: path.resolve(__dirname, "public") },
    port: Number(process.env.PORT) || 5174,
    open: true,
  },

  output: {
    path: path.resolve(__dirname, "dist"),
    filename: "assets/[name].[contenthash].js",
    publicPath: "",
    clean: true,
  },
  plugins: [
    new HtmlRspackPlugin({
      template: "./index.html",
    }),
  ],

  module: {
    rules: [
      {
        test: /\.ts$/,
        use: [
          {
            loader: "builtin:swc-loader",
            options: {
              jsc: { parser: { syntax: "typescript" } },
            },
          },
        ],
      },
    ],
  },

  resolve: { extensions: [".ts", ".js"] },
};