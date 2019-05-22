'use strict';

var content;

const express = require('express');

var fs = require("fs");
fs.readFile("index.html", "utf8", function (err, data) {
    if (err) throw err;
    console.log("read index.html");
    content = data;
});

// Constants
const PORT = 8080;
const HOST = '0.0.0.0';

const app = express();
// TODO: that's probably not the smartest way to go.
app.use(express.static(__dirname + '/node_modules/vis'));
app.get('/', (req, res) => {
  res.send(content);
});

app.listen(PORT, HOST);
console.log(`Running on http://${HOST}:${PORT}`);