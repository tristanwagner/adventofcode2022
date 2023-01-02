// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Fs from "fs";
import * as Js_dict from "rescript/lib/es6/js_dict.js";
import * as Caml_array from "rescript/lib/es6/caml_array.js";
import * as Caml_format from "rescript/lib/es6/caml_format.js";

var input = Fs.readFileSync("input.txt", "ascii");

var lines = input.split("\n").slice(0, -1);

function parseLine(line) {
  var s = line.split(" ");
  var match = Caml_array.get(s, 0);
  switch (match) {
    case "$" :
        if (Caml_array.get(s, 1) === "cd") {
          return {
                  TAG: /* Cd */0,
                  _0: Caml_array.get(s, 2)
                };
        } else if (Caml_array.get(s, 1) === "ls") {
          return /* Ls */0;
        } else {
          return {
                  TAG: /* File */1,
                  _0: Caml_format.int_of_string(Caml_array.get(s, 0))
                };
        }
    case "dir" :
        return {
                TAG: /* Dir */2,
                _0: Caml_array.get(s, 1)
              };
    default:
      return {
              TAG: /* File */1,
              _0: Caml_format.int_of_string(Caml_array.get(s, 0))
            };
  }
}

var currentDir = {
  contents: "/"
};

var dirs = {};

lines.map(parseLine).forEach(function (line) {
      if (typeof line === "number") {
        return ;
      }
      switch (line.TAG | 0) {
        case /* Cd */0 :
            var dir = line._0;
            if (dir === "..") {
              currentDir.contents = currentDir.contents.split("/").slice(0, -2).join("/") + "/";
            } else if (dir === "/") {
              currentDir.contents = "/";
            } else {
              currentDir.contents = currentDir.contents + dir + "/";
            }
            return ;
        case /* File */1 :
            var val = Js_dict.get(dirs, currentDir.contents);
            var s = val !== undefined ? val : 0;
            dirs[currentDir.contents] = s + line._0 | 0;
            return ;
        case /* Dir */2 :
            dirs[currentDir.contents + line._0 + "/"] = 0;
            return ;
        
      }
    });

Js_dict.entries(dirs).forEach(function (param) {
      var size = param[1];
      var s = param[0].split("/");
      s.pop();
      while(s.length > 1) {
        s.pop();
        var parentKey = s.join("/") + "/";
        var val = Js_dict.get(dirs, parentKey);
        var newSize = val !== undefined ? val + size | 0 : size;
        dirs[parentKey] = newSize;
      };
    });

var result = Js_dict.entries(dirs).reduce((function (acc, param) {
        if (param[1] >= 100000) {
          return acc;
        }
        var val = Js_dict.get(dirs, param[0]);
        var s = val !== undefined ? val : 0;
        return acc + s | 0;
      }), 0);

console.log("Part 1:", result);

var val = Js_dict.get(dirs, "/");

var totalUsedSize = val !== undefined ? val : 0;

var availableSpace = 70000000 - totalUsedSize | 0;

var neededSpace = 30000000 - availableSpace | 0;

var result2 = Js_dict.entries(dirs).filter(function (param) {
          return param[1] >= neededSpace;
        }).map(function (param) {
        return param[1];
      }).sort(function (n1, n2) {
      return n1 - n2 | 0;
    });

console.log("Part 2:", Caml_array.get(result2, 0));

var totalSize = 70000000;

var neededSpaceForUpdate = 30000000;

export {
  input ,
  lines ,
  parseLine ,
  currentDir ,
  dirs ,
  result ,
  totalSize ,
  neededSpaceForUpdate ,
  totalUsedSize ,
  availableSpace ,
  neededSpace ,
  result2 ,
}
/* input Not a pure module */
