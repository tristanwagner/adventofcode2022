// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Fs from "fs";
import * as Caml_array from "rescript/lib/es6/caml_array.js";
import * as Caml_int32 from "rescript/lib/es6/caml_int32.js";
import * as Caml_format from "rescript/lib/es6/caml_format.js";

var input = Fs.readFileSync("input.txt", "ascii");

var lines = input.split("\n").slice(0, -1);

var instructions = lines.map(function (line) {
      var s = line.split(" ");
      var match = Caml_array.get(s, 0);
      switch (match) {
        case "addx" :
            return [
                    /* AddX */1,
                    Caml_format.int_of_string(Caml_array.get(s, 1))
                  ];
        case "noop" :
            return [
                    /* Noop */0,
                    0
                  ];
        default:
          return [
                  /* Noop */0,
                  0
                ];
      }
    });

function generateCycles(instructions) {
  var x = {
    contents: 1
  };
  return instructions.reduce((function (acc, param, index) {
                if (index === 0) {
                  acc.push(x.contents);
                }
                if (param[0]) {
                  acc.push(x.contents);
                  x.contents = x.contents + param[1] | 0;
                }
                acc.push(x.contents);
                return acc;
              }), []);
}

var cycles = generateCycles(instructions);

var signals = cycles.map(function (value, index) {
      return Math.imul(value, index + 1 | 0);
    });

console.log("Part 1:", [
        20,
        60,
        100,
        140,
        180,
        220
      ].reduce((function (i, j) {
            return i + Caml_array.get(signals, j - 1 | 0) | 0;
          }), 0));

function draw(cycles, width) {
  var crt = cycles.map(function (x, i) {
        var ptr = Caml_int32.mod_(i, width);
        if (ptr >= (x - 1 | 0) && ptr <= (x + 1 | 0)) {
          return "#";
        } else {
          return ".";
        }
      });
  for(var cell = 0 ,cell_finish = crt.length; cell < cell_finish; ++cell){
    if (Caml_int32.mod_(cell, width) === 0) {
      console.log(crt.slice(cell - width | 0, cell).join(""));
    }
    
  }
}

console.log("Part 2:");

draw(cycles, 40);

export {
  input ,
  lines ,
  instructions ,
  generateCycles ,
  cycles ,
  signals ,
  draw ,
}
/* input Not a pure module */
