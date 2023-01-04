// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Fs from "fs";
import * as Js_dict from "rescript/lib/es6/js_dict.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Caml_array from "rescript/lib/es6/caml_array.js";
import * as Caml_format from "rescript/lib/es6/caml_format.js";

var input = Fs.readFileSync("input.txt", "ascii");

var lines = input.split("\n").slice(0, -1);

var instructions = lines.map(function (line) {
      var s = line.split(" ");
      var match = Caml_array.get(s, 0);
      switch (match) {
        case "D" :
            return [
                    /* Down */2,
                    Caml_format.int_of_string(Caml_array.get(s, 1))
                  ];
        case "L" :
            return [
                    /* Left */3,
                    Caml_format.int_of_string(Caml_array.get(s, 1))
                  ];
        case "R" :
            return [
                    /* Right */1,
                    Caml_format.int_of_string(Caml_array.get(s, 1))
                  ];
        case "U" :
            return [
                    /* Up */0,
                    Caml_format.int_of_string(Caml_array.get(s, 1))
                  ];
        default:
          return [
                  /* None */4,
                  0
                ];
      }
    });

function dirToVector(dir) {
  switch (dir) {
    case /* Up */0 :
        return [
                -1,
                0
              ];
    case /* Right */1 :
        return [
                0,
                1
              ];
    case /* Down */2 :
        return [
                1,
                0
              ];
    case /* Left */3 :
        return [
                0,
                -1
              ];
    case /* None */4 :
        return [
                0,
                0
              ];
    
  }
}

function vectorDistance($staropt$star, param) {
  var b = $staropt$star !== undefined ? $staropt$star : [
      0,
      0
    ];
  return Math.max(Math.abs(param[0] - b[0] | 0), Math.abs(param[1] - b[1] | 0));
}

function addVector(param, param$1) {
  return [
          param[0] + param$1[0] | 0,
          param[1] + param$1[1] | 0
        ];
}

function subVector(param, param$1) {
  return [
          param[0] - param$1[0] | 0,
          param[1] - param$1[1] | 0
        ];
}

function signedVector(param) {
  return [
          Math.sign(param[0]),
          Math.sign(param[1])
        ];
}

function $$process(instructions, ropeLength) {
  var tracks = {};
  var rope = Belt_Array.make(ropeLength, [
        0,
        0
      ]);
  instructions.forEach(function (param) {
        var instruction = param[0];
        for(var _for = 1 ,_for_finish = param[1]; _for <= _for_finish; ++_for){
          Caml_array.set(rope, 0, addVector(Caml_array.get(rope, 0), dirToVector(instruction)));
          for(var x = 1 ,x_finish = rope.length; x < x_finish; ++x){
            var difference = subVector(Caml_array.get(rope, x - 1 | 0), Caml_array.get(rope, x));
            if (vectorDistance(undefined, difference) > 1) {
              Caml_array.set(rope, x, addVector(Caml_array.get(rope, x), signedVector(difference)));
            }
            
          }
          var match = Caml_array.get(rope, rope.length - 1 | 0);
          var key = "" + String(match[0]) + "-" + String(match[1]) + "";
          var v = Js_dict.get(tracks, key);
          var visited = v !== undefined ? v : 0;
          tracks[key] = visited + 1 | 0;
        }
      });
  return Js_dict.values(tracks).length;
}

function p(param) {
  return $$process(instructions, param);
}

console.log("Part 1:", $$process(instructions, 2));

console.log("Part 2:", $$process(instructions, 10));

export {
  input ,
  lines ,
  instructions ,
  dirToVector ,
  vectorDistance ,
  addVector ,
  subVector ,
  signedVector ,
  $$process ,
  p ,
}
/* input Not a pure module */
