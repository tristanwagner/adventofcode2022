// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Fs from "fs";
import * as Caml_array from "rescript/lib/es6/caml_array.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";
import * as Caml_format from "rescript/lib/es6/caml_format.js";
import * as Caml_option from "rescript/lib/es6/caml_option.js";
import * as Caml_splice_call from "rescript/lib/es6/caml_splice_call.js";

var input = Fs.readFileSync("input.txt", "ascii");

var parts = input.split("\n").filter(function (s) {
      return s !== "";
    });

var stacks = parts.filter(function (s) {
      return !s.includes("move");
    });

var instructions = parts.filter(function (s) {
        return s.includes("move");
      }).reduce((function (acc, current) {
        var sp = current.split(" ");
        var amount = Caml_format.int_of_string(Caml_array.get(sp, 1));
        var from = Caml_format.int_of_string(Caml_array.get(sp, 3));
        var to = Caml_format.int_of_string(Caml_array.get(sp, 5));
        acc.push({
              from: from,
              to: to,
              amount: amount
            });
        return acc;
      }), []);

var l = stacks.pop();

var cols = l !== undefined ? l.replace(/\s/g, "").length : 0;

function getPiles(param) {
  var piles = [];
  for(var _for = 0; _for < cols; ++_for){
    piles.push([]);
  }
  for(var y = 0 ,y_finish = stacks.length; y < y_finish; ++y){
    var col = 0;
    for(var t = 1 ,t_finish = Caml_array.get(stacks, y).length; t <= t_finish; ++t){
      if (t === 1 || (t - 1 | 0) % 4 === 0) {
        var c = Caml_array.get(stacks, y).charAt(t);
        if (c !== " ") {
          Caml_array.get(piles, col).push(c);
        }
        col = col + 1 | 0;
      }
      
    }
  }
  return piles;
}

var piles = getPiles(undefined);

instructions.forEach(function (instruction) {
      var amnt = instruction.amount;
      while(amnt > 0) {
        var box = Belt_Option.getExn(Caml_option.undefined_to_opt(Caml_array.get(piles, instruction.from - 1 | 0).shift()));
        Caml_array.get(piles, instruction.to - 1 | 0).unshift(box);
        amnt = amnt - 1 | 0;
      };
    });

var result = piles.reduce((function (acc, curr) {
        return acc + Caml_array.get(curr, 0);
      }), "");

console.log("Part 1:", result);

var piles2 = getPiles(undefined);

instructions.forEach(function (instruction) {
      var amnt = instruction.amount;
      var tmp = [];
      while(amnt > 0) {
        var box = Belt_Option.getExn(Caml_option.undefined_to_opt(Caml_array.get(piles2, instruction.from - 1 | 0).shift()));
        tmp.push(box);
        amnt = amnt - 1 | 0;
      };
      Caml_splice_call.spliceObjApply(Caml_array.get(piles2, instruction.to - 1 | 0), "unshift", [tmp]);
    });

var result2 = piles2.reduce((function (acc, curr) {
        return acc + Caml_array.get(curr, 0);
      }), "");

console.log("Part 2:", result2);

export {
  input ,
  parts ,
  stacks ,
  instructions ,
  cols ,
  getPiles ,
  piles ,
  result ,
  piles2 ,
  result2 ,
}
/* input Not a pure module */
