// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Fs from "fs";
import * as Js_option from "rescript/lib/es6/js_option.js";
import * as Caml_array from "rescript/lib/es6/caml_array.js";
import * as Caml_format from "rescript/lib/es6/caml_format.js";
import * as Caml_option from "rescript/lib/es6/caml_option.js";

var input = Fs.readFileSync("input.txt", "ascii");

function getStartingItems(monkey) {
  var substrings = Caml_array.get(monkey, 1).split(": ");
  return Caml_array.get(substrings, 1).split(", ").map(Caml_format.float_of_string);
}

function getOperation(monkey) {
  var substrings = Caml_array.get(monkey, 2).split("= ");
  var mul = Caml_array.get(substrings, 1).split(" * ");
  if (mul.length === 1) {
    var add = Caml_array.get(substrings, 1).split(" + ");
    var match = Caml_array.get(add, 1);
    if (match === "old") {
      return {
              TAG: /* Add */0,
              _0: -1.0
            };
    } else {
      return {
              TAG: /* Add */0,
              _0: Caml_format.float_of_string(Caml_array.get(add, 1))
            };
    }
  }
  var match$1 = Caml_array.get(mul, 1);
  if (match$1 === "old") {
    return {
            TAG: /* Mul */1,
            _0: -1.0
          };
  } else {
    return {
            TAG: /* Mul */1,
            _0: Caml_format.float_of_string(Caml_array.get(mul, 1))
          };
  }
}

function getTest(monkey) {
  var lines = monkey.slice(-3);
  var test = Caml_array.get(lines, 0).split("by ");
  var iftrue = Caml_array.get(lines, 1).split("monkey ");
  var iffalse = Caml_array.get(lines, 2).split("monkey ");
  return {
          divisibleBy: Caml_format.float_of_string(Caml_array.get(test, 1)),
          condTrue: Caml_format.int_of_string(Caml_array.get(iftrue, 1)),
          condFalse: Caml_format.int_of_string(Caml_array.get(iffalse, 1))
        };
}

var monkeys_str = input.split("\n\n").map(function (x) {
      return x.split("\n").filter(function (y) {
                  return y !== "";
                });
    });

var monkeys = monkeys_str.map(function (m) {
      return {
              items: getStartingItems(m),
              op: getOperation(m),
              test: getTest(m),
              interactions: {
                contents: 0.0
              }
            };
    });

function calculateOperation(op, item) {
  if (op.TAG === /* Add */0) {
    var number = op._0;
    if (number === -1.0) {
      return item + item;
    } else {
      return item + number;
    }
  }
  var number$1 = op._0;
  if (number$1 === -1.0) {
    return item * item;
  } else {
    return item * number$1;
  }
}

function getInteractions(monkeys) {
  return monkeys.map(function (x) {
                return x.interactions.contents;
              }).sort(function (n1, n2) {
              return n2 - n1 | 0;
            });
}

function round(monkeys) {
  for(var idx = 0 ,idx_finish = monkeys.length; idx < idx_finish; ++idx){
    var monkey = Caml_array.get(monkeys, idx);
    while(monkey.items.length > 0) {
      var item = Js_option.getExn(Caml_option.undefined_to_opt(monkey.items.shift()));
      var worry = calculateOperation(monkey.op, item) / 3.0;
      var throwto = worry % monkey.test.divisibleBy === 0.0 ? monkey.test.condTrue : monkey.test.condFalse;
      Caml_array.get(monkeys, throwto).items.push(worry);
      monkey.interactions.contents = monkey.interactions.contents + 1.0;
    };
  }
}

for(var _for = 1; _for <= 20; ++_for){
  round(monkeys);
}

var interactions = getInteractions(monkeys);

console.log("Part 1:", Caml_array.get(interactions, 0) * Caml_array.get(interactions, 1));

var monkeys$1 = monkeys_str.map(function (m) {
      return {
              items: getStartingItems(m),
              op: getOperation(m),
              test: getTest(m),
              interactions: {
                contents: 0.0
              }
            };
    });

var maxWorry = monkeys$1.reduce((function (acc, i) {
        return acc * i.test.divisibleBy;
      }), 1.0);

function roundPart2(monkeys) {
  for(var idx = 0 ,idx_finish = monkeys.length; idx < idx_finish; ++idx){
    var monkey = Caml_array.get(monkeys, idx);
    while(monkey.items.length > 0) {
      var item = Js_option.getExn(Caml_option.undefined_to_opt(monkey.items.shift()));
      var worry = calculateOperation(monkey.op, item) % maxWorry;
      var throwto = worry % monkey.test.divisibleBy === 0.0 ? monkey.test.condTrue : monkey.test.condFalse;
      Caml_array.get(monkeys, throwto).items.push(worry);
      monkey.interactions.contents = monkey.interactions.contents + 1.0;
    };
  }
}

for(var _for$1 = 1; _for$1 <= 10000; ++_for$1){
  roundPart2(monkeys$1);
}

var interactions$1 = getInteractions(monkeys$1);

console.log("Part 2:", Caml_array.get(interactions$1, 0) * Caml_array.get(interactions$1, 1));

export {
  input ,
  getStartingItems ,
  getOperation ,
  getTest ,
  monkeys_str ,
  calculateOperation ,
  getInteractions ,
  round ,
  monkeys$1 as monkeys,
  maxWorry ,
  roundPart2 ,
  interactions$1 as interactions,
}
/* input Not a pure module */