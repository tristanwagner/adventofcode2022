open Js.Array2
open Node.Fs

let input = readFileSync("input.txt", #ascii)

let sum = (i, j) => i + j

let sumArray = (arr) => reduce(arr, sum, 0)

let elfs = Js.String.split("\n\n", input)

let calories = map(elfs, (elf) => {
  let splitted = Js.String.split("\n", elf)
  let parsed = map(splitted, (cal) => {
    Js.String.length(cal) > 0 ? int_of_string(cal) : 0
  })
  parsed->sumArray
})

// Answer 1 Get max calories for one elf
Js.log2("Part 1: ", calories->Js.Math.maxMany_int)

// Answer 2 Get sum of 3 top elf carriers
Js.log2("Part 2: ", calories->sortInPlace->sliceFrom(-3)->sumArray)
