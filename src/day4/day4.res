open Js.Array2
open Node.Fs

let input = readFileSync("input.txt", #ascii)

let pairs = input->Js.String2.split("\n")->Js.Array2.filter((s) => s !== "")

type range = { low: int, high: int }

let ranges = pairs->map((pair) => {
  pair->Js.String2.split(",")->map((x) => {
    let s = x->Js.String2.split("-")
    { low: int_of_string(s[0]), high: int_of_string(s[1]) }
  })
})

let rangeOverlap = (r1, r2) => {
  (r1.low <= r2.low && r1.high >= r2.high) || (r2.low <= r1.low && r2.high >= r1.high)
}

let result = ranges->reduce((acc, range) => {
  rangeOverlap(range[0], range[1]) ? acc + 1 : acc
}, 0)

Js.log2("Part 1:", result)

let rangeOverlapAtAll = (r1, r2) => {
  (r1.low <= r2.low && r1.high >= r2.low) || (r2.low <= r1.low && r2.high >= r1.low)
}

let result2 = ranges->reduce((acc, range) => {
  rangeOverlapAtAll(range[0], range[1]) ? acc + 1 : acc
}, 0)

Js.log2("Part 2:", result2)
