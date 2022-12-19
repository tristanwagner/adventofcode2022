open Js.Array2
open Node.Fs

let input = readFileSync("input.txt", #ascii)

let backpacks = Js.String.split("\n", input)

type content = {
  first: string,
  second: string
}

let contents = reduce(backpacks, (acc, current) => {
  let contentLength = Js.String2.length(current)
  let half = contentLength / 2
  let first = Js.String2.slice(current, ~from=0, ~to_=half)
  let second = Js.String2.sliceToEnd(current, ~from=half)
  push(acc, { first, second })->ignore
  acc
}, [])

let duplicates = reduce(contents, (acc, current) => {
  let dups = []
  for x in 0 to Js.String2.length(current.first) {
    let first = Js.String2.charAt(current.first, x)
    if (first !== "" && Js.String.includes(first, current.second) && indexOf(dups, first) < 0) {
      push(dups, first)->ignore
    }
  }
  concat(acc, dups)
}, [])

let getItemPriority = (item) => {
  let base = int_of_float(Js.String2.charCodeAt(item, 0))
  if (base <= 90) {
    base - int_of_float(Js.String2.charCodeAt("A", 0)) + 1 + 26
  } else {
    base - int_of_float(Js.String2.charCodeAt("a", 0)) + 1
  }
}

let result = reduce(duplicates, (acc, current) => {
  acc + getItemPriority(current)
}, 0)

Js.log2("Part 1:", result)

// split backpacks by groups of 3
// check for common letters in each groups of backpacks
// add

let groupedBackpacks = []
for x in 0 to length(backpacks) - 1 {
  let currentLine = backpacks[x]
  let group = [currentLine]
  if (mod(x, 3) === 0) {
    for y in 1 to 2 {
      if (x + y < length(backpacks)) {
        let targetLine = backpacks[x + y]
        push(group, targetLine)->ignore
      }
    }
    push(groupedBackpacks, group)->ignore
  }
}

let badges = reduce(groupedBackpacks, (acc, group) => {
  let break = ref(false)
  let current = group[0]
  for x in 0 to current->Js.String.length {
    let currentChar = Js.String.charAt(x, current)
    if (!break.contents &&
        currentChar !== "" &&
        every(group, (pack) => Js.String.includes(currentChar, pack))) {
      push(acc, currentChar)->ignore
      break.contents = true
    }
  }
  acc
}, [])

let result2 = reduce(badges, (acc, current) => {
  acc + getItemPriority(current)
}, 0)

Js.log2("Part 2:", result2)
