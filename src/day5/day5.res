open Js.Array2
open Node.Fs

let input = readFileSync("input.txt", #ascii)

let parts = input->Js.String2.split("\n")->Js.Array2.filter((s) => s !== "")

let stacks = parts->Js.Array2.filter((s) => !(s->Js.String2.includes("move")))

type instruction = { from: int, to: int, amount: int }

let instructions = parts->Js.Array2.filter((s) => s->Js.String2.includes("move"))->reduce((acc, current) => {
  let sp = current->Js.String2.split(" ")
  let amount = int_of_string(sp[1])
  let from = int_of_string(sp[3])
  let to = int_of_string(sp[5])
  acc->push({ from, to, amount })->ignore
  acc
}, [])

let cols = switch stacks->Js.Array2.pop {
  | Some(l) => l->Js.String2.replaceByRe(%re("/\s/g"), "")->Js.String2.length
  | _ => 0
}

let getPiles = () => {
  let piles = []

  for _ in 0 to cols - 1 {
    piles->push([])->ignore
  }

  for y in 0 to stacks->length - 1 {
    let col = ref(0)
    for t in 1 to stacks[y]->Js.String2.length {
      if (t === 1 || mod(t - 1, 4) === 0) {
        let c = stacks[y]->Js.String2.charAt(t)
        if (c !== " ") {
          piles[col.contents]->push(c)->ignore
        }
        col.contents = col.contents + 1
      }
    }
  }

  piles
}

let piles = getPiles()

forEach(instructions, (instruction) => {
  let amnt = ref(instruction.amount)
  while (amnt.contents > 0) {
    let box = piles[instruction.from - 1]->shift->Belt.Option.getExn
    piles[instruction.to - 1]->unshift(box)->ignore
    amnt.contents = amnt.contents - 1
  }
})

let result = piles->reduce((acc, curr) => {
  acc ++ curr[0]
}, "")

Js.log2("Part 1:", result)

let piles2 = getPiles()

forEach(instructions, (instruction) => {
  let amnt = ref(instruction.amount)
  let tmp = []
  while (amnt.contents > 0) {
    let box = piles2[instruction.from - 1]->shift->Belt.Option.getExn
    tmp->push(box)->ignore
    amnt.contents = amnt.contents - 1
  }
  piles2[instruction.to - 1]->unshiftMany(tmp)->ignore
})

let result2 = piles2->reduce((acc, curr) => {
  acc ++ curr[0]
}, "")

Js.log2("Part 2:", result2)
