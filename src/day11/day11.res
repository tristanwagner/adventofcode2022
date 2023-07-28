open Js.Array2
open Node.Fs

let input = readFileSync("input.txt", #ascii)

type op = Add(float) | Mul(float)

type test = {
  divisibleBy: float,
  condTrue: int,
  condFalse: int,
}

type monkey = {
  items: array<float>,
  op,
  test,
  interactions: ref<float>,
}

let getStartingItems = (monkey) => {
  let substrings = monkey[1]->Js.String2.split(": ")
  substrings[1]->Js.String2.split(", ")
  ->map(x => (float_of_string(x)))
}

let getOperation = (monkey) => {
  let substrings = monkey[2]->Js.String2.split("= ")
  let mul = substrings[1]->Js.String2.split(" * ")
  if (mul->length == 1) {
    let add = substrings[1]->Js.String2.split(" + ")
    switch (add[1]) {
      | "old" => Add(-1.0)
      | _ => Add(float_of_string(add[1]))
    }
  } else {
    switch (mul[1]) {
      | "old" => Mul(-1.0)
      | _ => Mul(float_of_string(mul[1]))
    }
  }
}

let getTest = (monkey) => {
  let lines = monkey->sliceFrom(-3)

  let test = lines[0]->Js.String2.split("by ")
  let iftrue = lines[1]->Js.String2.split("monkey ")
  let iffalse = lines[2]->Js.String2.split("monkey ")
  {
    divisibleBy: float_of_string(test[1]),
    condTrue: int_of_string(iftrue[1]),
    condFalse: int_of_string(iffalse[1]),
  }
}

let monkeys_str =
  input->Js.String2.split("\n\n")
  ->map(x =>
    x->Js.String2.split("\n")
    ->filter(y => y != "")
  )

let monkeys = monkeys_str->map(m => {
  {
    items: m->getStartingItems,
    op: m->getOperation,
    test: m->getTest,
    interactions: ref(0.0)
  }
})

let calculateOperation = (op, item) => {
  switch (op) {
    | Add(number) => number == -1.0 ? item +. item : item +. number
    | Mul(number) => number == -1.0 ? item *. item : item *. number
  }
}

let getInteractions = (monkeys) => {
  monkeys->map(x => x.interactions.contents)
  ->sortInPlaceWith((n1, n2) => int_of_float(n2 -. n1))
}

let round = (monkeys) => {
  for idx in 0 to monkeys->length - 1 {
    let monkey = monkeys[idx]
    while(monkey.items->length > 0){
      let item = monkey.items->shift->Js.Option.getExn
      let worry = calculateOperation(monkey.op, item) /. 3.0
      let throwto = switch mod_float(worry, monkey.test.divisibleBy) == 0.0 {
        | true => monkey.test.condTrue
        | false => monkey.test.condFalse
      }
      // throw to monkey
      monkeys[throwto].items->push(worry)->ignore
      monkey.interactions.contents = monkey.interactions.contents +. 1.0
    }
  }
}

for _ in 1 to 20 {
  round(monkeys)
}

let interactions = monkeys->getInteractions

Js.log2("Part 1:", interactions[0] *. interactions[1])

let monkeys = monkeys_str->map(m => {
  {
    items: m->getStartingItems,
    op: m->getOperation,
    test: m->getTest,
    interactions: ref(0.0)
  }
})

let maxWorry = reduce(monkeys, (acc, i) => {
  acc *. i.test.divisibleBy
}, 1.0)

let roundPart2 = (monkeys) => {
  for idx in 0 to monkeys->length - 1 {
    let monkey = monkeys[idx]
    while(monkey.items->length > 0){
      let item = monkey.items->shift->Js.Option.getExn
      let worry = mod_float(calculateOperation(monkey.op, item), maxWorry)
      let throwto = switch mod_float(worry, monkey.test.divisibleBy) == 0.0{
        | true => monkey.test.condTrue
        | false => monkey.test.condFalse
      }
      // throw to monkey
      monkeys[throwto].items->push(worry)->ignore
      monkey.interactions.contents = monkey.interactions.contents +. 1.0
    }
  }
}

for _ in 1 to 10000 {
  roundPart2(monkeys)
}

let interactions = monkeys->getInteractions

Js.log2("Part 2:", interactions[0] *. interactions[1])
