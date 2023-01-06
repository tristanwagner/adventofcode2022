open Js.Array2
open Node.Fs

let input = readFileSync("input.txt", #ascii)

let lines = input->Js.String2.split("\n")->slice(~start=0 , ~end_=-1)

type instruction = Noop | AddX

let instructions = lines->map((line) => {
  let s = line->Js.String2.split(" ")
  switch(s[0]) {
    | "addx" => (AddX, int_of_string(s[1]))
    | "noop"
    | _ => (Noop, 0)
  }
})

let generateCycles = (instructions) => {
  let x = ref(1)
  instructions->reducei((acc, (instruction, value), index) => {
    if (index == 0) {
      acc->push(x.contents)->ignore
    }
    switch instruction {
      | AddX => {
        acc->push(x.contents)->ignore
        x.contents = x.contents + value
      }
      | _ => ()
    }
    acc->push(x.contents)->ignore
    acc
  }, [])
}

let cycles = generateCycles(instructions)

let signals = cycles->mapi((value, index) => {
  value * (index + 1)
})

Js.log2("Part 1:", [20, 60, 100, 140, 180, 220]->reduce((i, j) => i + signals[j - 1], 0))

let draw = (cycles, width) => {
  let crt = cycles->mapi((x, i) => {
    let ptr = mod(i, width)
    ptr >= (x - 1) && ptr <=(x + 1) ? "#" : "."
  })

  for cell in 0 to crt->length - 1 {
    if mod(cell, width) == 0 {
      Js.log(crt->slice(~start=cell - width, ~end_=cell)->joinWith(""))
    }
  }
}

Js.log("Part 2:")

cycles->draw(40)
