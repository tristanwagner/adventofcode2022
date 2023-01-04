open Js.Array2
open Node.Fs

let input = readFileSync("input.txt", #ascii)

let lines = input->Js.String2.split("\n")->slice(~start=0 , ~end_=-1)

let grid = lines->reduce((acc, current) => {
  acc->push(current->Js.String2.split("")->map((cell) => int_of_string(cell)))->ignore
  acc
}, [])

let edge = ((grid->length * 2) - 4) + (grid[0]->length * 2)

let dict = Js.Dict.empty()

for x in 1 to grid->length - 2 {
  for y in 1 to grid[x]->length - 2 {
    // for each tree not on the edge
    let tree = grid[x][y]

    // left
    // y - n
    let visibleLeft = ref(true)

    for l in y - 1 downto 0 {
      if (grid[x][l] >= tree) {
        visibleLeft.contents = false
      }
    }

    // right
    // y + n
    let visibleRight = ref(true)

    for r in y + 1 to grid[x]->length - 1 {
      if (grid[x][r] >= tree) {
        visibleRight.contents = false
      }
    }

    // top
    // x - n
    let visibleTop = ref(true)

    for t in x - 1 downto 0 {
      if (grid[t][y] >= tree) {
        visibleTop.contents = false
      }
    }

    // bottom
    // x + n
    let visibleBottom = ref(true)

    for b in x + 1 to grid->length - 1 {
      if (grid[b][y] >= tree) {
        visibleBottom.contents = false
      }
    }

    if (
        visibleLeft.contents
        || visibleRight.contents
        || visibleTop.contents
        || visibleBottom.contents
      ) {
      dict->Js.Dict.set(`${string_of_int(x)}-${string_of_int(y)}`, 1)
    }
  }
}

let visibleTrees = dict->Js.Dict.values->length + edge

Js.log2("Part 1:", visibleTrees)

let dict2 = Js.Dict.empty()

for x in 0 to grid->length - 1 {
  for y in 0 to grid[x]->length - 1 {
    let tree = grid[x][y]

    // left
    // y - n
    let blockedLeft = ref(false)
    let scoreLeft = ref(0)

    for l in y - 1 downto 0 {
      if (!blockedLeft.contents) {
        scoreLeft.contents = scoreLeft.contents + 1
        if (grid[x][l] >= tree) {
          blockedLeft.contents = true
        }
      }
    }

    // right
    // y + n
    let blockedRight = ref(false)
    let scoreRight = ref(0)

    for r in y + 1 to grid[x]->length - 1 {
      if (!blockedRight.contents) {
        scoreRight.contents = scoreRight.contents + 1
        if (grid[x][r] >= tree) {
          blockedRight.contents = true
        }
      }
    }

    // top
    // x - n
    let blockedTop = ref(false)
    let scoreTop = ref(0)

    for t in x - 1 downto 0 {
      if (!blockedTop.contents) {
        scoreTop.contents = scoreTop.contents + 1
        if (grid[t][y] >= tree) {
          blockedTop.contents = true
        }
      }
    }

    // bottom
    // x + n
    let blockedBottom = ref(false)
    let scoreBottom = ref(0)

    for b in x + 1 to grid->length - 1 {
      if (!blockedBottom.contents) {
        scoreBottom.contents = scoreBottom.contents + 1
        if (grid[b][y] >= tree) {
          blockedBottom.contents = true
        }
      }
    }

    dict2->Js.Dict.set(`${string_of_int(x)}-${string_of_int(y)}`, scoreTop.contents * scoreLeft.contents * scoreRight.contents * scoreBottom.contents)
  }
}

let result2: array<int> = dict2->Js.Dict.values->sortInPlaceWith((n1, n2) => n2 - n1)

Js.log2("Part 2:", result2[0])
