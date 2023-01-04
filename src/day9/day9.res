open Js.Array2
open Node.Fs

let input = readFileSync("input.txt", #ascii)

let lines = input->Js.String2.split("\n")->slice(~start=0 , ~end_=-1)

type dir = Up | Right | Down | Left | None

let instructions = lines->map((line) => {
  let s = line->Js.String2.split(" ")
  switch s[0] {
    | "U" => (Up, int_of_string(s[1]))
    | "R" => (Right, int_of_string(s[1]))
    | "D" => (Down, int_of_string(s[1]))
    | "L" => (Left, int_of_string(s[1]))
    | _ => (None, 0)
  }
})

let dirToVector = (dir) => {
  switch dir {
    | Up => {
      (-1, 0)
    }
    | Right => {
      (0, 1)
    }
    | Down => {
      (1, 0)
    }
    | Left => {
      (0, -1)
    }
    | _ => (0, 0)
  }
}

let vectorDistance = (~b=(0, 0), (ax, ay)) => {
  let (bx, by) = b
  Js.Math.max_int(Js.Math.abs_int(ax - bx), Js.Math.abs_int(ay - by))
}

let addVector = ((ax, ay), (bx, by)) => {
  (ax + bx, ay + by)
}

let subVector = ((ax, ay), (bx, by)) => {
  (ax - bx, ay - by)
}

let signedVector = ((ax, ay)) => {
  (Js.Math.sign_int(ax), Js.Math.sign_int(ay))
}

let process = (instructions) => (ropeLength) => {
  let tracks = Js.Dict.empty()
  let rope = Belt.Array.make(ropeLength, (0, 0))

  instructions->forEach(((instruction, steps)) => {
    for _ in 1 to steps {
      rope[0] = addVector(rope[0], dirToVector(instruction))

      for x in 1 to rope->Belt.Array.length - 1 {
        let difference = subVector(rope[x - 1], rope[x])

        if (vectorDistance(difference) > 1) {
          rope[x] = addVector(rope[x], signedVector(difference))
        }
      }

      let (tailx, taily) = rope[rope->Belt.Array.length - 1]
      let key = `${tailx->string_of_int}-${taily->string_of_int}`
      let visited = switch tracks->Js.Dict.get(key) {
        | Some(v) => v
        | None => 0
      }

      tracks->Js.Dict.set(key, visited + 1)
    }
  })

  tracks->Js.Dict.values->length
}

let p = process(instructions)

Js.log2("Part 1:", p(2))
Js.log2("Part 2:", p(10))
