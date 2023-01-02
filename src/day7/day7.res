open Js.Array2
open Node.Fs

let input = readFileSync("input.txt", #ascii)

let lines = input->Js.String2.split("\n")->slice(~start=0 , ~end_=-1)

type line = Cd(string) | Ls | File(int) | Dir(string)

let parseLine = (line) => {
  let s = line->Js.String2.split(" ")
  switch s[0] {
    | "$" if s[1] == "cd" => Cd(s[2])
    | "$" if s[1] == "ls" => Ls
    | "dir" => Dir(s[1])
    | _ => File(int_of_string(s[0]))
  }
}

let currentDir = ref("/")
let dirs = Js.Dict.empty()

forEach(lines->map(parseLine), (line) => {
  switch line {
    | File(size) =>
      let s = switch dirs->Js.Dict.get(currentDir.contents) {
        | Some(val) => val
        | None => 0
      }
      dirs->Js.Dict.set(currentDir.contents, s + size)
    | Dir(name) =>
      dirs->Js.Dict.set(currentDir.contents ++ name ++ "/", 0)
    | Cd(dir) =>
      if (dir == "..") {
        currentDir.contents = currentDir.contents->Js.String2.split("/")->slice(~start=0 , ~end_=-2)->joinWith("/") ++ "/"
      } else if (dir == "/") {
        currentDir.contents = "/"
      } else {
        currentDir.contents = currentDir.contents ++ dir ++ "/"
      }
    | _ => ()
  }
})

forEach(dirs->Js.Dict.entries, ((dir, size)) => {
  let s = dir->Js.String2.split("/")
  s->pop->ignore
  while (s->length > 1) {
    s->pop->ignore
    let parentKey = s->joinWith("/") ++ "/"
    let newSize = switch dirs->Js.Dict.get(parentKey) {
      | Some(val) => val + size
      | _ => size
    }
    dirs->Js.Dict.set(parentKey, newSize)
  }
})

let result = reduce(dirs->Js.Dict.entries, (acc, (dir, size)) => {
  if (size < 100000) {
    let s = switch (dirs->Js.Dict.get(dir)) {
      | Some(val) => val
      | _ => 0
    }
    acc + s
  } else {
    acc
  }
}, 0)

Js.log2("Part 1:", result)

let totalSize = 70000000

let neededSpaceForUpdate = 30000000

let totalUsedSize = switch dirs->Js.Dict.get("/") {
  | Some(val) => val
  | _ => 0
}

let availableSpace = totalSize - totalUsedSize

let neededSpace = neededSpaceForUpdate - availableSpace

let result2: array<int> = filter(dirs->Js.Dict.entries, ((_, size)) => {
    size >= neededSpace
})->map(((_, size)) => size)->sortInPlaceWith((n1, n2) => n1 - n2)

Js.log2("Part 2:", result2[0])
