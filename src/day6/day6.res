open Js.Array2
open Node.Fs

let input = readFileSync("input.txt", #ascii)

let letters = input->Js.String2.split("")

let uniqueLetters = (a) => a->Belt.Set.String.fromArray->Belt.Set.String.size == a->length

let detectMarker = (message, markerLength) => {
  let result = ref(0)

  for i in 0 to message->length - 1 {
    if (i - markerLength >= 0 && result.contents == 0) {
      let part = message->slice(~start=i - markerLength, ~end_=i)
      if (uniqueLetters(part)) {
        result.contents = message->slice(~start=0, ~end_=i)->length
      }
    }
  }
  result.contents
}

Js.log2("Part 1:", detectMarker(letters, 4))

Js.log2("Part 2:", detectMarker(letters, 14))
