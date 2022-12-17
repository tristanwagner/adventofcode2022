open Js.Array2
open Node.Fs

let input = readFileSync("input.txt", #ascii)

let rounds = Js.String.split("\n", input)

type outcome = Won | Lost | Draw | None

type shape = Paper | Rock | Scissors | None

let getRoundResult = (player, enemy) => {
  switch(player, enemy) {
    | (Paper, Scissors) => Lost
    | (Scissors, Paper) => Won
    | (Paper, Rock) => Won
    | (Rock, Paper) => Lost
    | (Rock, Scissors) => Won
    | (Scissors, Rock) => Lost
    | _ => Draw
  }
}

let getScoreFromShape = (shape) => {
  switch(shape) {
    | Rock => 1
    | Paper => 2
    | Scissors => 3
    | _ => 0
  }
}

let getShapeFromLetter = (letter) => {
  switch(letter) {
    | "A"
    | "X" => Rock
    | "B"
    | "Y" => Paper
    | "C"
    | "Z" => Scissors
    | _ => None
  }
}

let getScoreFromResult = (result) => {
  switch(result) {
    | Won => 6
    | Draw => 3
    | _ => 0
  }
}

let score = reduce(rounds, (acc, round) => {
  if (Js.String.length(round) > 0) {
    let letters = Js.String.split(" ", round)
    let enemy = letters[0]
    let player = letters[1]
    let enemyShape = getShapeFromLetter(enemy)
    let playerShape = getShapeFromLetter(player)
    let result = getRoundResult(playerShape, enemyShape)
    let playerScoreFromShape = getScoreFromShape(playerShape)
    let playerScoreFromResult = getScoreFromResult(result)
    let currentScore = playerScoreFromShape + playerScoreFromResult
    acc + currentScore
  } else  {
    acc + 0
  }
} ,0)

Js.log2("Part 1:", score)

let getResultFromLetter = (letter) => {
  switch(letter) {
    | "X" => Lost
    | "Y" => Draw
    | "Z" => Won
    | _ => None
  }
}

let getShapeFromResult = (enemy, result) => {
  switch(enemy, result) {
    | (Rock, Won) => Paper
    | (Rock, Lost) => Scissors
    | (Rock, Draw) => Rock
    | (Paper, Won) => Scissors
    | (Paper, Lost) => Rock
    | (Paper, Draw) => Paper
    | (Scissors, Won) => Rock
    | (Scissors, Lost) => Paper
    | (Scissors, Draw) => Scissors
    | _ => None
  }

}

let score2 = reduce(rounds, (acc, round) => {
  if (Js.String.length(round) > 0) {
    let letters = Js.String.split(" ", round)
    let enemy = letters[0]
    let player = letters[1]
    let enemyShape = getShapeFromLetter(enemy)
    let endRound = getResultFromLetter(player)
    let playerShape = getShapeFromResult(enemyShape, endRound)
    let playerScoreFromShape = getScoreFromShape(playerShape)
    let playerScoreFromResult = getScoreFromResult(endRound)
    let currentScore = playerScoreFromShape + playerScoreFromResult
    acc + currentScore
  } else  {
    acc + 0
  }
} ,0)

Js.log2("Part 2:", score2)
