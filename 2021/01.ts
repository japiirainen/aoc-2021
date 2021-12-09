const lines = (await Deno.readTextFile('01.txt')).split('\n').map(Number)

const p1 = lines
  .map((x, i) => [x, lines[i + 1]])
  .filter(([x, y]) => x < y).length

const p2 = lines
  .flatMap((_, i) => (i > 1 ? [lines[i - 2] + lines[i - 1] + lines[i]] : []))
  .filter((x, i, arr) => x > arr[i - 1]).length

console.log(p1)
console.log(p2)
