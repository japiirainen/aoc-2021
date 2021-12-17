import std/os/path
import std/os/file

fun increases(xs : list<int>)
  xs.zip(xs.drop(1)).foldl(0, fun(acc, (a, b)) { if (b > a) acc + 1 else acc })
  
fun p2(xs : list<int>)
  var triples := xs.zip(xs.drop(1)).zip(xs.drop(2)).map(fun(((a,b),c)) {a+b+c})
  increases(triples)

fun main()
  var f := read-text-file((/)(".", "01.txt"))
  var lines := split(f, "\n").map(fn(x) { parse-int-default(x, 0) })
                      
  var part1 := increases(lines)
  var part2 := p2(lines)
  println("Part 1: " ++ part1.show)
  println("Part 2: " ++ part2.show)