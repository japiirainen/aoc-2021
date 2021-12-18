import std/os/path
import std/os/file

fun increases(xs : list<int>)
  xs.zip(xs.drop(1)).foldl(0, fn(acc, (a, b)) { if (b > a) acc + 1 else acc })
  
fun main()
  var lines := read-text-file((/)(".", "01.txt"))
                .split("\n")
                .map(fn(x) { parse-int-default(x, 0) })
                      
  var part1 := lines.increases
  var part2 := lines
                .zip(lines.drop(1))
                .zip(lines.drop(2))
                .map(fun(((a,b),c)) { a+b+c })
                .increases
  println("Part 1: " ++ part1.show)
  println("Part 2: " ++ part2.show)