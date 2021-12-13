using ImageFiltering: centered, Fill, imfilter
using IterTools: iterated
using Pipe: @pipe

parse_line(line) = @pipe collect(line) |> parse.(Int, _)

load_data() = @pipe readlines("11.txt") |> parse_line.(_) |> hcat(_...)'

fixpoint(f, x) = @pipe f(x) |> (_ == x ? x : fixpoint(f, _))

substep((X, M)) = @pipe (X .> 9) .- M |> imfilter(_, centered(ones(Int, 3, 3)), Fill(0)) |> (X .+ _, X .> 9)
step(X) = @pipe fixpoint(substep, (X .+ 1, X .* 0))[1] |> _ .* (_ .<= 9)

eachflashcount(X) = @pipe iterated(step, X) |> Iterators.map(X -> sum(X .== 0), _)

solve_a() = @pipe load_data() |> eachflashcount |> Iterators.take(_, 101) |> sum

solve_b() = @pipe load_data() |> eachflashcount |> Iterators.takewhile(n -> n < 100, _) |> collect |> length

println("11A: ", solve_a())
println("11B: ", solve_b())
