function readinput(filename::String)::Tuple{Array{Int64}, Array{Int64}}
    open(filename) do f
        lines = readlines(f)
        left = Array{Int64}(undef, length(lines)) 
        right = Array{Int64}(undef, length(lines)) 
        for (i, l) in enumerate(lines)
            (a, b) = split(l)
            left[i] = parse(Int64, a)
            right[i] = parse(Int64, b)
        end
        
        return (left, right)
    end
end


function part1(left::Array{Int64}, right::Array{Int64})::Int64
    res = 0
    for (a, b) in zip(sort(left), sort(right))
        res += abs(a - b)
    end
    return res
end

function part2(left::Array{Int64}, right::Array{Int64})::Int64
    res = 0
    for a in left
        for b in right
            if a == b
                res += a
            end
        end
    end
    return res
end

function main()
    left, right = readinput("../input.txt")
    println(part1(left, right))
    println(part2(left, right))
end


main()
