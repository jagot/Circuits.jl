using CSV
using DataFrames

function bom(filename::String, c::Circuit, query::Bool=false)
    df = DataFrame(ref=String[], manf_num=Union{String,Missing}[])
    for e in c.elements
        push!(df, (string(e), get(e.meta, :manf_num, missing)))
    end
    count_parts = dropmissing(by(df, :manf_num, :ref => length),
                              disallowmissing=true)
    rename!(count_parts, :manf_num => Symbol("manf#"), :ref_length => :qty)
    CSV.write(filename, count_parts)
    if query
        xl = rsplit(filename, ".", limit=2)[1]*".xlsx"
        isfile(xl) && rm(xl)
        run(`kicost -i $(filename) --eda csv`)
        xl
    else
        filename
    end
end

bom(c::Circuit, query::Bool=false) = bom("bom.csv", c, query)
