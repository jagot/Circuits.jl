module Circuits

using Unitful
import Unitful: ElectricalResistance, Capacitance, Inductance, Voltage, Current

using SparseArrays

using UnicodeFun

include("tikz.jl")

if VERSION < v"1.1-DEV"
    isnothing(::Nothing) = true
    isnothing(::Any) = false
end

# * Generic element

abstract type Element end
abstract type NPort{N} <: Element end
const TwoPort = NPort{2}

const Pin = Union{<:Integer,String}

is_subscriptable(::Integer) = true
is_subscriptable(c::Char) = c ∈ "0123456789+-=()aeioruvxβγρφ"
is_subscriptable(s::AbstractString) = all(map(is_subscriptable, collect(s)))
is_subscriptable(::Any) = false

label(e::Element) = is_subscriptable(e.label) ? to_subscript(e.label) : string(e.label)

pins(e::Element) = throw(ArgumentError("Cannot list pins for abstract Element $(e)"))
pins(e::NPort{N}) where N = 1:N

symbolic_pins(e::Element) = throw(ArgumentError("Cannot list symbolic pins for abstract Element $(e)"))

function pin(e::Element, p::String)
    i = findfirst(isequal(p), symbolic_pins(e))
    isnothing(i) && throw(ArgumentError("Pin $i not present for element $(e)"))
    i
end
pin(e::Element, p::Integer) = p

num_pins(e::Element) = length(pins(e))

symbol(::Element) = "Generic element"
value(::Element) = nothing

footprint(e::Element) = e.footprint

function Base.show(io::IO, e::Element)
    write(io, "$(symbol(e))$(label(e))")
    v = value(e)
    !isnothing(v) && write(io, "($(v))")
end

# * Physical two-port

mutable struct PhysicalTwoPort{U,S,polar,L} <: TwoPort
    value::U
    label::L
    footprint::String
end
PhysicalTwoPort{U,S,polar}(value::U; label::L = 1, footprint = "") where {U,S,polar,L} =
    PhysicalTwoPort{U,S,polar,L}(value, label, footprint)

symbol(::PhysicalTwoPort{U,S,L}) where {U,S,L} = S
value(tp::PhysicalTwoPort) = tp.value

# ** Misc physical two-ports

const Resistor{L} = PhysicalTwoPort{ElectricalResistance,:R,false,L}
const Capacitor{L} = PhysicalTwoPort{Capacitance,:C,false,L}
const PolarCapacitor{L} = PhysicalTwoPort{Capacitance,:C⁺,true,L}
const Inductor{L} = PhysicalTwoPort{Inductance,:L,false,L}

const Diode{L} = PhysicalTwoPort{Nothing,:D,true,L}
Diode(;kwargs...) = Diode(nothing; kwargs...)

const Switch{L} = PhysicalTwoPort{Bool,:S,false,L}
Switch(;kwargs...) = Switch(false; kwargs...)
value(s::Switch) = s.value ? "on" : "off"

symbolic_pins(::PhysicalTwoPort{U,S,true,L}) where {U,S,L} = ["+", "-"]

# *** Two-port TikZ symbols
tikz_symbol(r::Resistor{L}) where {L} = "resistor"
tikz_symbol(r::Capacitor{L}) where {L} = "capacitor"
tikz_symbol(c::PolarCapacitor{L}) where L = "capacitor"
tikz_symbol(i::Inductor{L}) where L = "inductor"
tikz_symbol(d::Diode{L}) where L = "diode"
tikz_symbol(s::Switch{L}) where L = s.value ? "break contact" : "make contact"

# * Circuit

mutable struct Circuit
    elements::Vector{<:Element}
    connections::SparseMatrixCSC
    offsets::Vector{Int}
end

Circuit() = Circuit(Vector{Element}(), spzeros(Bool,0,0), Vector{Int}())

findelement(c::Circuit, e::Element) = findfirst(isequal(e), c.elements)

function Base.push!(c::Circuit, e::Element)
    !isnothing(findelement(c, e)) &&
        throw(ArgumentError("Element $e already present in circuit $c"))

    m = size(c.connections, 1)
    n = num_pins(e)
    M = m+n
    o = length(c.offsets) > 0 ? n + c.offsets[end] : n

    push!(c.elements, e)
    push!(c.offsets, o)

    connections = spzeros(Bool,M,M)
    copyto!(view(connections, 1:m, 1:m), c.connections)
    c.connections = connections

    c
end

offset(c::Circuit, i::Integer) = i > 1 ? c.offsets[i-1] : 0
function offset(c::Circuit, e::Element)
    i = findelement(c, e)
    isnothing(i) && throw(ArgumentError("$(e) not present in circuit $(c)"))
    offset(c, i)
end

function Base.show(io::IO, ::MIME"text/plain", c::Circuit)
    write(io, "Circuit with ")
    show(io, "text/plain", c.elements)
end

function Base.show(io::IO, c::Circuit)
    write(io, "Circuit(")
    show(io, c.elements)
    write(io, ")")
end

function connect!(c::Circuit, a::Element, ap::Pin, b::Element, bp::Pin)
    api = pin(a, ap)
    bpi = pin(b, bp)

    ai = offset(c, a) + api
    bi = offset(c, b) + bpi

    @debug "Connecting pin $(ap)($(ai)) of $(a) to pin $(bp)($(bi)) of $(b)"

    c.connections[ai,bi] = true
    c.connections[bi,ai] = true

    c
end

function unique_labels!(c::Circuit)
    d = Dict{Type{<:Element},Int}()
    for e ∈ c.elements
        e.label isa Integer || throw(ArgumentError("Cannot uniquify symbolic labels"))
        et = typeof(e)
        i = get(d, et, 0) + 1
        e.label = i
        d[et] = i
    end
    c
end

function unique_nodes(c::Circuit)
    connected_nodes = reduce(|, c.connections, dims=1) |> vec |> findall
    @debug "Number of connections: $(length(connected_nodes))"

    nodes = collect(1:size(c.connections,1))

    for m in connected_nodes
        for n in findall(c.connections[:,m])
            n ≤ m && continue
            nodes[n] = nodes[m]
        end
    end

    nodes
end
# * TikZ

function Base.convert(TikzPicture, c::Circuit; kwargs...)
    all(map(e -> e isa Circuits.TwoPort, c.elements)) ||
        throw(ArgumentError("Don't know how to generate TikZ diagrams consisting of anything but two-ports"))

    nodes = unique_nodes(c)
    connection_count = map(nodes) do n
        length(findall(isequal(n), nodes))
    end
    graph = map(1:2:length(nodes)) do i
        a,b = nodes[i],nodes[i+1]
        a == b && @info("Two-port nodes identical")
        e = c.elements[cld(i,2)]
        style = connection_count[i+1] > 2 ? "[mark]" : ""
        "n$(a) --[$(tikz_symbol(e))] n$(b)$(style)"
    end |> n -> join(n, ",\n")

    graph = "\\graph[empty nodes]{$(graph)};"

    options = join(map(tikz_arg, TikZarg["circuit ee IEC",
                                         "every info/.style"=>"{font=\\footnotesize}",
                                         "small circuit symbols",
                                         "layered layout",
                                         "rotate"=>90,
                                         # "xscale"=>-1,
                                         "mark/.style"=>"{fill,circle}",
                                         "mark/.default"=>"",
                                         "every node/.style"=>"inner sep=0pt,minimum size=0pt"]), ",\n")
    TikzPicture(graph; options=options, kwargs...)
end

TikzPictures.save(f::S, c::Circuit; kwargs...) where {S<:TikzPictures.SaveType} =
    save(f, convert(TikzPicture, c; preamble=preamble, kwargs...))

# * Exports

export Element, Resistor, Capacitor, PolarCapacitor, Inductor, Diode, Switch,
    Circuit, connect!, unique_labels!

end # module
