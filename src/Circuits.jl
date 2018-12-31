module Circuits

using Unitful
import Unitful: ElectricalResistance, Capacitance, Inductance, Voltage, Current

using SparseArrays

using UnicodeFun
using TikzPictures

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

pin(e::Element, ::String) = throw(ArgumentError("Symbolic pins not implemented for $(e)"))
pin(e::Element, p::Integer) = p

ports(e::Element) = throw(ArgumentError("Cannot list ports for abstract Element $(e)"))
ports(e::NPort{N}) where N = 1:N

num_ports(e::Element) = length(ports(e))

symbol(::Element) = "Generic element"
value(::Element) = nothing

footprint(e::Element) = e.footprint

function Base.show(io::IO, e::Element)
    write(io, "$(symbol(e))$(label(e))")
    v = value(e)
    !isnothing(v) && write(io, " @ $(v)")
end

# * Physical two-port

mutable struct PhysicalTwoPort{U,S,L} <: TwoPort
    value::U
    label::L
    footprint::String
end

PhysicalTwoPort{U,S}(value::U; label::L = 1, footprint = "") where {U,S,L} =
    PhysicalTwoPort{U,S,L}(value, label, footprint)

symbol(::PhysicalTwoPort{U,S,L}) where {U,S,L} = S
value(tp::PhysicalTwoPort) = tp.value

# ** Misc physical two-ports

const Resistor{L} = PhysicalTwoPort{ElectricalResistance,:R,L}
const Capacitor{L} = PhysicalTwoPort{Capacitance,:C,L}
const Inductor{L} = PhysicalTwoPort{Inductance,:L,L}
const Diode{L} = PhysicalTwoPort{NoUnits,:D,L}
const Switch{L} = PhysicalTwoPort{NoUnits,:S,L}

# * Circuit

mutable struct Circuit
    elements::Vector{<:Element}
    connections::SparseMatrixCSC
    offsets::Vector{Int}
end

Circuit() = Circuit(Vector{Element}(), spzeros(Bool,0,0), Vector{Int}())

function Base.push!(c::Circuit, e::Element)
    m = size(c.connections, 1)
    n = num_ports(e)
    M = m+n
    
    push!(c.elements, e)
    push!(c.offsets, n)
    
    connections = spzeros(Bool,M,M)
    copyto!(view(connections, 1:m, 1:m), c.connections)
    c.connections = connections
    
    c
end

offset(c::Circuit, i::Integer) = i > 1 ? c.offsets[i-1] : 0
function offset(c::Circuit, e::Element)
    i = findfirst(isequal(e), c.elements)
    isnothing(i) && throw(ArgumentError("$(e) not present in circuit $(c)"))
    offset(c, i)
end

function Base.show(io::IO, ::MIME"text/plain", c::Circuit)
    write(io, "Circuit with ")
    show(io, "text/plain", c.elements)
end

function connect!(c::Circuit, a::Element, ap::Pin, b::Element, bp::Pin)
    ap = pin(a, ap)
    bp = pin(b, bp)
    
    ai = offset(c, a) + ap
    bi = offset(c, b) + bp
    
    @info "Connecting pin $(ap) of $(a) to pin $(bp) of $(b)"

    c.connections[ai,bi] = true
    c.connections[bi,ai] = true
    
    c
end

# * Exports

export Element, Resistor, Capacitor, Inductor,
    Circuit, connect!

end # module
