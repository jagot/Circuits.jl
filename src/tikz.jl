module TikZ
using TikzPictures

function indent(s::String)
    map(split(s, "\n")) do line
        "  "*line
    end |> l -> join(l, "\n")
end

const TikZarg = Union{String,<:Pair{String,<:Any}}

tikz_arg(arg::String) = arg
tikz_arg(arg::Pair{String,T}) where T = "$(arg[1])=$(arg[2])"

function Base.convert(::Type{MIME"text/tikz"}, args::Vector{<:TikZarg})
    if !isempty(args)
        "[" * join(map(tikz_arg, args), ",\n") * "]"
    else
        ""
    end
end

function tikz_environment(fun::Function, environment::String, args::TikZarg...)
    args = convert(MIME"text/tikz", TikZarg[args...])
    "\\begin{$(environment)}$(args)\n"*indent(fun())*"\n\\end{$(environment)}"
end

mutable struct TikZnode{T}
    label::String
    id::String
    args::Vector{TikZarg}
    x::T
    y::T
end
TikZnode(label::String, args::TikZarg...; id="", x::T=0, y::T=0) where T =
    TikZnode{T}(label, id, TikZarg[args...], x, y)

function Base.convert(::Type{MIME"text/tikz"}, node::TikZnode{T}) where T
    args = convert(MIME"text/tikz", node.args)
    id = if !isempty(node.id)
        " ($(node.id)) "
    else
        ""
    end
    pos = if node.x != zero(T) || node.y != zero(T)
        " at ($(node.x),$(node.y)) "
    else
        ""
    end
    "\\node$(args)$(id)$(pos){$(node.label)};"
end

tikz_scope(fun::Function, args::TikZarg...) = tikz_environment(fun, "scope", args...)

export indent, TikZarg, tikz_arg, tikz_environment, TikZnode, tikz_scope
end
