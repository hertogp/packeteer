# Packeteer

Helper library to make encoding/decoding binaries easier.

When a binary has a fixed layout, [`fixed/2`](`Packeteer.fixed/2`) generates
the encode resp. decode function based on a given name and options, which must
include a list of field definitions.

## Fixed fields

```elixir
defmodule Header do
  import Packeteer

  fixed("",
    fields: [
      id: uint(16),
      qr: bits(1),
      opcode: bits(4),
      aa: bits(1),
      tc: bits(1),
      rd: bits(1),
      ra: bits(1),
      z: bits(1),
      ad: bits(1),
      cd: bits(1),
      rcode: bits(4),
      qdc: uint(16),
      anc: uint(16),
      nsc: uint(16),
      arc: uint(16)
    ],
    defaults: [
      qr: 0,     # query
      opcode: 0, # query
      rd: 1,     # recursion desired
      z: 0,      # not used
      cd: 0,     # not check disabled
      rcode: 0,  # no error
    ])

end
```

The above would create two functions in the `Header` module:

```elixir
Header.encode/1 #- takes a keyword list with fields and values, returns a binary
Header.decode/2 #- takes an offset and a binary, returns {offset, kw, binary}
```

Since the first argument (name) is `""`, the resulting names are simply
`encode` resp. `decode`.  See [`fixed/2`](`Packeteer.fixed/2`) for another
example.

## Mixed fields

If some fields cannot be expressed as a bitstring expression because the
decoding is more involved than a bitstring match operation,
[`mixed/2`](`Packeteer.mixed/2`)  is your friend.  It allows for mixing
in custom encoder/decoder's.





## related work
This is yet another library to aid in the development of custom binary
encoder/decoders.  Other work on this front include:
- [bitcraft](https://hexdocs.pm/bitcraft/Bitcraft.html)
- [bifrost](https://hexdocs.pm/bifrost/Bifrost.html)
- [bin_format](https://hexdocs.pm/bin_format/extra-api-reference.html)
- [codec](https://hexdocs.pm/codec/api-reference.html)


## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `packeteer` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:packeteer, "~> 0.1.0"}
  ]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at <https://hexdocs.pm/packeteer>.

