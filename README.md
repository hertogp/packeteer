# Packeteer

Helper library to make encoding/decoding binaries easier.

When a binary has a fixed layout, [`fixed/2`](`Packeteer.fixed/2`) generates
the encode resp. decode function based on a given name and options, which must
include a list of [_primitive_](`Packeteer#primitives`) field definitions.

## Fixed fields

This is what a DNS header encoder/decoder could look like:

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
`encode/1` resp. `decode/2`.  The functions are generated with a docstring. For
`encode/1` it would show the list of field definitions, their default values
and the bitstring expression used for encoding.  For `decode/2` it would show
basically the same.  See [`fixed/2`](`Packeteer.fixed/2`) for another example.

## Mixed fields

If you need a little more _umpf_ for decoding a particular piece of a binary,
[`mixed/2`](`Packeteer.mixed/2`)  is your friend.  It allows for mixing
in custom encoder/decoder's.  Suppose you had a `Helper` library exposing
some of those custom encoder/decoder's you could, e.g. for a DNS question
part, do something like this:

```elixir
defmodule Question do
  import Packeteer
  import Helper

  mixed("",
    fields: [
      name: {&name_enc/2, &name_dec/4},
      type: uint(16),
      class: uint(16)
    ],
    defaults: [
      type: 1,  # :A
      class: 1, # :IN
    ]
  )
end
```

The above would again define an encode resp. decode function as:

```elixir
Question.encode/1
Question.decode/2
```

The custom `name_enc/2` encoder from `Helper`  would receive the field name
(`:name`) and the value to encode.  Including the name in the call makes it
possible for a custom encoder to act differently for different field names.

The custom `name_dec/4` decoder from `Helper` would receive the field name
(:name), the keyword list decoded thus far (here that would be `[]` since its
the first encoder/decoder), the offset into the binary and the binary itself.
It is required to return `{offset, kw, binary}`, where kw is the updated
keyword list of field,value-pairs.

## options

Both [`fixed/2`](`Packeteer.fixed/2`) and [`mixed/2`](`Packeteer.mixed/2`)
support a number of options, amongst wich:

- `:pattern`, to define an extra first argument in the call signature
- `:before_encode`, to manipulate field,value-pairs before actual encoding
- `:after_decode`, to manipulate the returned results
- `:docstr`, to allow/prevent docstrings to be generated
- `:private`, to define the encode/decode functions as private functions
- `:silent`, to print out the defined functions during compilation (or not)

See [`fixed/2`](`Packeteer.fixed/2`) for more information.

## related work
This is yet another library to aid in the development of custom binary
encoder/decoders.  Other work on this front include:

- [bitcraft](https://hexdocs.pm/bitcraft/Bitcraft.html) - A toolkit for
  encoding/decoding bit strings and DSL for binary protocols bit blocks.

- [bifrost](https://hexdocs.pm/bifrost/Bifrost.html) - Provides functions to
  create composable and bidirectional serializers.

- [bin_format](https://hexdocs.pm/bin_format/extra-api-reference.html) -
  Generates the Elixir code for handling binary formats through structs.

- [codec](https://hexdocs.pm/codec/api-reference.html) - Facilitate the
  development of layered binary protocols while mostly sticking with the Elixir
  bit field syntax.

So why Packeteer?  Why not?

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

