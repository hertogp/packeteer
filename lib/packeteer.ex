defmodule Packeteer do
  @moduledoc """
  Declaratively create encode/decode functions for binary data.

  """

  @primitives [:uint, :sint, :float, :bytes, :binary, :bits, :bitstring, :utf8, :utf16, :utf32]

  # [[ PRIMITIVES ]]

  @doc """
  Returns a quoted fragment for an unsigned integer for given `size` in _bits_.

  The `size` argument can also be a simple expression like `:a * 8` (yielding an integer) when `:a` is an
  earlier field that decodes an integer.

  Pass in an optional second argument (:little or :native) if the
  default doesn't suit your needs.

  The fragment is used as the right hand side of an unsigned integer segment
  in a bitstring match expression.

  """
  @doc section: :fragment
  def uint(size, endian \\ :big) do
    endian = var(endian)
    size = var(size)
    quote(do: integer - size(unquote(size)) - unquote(endian))
  end

  @doc """
  Returns a quoted fragment for an signed integer for given `size` in _bits_.

  The `size` argument can also be a simple expression like `:a * 8` when `:a` is an
  earlier field that decodes an integer.

  Pass in an optional second argument (`:little` or `:native`) if the
  default doesn't suit your needs.

  The fragment is used as the right hand side of a signed integer segment
  in a bitstring match expression.

  """
  @doc section: :fragment
  def sint(size, endian \\ :big) do
    endian = var(endian)
    size = var(size)
    quote(do: integer - size(unquote(size)) - unquote(endian) - signed)
  end

  @doc """
  Returns quoted fragment for a float for given `size` in _bits_.

  The `size` argument must be one of `[16, 32, 64]`.

  Pass in an optional second argument (`:little` or `:native`) if the
  default doesn't suit your needs.

  The fragment is used as the right hand side of a float segment
  in a bitstring match expression.

  """
  @doc section: :fragment
  def float(size, endian \\ :big) do
    endian = var(endian)
    size = var(size)
    quote(do: integer - size(unquote(size)) - unquote(endian) - signed)
  end

  @doc """
  Shorthand for `binary/1`.

  """
  @doc section: :fragment
  def bytes(size \\ nil) do
    if size,
      do: quote(do: bytes - size(unquote(var(size)))),
      else: quote(do: bytes)
  end

  @doc """
  Returns a quoted fragment for a binary of given `size` in _bytes_.

  The `size` argument can also be a simple expression like `:a * 8` when `:a` is an
  earlier field that decodes an integer. When omitted, matches the remaining
  bytes.

  The fragment is used as the right hand side of a binary segment
  in a bitstring match expression.

  """
  @doc section: :fragment
  def binary(size \\ nil),
    do: bytes(size)

  @doc """
  Shorthand for `bitstring/1`.

  """
  @doc section: :fragment
  def bits(size \\ nil) do
    if size,
      do: quote(do: bits - size(unquote(var(size)))),
      else: quote(do: bits)
  end

  @doc """
  Returns a quoted fragment for a bitstring of given `size` in _bits_.

  The `size` argument can also be a simple expression like `:a * 8` when `:a` is an
  earlier field that decodes an integer. When omitted, matches the remaining
  bits.

  The fragment is used as the right hand side of a bitstring segment
  in a bitstring match expression.

  """
  @doc section: :fragment
  def bitstring(size \\ nil),
    do: bits(size)

  @doc """
  Returns a quoted fragment for a utf8 codepoint.

  Utf8 codepoints are encoded in (or decoded from) 1..4 bytes. The bitsyntax
  only supports an endianness modifier, so this can encode or decode only 1 utf
  codepoint at a time.

  The default for `endian` is `:big`, pass in either `:little` or `native`
  as appropiate.

  """
  @doc section: :fragment
  def utf8(endian \\ :big),
    do: quote(do: utf8 - unquote(endian))

  @doc """
  Returns a quoted fragment for a utf16 codepoint.

  Utf16 codepoints are encoded in (or decoded from) two 16 bit units
  (i.e. 4 bytes). The bitsyntax only supports an endianness modifier, so this
  can encode or decode only 1 utf codepoint at a time.

  The default for `endian` is `:big`, pass in either `:little` or `native`
  as appropiate.

  """
  @doc section: :fragment
  def utf16(endian \\ :big),
    do: quote(do: utf16 - unquote(endian))

  @doc """
  Returns a quoted fragment for a utf32 codepoint.

  Utf32 codepoints are encoded in (or decoded from) 32 bits (i.e. 4 bytes).
  The bitsyntax only supports an endianness modifier, so this can encode or
  decode only 1 utf codepoint at a time.  Only the first (lsb) 21 bits are
  used at the moment.

  The default for `endian` is `:big`, pass in either `:little` or `native`
  as appropiate.

  """
  @doc section: :fragment
  def utf32(endian \\ :big),
    do: quote(do: utf32 - unquote(endian))

  # [[ HELPERS ]]

  defp all?(fields) do
    # returns true if expression matches all bits
    {_name, {fld, _, arg}} = List.last(fields)
    fld in [:bits, :bitstring, :bytes, :binary] and arg in [[], [nil]]
  end

  # [[ FRAGMENTS ]]
  # - bit syntax:  <<var::spec, ..>>
  # - var::spec is a segment of the bit syntax expression
  # - spec if a fragment: the rhs of the segment.

  # turns atom's into var in an expression
  defp walk(ast) do
    Macro.prewalk(ast, fn
      v when is_atom(v) -> var(v)
      v -> v
    end)
  end

  # turn atom's into vars, keeping literals as-is
  # v could be a field expression to calculate size
  defp var(v) do
    case v do
      v when is_atom(v) -> Macro.var(v, __MODULE__)
      v when is_tuple(v) -> walk(v)
      v -> v
    end
  end

  defp fragments(fields) do
    non_p = Enum.filter(fields, fn field -> f_type(field) != :p end)

    if non_p != [] do
      msg = Enum.map(non_p, fn {field, _gf} -> "'#{field}'" end) |> Enum.join(", ")
      raise ArgumentError, "Packeteer.fixed/2 got non-primitive fields: #{msg}."
    end

    parts =
      for {n, q} <- fields do
        f = apply(__MODULE__, elem(q, 0), elem(q, 2))
        {:"::", [], [var(n), f]}
      end

    {:<<>>, [], parts}
  end

  # [[ BIND VARS ]]
  # - introduce bindings for field names to kw-list named values
  # - uses fields to ensure all var's required by the codec exist
  #   when encoding them into a binary
  defp bindings(fields) do
    for {n, _q} <- fields do
      {:=, [],
       [
         var(n),
         {{:., [from_brackets: true], [Access, :get]}, [from_brackets: true], [var(:kw), n]}
       ]}
    end
  end

  # [[ DOC STRINGS ]]

  defp pretty_fields(fields) do
    # fields is keyword list of field names and an ast for their function call.
    # So doing Macro.to_string and inserting some new lines to make it
    # formatted the same as the keyword list of default values.
    fields
    |> Macro.to_string()
    |> String.replace(", ", ",\n ")
    |> String.replace("[", "[\n ")
    |> String.replace("]", "\n]")
  end

  defp docstring(:encode, opts) do
    if opts[:private] or not opts[:docstr] do
      false
    else
      # don't want the hidden field to show up (if present)
      fields = Keyword.delete(opts[:fields] || [], :skip__)
      values = Keyword.delete(opts[:values] || [], :skip__)
      codec = fragments(fields)

      """
      Encodes #{length(fields)} _named_ fields from given `kw` keyword list to a binary
      as per field definitions below.

      Fields are encoded in the order listed in the field definition.  All _named_ fields
      must be present in `kw`-list, unless they have a defined default value.

      Field definitions:
      ```
      #{pretty_fields(fields)}
      ```

      Bitstring expression:
      ```
      #{Macro.to_string(codec)}
      ```
      #{if values == [] do
        "The encoder has no default values defined."
      else
        """
        Default values:
        ```
        #{inspect(values, pretty: true, width: 5, limit: :infinity)}
        ```
        """
      end}
      """
    end
  end

  defp docstring(:decode, opts) do
    if opts[:private] or not opts[:docstr] do
      false
    else
      # don't want the hidden field to show up
      fields = Keyword.delete(opts[:fields] || [], :skip__)
      codec = fragments(fields)

      """
      Decodes #{length(fields)} _named_ fields from given `bin` binary, starting at `offset`,
      returns `{new_offset, Keyword.t, binary}`.

      Field definitions:
      ```
      #{pretty_fields(fields)}
      ```

      Bitstring expression:
      ```
      #{Macro.to_string(codec)}
      ```
      """
    end
  end

  # [[ FIXED GENERATOR ]]

  defp before_encode(fun) do
    # return ast (or not) that calls the specified {before_encode: fun}
    if fun do
      quote do
        kw = unquote(fun).(kw)
      end
    end
  end

  defp after_decode(fun) do
    # return ast (or not) that calls the specified {after_decode: fun}
    if fun do
      quote do
        {offset, kw, bin} = unquote(fun).(offset, kw, bin)
      end
    end
  end

  defp maybe_skip(all?) do
    # return ast the adds hidden :skip__ field to always match rest bits
    # unless the field spec already has last field matching all bits
    unless all? do
      quote do
        kw = Keyword.put(kw, :skip__, "")
      end
    end
  end

  defp maybe_pattern(:encode, opts) do
    # return ast for encoder func args, optionally with extra 1st arg for
    # pattern matching:
    # - normal: x_encode(kw \\ [])
    # - pattern : x_encode(:something, kw \\ [])
    arg = [{:kw, [], Packeteer}]
    pat = opts[:pattern]

    if pat,
      do: [pat | arg],
      else: arg
  end

  defp maybe_pattern(:decode, opts) do
    # return ast for decoder func args, optionally with extra 1st arg for
    # pattern matching:
    # normal:  x_decode(offset \\ 0, bin)
    # pattern :  x_decode(:something, offset \\ 0, bin)
    # changes that to x_encode(:something, kw \\ []).  Useful if you
    arg = [{:offset, [], Packeteer}, {:bin, [], Packeteer}]
    pat = opts[:pattern]

    if pat,
      do: [pat | arg],
      else: arg
  end

  defp do_defp(ast) do
    # take the ast and turn all public functions into private functions
    Macro.prewalk(ast, fn
      {:def, x, y} -> {:defp, x, y}
      other -> other
    end)
  end

  defp do_fixed(name, opts) do
    # builds the entire ast for the fixed macro to use
    # add in some defaults
    opts =
      opts
      |> Keyword.put_new(:docstr, true)
      |> Keyword.put_new(:private, false)
      |> Keyword.put_new(:defaults, [])

    fields = opts[:fields]
    values = opts[:defaults]
    all? = all?(fields)
    maybe_skip = maybe_skip(all?)
    fields = if all?, do: fields, else: fields ++ [{:skip__, {:bits, [], []}}]
    keys = Enum.map(fields, fn {k, _} -> k end)
    vars = Enum.map(keys, fn k -> var(k) end)

    encode_doc = docstring(:encode, opts)
    encode_fun = String.to_atom("#{name}encode")
    encode_args = maybe_pattern(:encode, opts)
    before_encode = before_encode(opts[:before_encode])

    decode_doc = docstring(:decode, opts)
    decode_fun = String.to_atom("#{name}decode")
    decode_args = maybe_pattern(:decode, opts)
    after_decode = after_decode(opts[:after_decode])

    codec = fragments(fields)
    binds = bindings(fields)

    qq =
      quote do
        @doc unquote(encode_doc)
        def unquote(encode_fun)(unquote_splicing(encode_args)) when is_list(kw) do
          try do
            unquote(before_encode)
            kw = Keyword.merge(unquote(values), kw)
            unquote(maybe_skip)
            unquote_splicing(binds)
            unquote(codec)
          rescue
            error -> {:error, Exception.message(error)}
          end
        end

        @doc unquote(decode_doc)
        def unquote(decode_fun)(unquote_splicing(decode_args)) when is_binary(bin) do
          try do
            <<_::bits-size(offset), rest::bits>> = bin
            unquote(codec) = rest
            kw = Enum.zip(unquote(keys), unquote(vars))
            skipped = kw[:skip__] || <<>>
            offset = bit_size(bin) - bit_size(skipped)
            kw = Keyword.delete(kw, :skip__)
            unquote(after_decode)
            {offset, kw, bin}
          rescue
            error -> {:error, Exception.message(error)}
          end
        end
      end

    if opts[:private], do: do_defp(qq), else: qq
  end

  @doc """
  Defines encode/decode functions for given `name` and `opts`, which must
  include a list of [_primitive_](#primitives) fields.

  The following encode/decode functions will be defined in the calling module:
  - `\#{name}encode/1` and `\#{name}decode/2`, or
  - `\#{name}encode/2` and `\#{name}decode/3`

  whose signatures (assuming name is "name_") are:
  ```
  name_encode(Keyword.t) :: binary | {:error, binary}
  name_decode(non_neg_integer, binary) :: {non_neg_integer, Keyword.t, binary} | {:error, binary}

  # or when using the option `pattern: literal`:
  name_encode(literal, Keyword.t) :: binary | {:error, binary}
  name_decode(literal, non_neg_integer, binary) :: {non_neg_integer, Keyword.t, binary} | {:error, binary}
  ```

  Where the [literal](https://hexdocs.pm/elixir/typespecs.html#literals). is an
  expression that is its own ast.  If some module M has only a single call
  to `fixed/2`, using `name = ""` will define: `M.encode(kw)` and
  `M.decode(offset, bin)`.

  - `:fields` a mandatory list of [primitive](#primitives) field definitions
  used to construct the bitstring match expression for both the encode and
  decode functions. Hence, the order of field definitons in this list is
  significant.

  Possible extra options include:

  - `:defaults`, a keyword list specifying default values for one or more
  fields, used by the encode function when called with fields missing. Note
  that fields with the same name will encode to the first default value given
  the nature of keyword lists.

  - `:before_encode`, either an anonymous function or a function reference
  whose signature is
  ```
  fun(Keyword.t) :: Keyword.t
  ```
  If specified, this function will be called with the original keyword arguments
  supplemented with any default values for fields that were omitted in the call
  to the encode function, prior to the actual encoding.

  - `:after_decode`, either an anonymous function or a function reference whose
  signature is
  ```
  fun(non_neg_integer, Keyword.t, binary) :: (non_neg_integer, Keyword.t, binary)
  ```
  If specified, this function will be called with the results of decoding and its
  return values will be used as the final result of the decoder function.

  - `:docstr`, either `true` (the default) or `false`, specify whether
  or not docstrings are to be generated for the encode/decode functions. By
  default the encode/decode functions are generated as public functions,
  setting `docstr` to `false` allows for eliminating them from documentation
  while still be available as public functions in your en/decoder module.

  - `:private`, either `true` or `false` (default), specify whether the
  encode/decode functions are defined as private or public functions. If
  `true`, this overrides the `:docstr` option without warning.

  - `:pattern`, a literal expression that becomes the, additional, first argument of the
  encode and decode functions.  Allows for creating multiple encode/decode
  functions with the same name.  Additional, manual catch-all encode/decode
  functions might be required to handle any `FunctionClauseError`.  Note that if
  the generated encode/decode functions are private, the manual encode/decode
  function names need to be different.

  ## Example

  A contrived, but simple, example would be to decode an unsigned integer whose
  width is specified by a preceeding byte as a multiple of 4 bits, followed by
  a binary of 5 bytes.

      iex> mod = \"""
      ...> defmodule M do
      ...>  import Packeteer
      ...>  fixed("",
      ...>      fields: [
      ...>        len: uint(8),
      ...>        val: uint(:len * 4),
      ...>        str: binary(5)
      ...>     ],
      ...>     defaults: [
      ...>       str: "stuff"
      ...>     ],
      ...>     docstr: false
      ...>  )
      ...> end
      ...> \"""
      iex> [{m, _}] = Code.compile_string(mod)
      iex> bin = m.encode(len: 4, val: 65535) <> "more stuff"
      <<4, 255, 255, "stuff", "more stuff">>
      iex> m.decode(0, bin)
      {64, [len: 4, val: 65535, str: "stuff"], <<4, 255, 255, "stuff", "more stuff">>}
      iex> <<_::bits-size(64), more::binary>> = bin
      iex> more
      "more stuff"

  """
  defmacro fixed(name, opts) do
    qq = do_fixed(name, opts)
    # make this an option: unless opts[:silent], do: IO.puts...
    # IO.puts(Macro.to_string(qq))
    qq
  end

  # [[ FLUID GENERATOR ]]

  defp f_type({_fieldname, v}) do
    # check type of value for a given field
    # - :p means it is a primitive
    # - :f means a user supplied function
    if elem(v, 0) in @primitives, do: :p, else: :f
  end

  # group fields by type in order to turn consecutive primitives into a single
  # private expression using bit syntax (i.e. a single fixed encoder/decoder)
  # returns {funcs, defs}
  # - funcs, list of function calls, including new, private fixed en/decoder
  # - defs, list of ast's that will define the private fixed en/decoders
  defp consolidate(fields, opts) do
    # this is where consolidation starts, only used by `do_fluid`
    annotated_fields =
      for field <- fields do
        {f_type(field), field}
      end
      |> Enum.chunk_by(fn t -> elem(t, 0) end)

    consolidate(annotated_fields, opts, [], [])
  end

  defp consolidate([], _opts, funcs, defs),
    do: {funcs, defs}

  defp consolidate([[{:f, _} | _] = h | tail], opts, funcs, defs) do
    # plain function calls, so move on
    flist = for {:f, f} <- h, do: f
    consolidate(tail, opts, funcs ++ flist, defs)
  end

  defp consolidate([[{:p, _} | _] = h | tail], opts, funcs, defs) do
    # turn list of primitives into single fixed encoder/decoder pair
    plist = for {:p, p} <- h, do: p
    klist = for {k, _} <- plist, do: k

    # private func name xxx_part_<n>_encode/decode
    # TODO: perhaps use fixed_n, n = :System.unique_integer([:positive])
    # because name might be ""
    n = System.unique_integer([:positive])
    name = String.to_atom("fixed_#{n}_")
    encode = String.to_atom("#{name}encode")
    decode = String.to_atom("#{name}decode")

    # take fields and defaults for these primitives from the given `opts`
    fields = Keyword.take(opts[:fields], klist)
    values = Keyword.take(opts[:defaults], klist)

    # assemble args for fixed call later on
    bbdef = {name, fields: fields, defaults: values, docstr: false, private: true}

    # can't use quote do &(encode/2) end, for some reason..
    call_enc =
      {:&, [],
       [
         {:/, [context: Elixir, imports: [{2, Kernel}]], [{encode, [], nil}, 1]}
       ]}

    call_dec =
      {:&, [],
       [
         {:/, [context: Elixir, imports: [{2, Kernel}]], [{decode, [], nil}, 2]}
       ]}

    # add consolidated primitives as single fixed expr to list of funcs
    # add fixed args to the list of definitions for later ast generation
    funcs = funcs ++ [{:fixed, {call_enc, call_dec}}]
    defs = defs ++ [bbdef]
    consolidate(tail, opts, funcs, defs)
  end

  defp do_fluid(name, opts) do
    # return the ast for fluid macro to use
    # opts[:name] is for possible later use when consolidating primitives
    opts = Keyword.put_new(opts, :name, name)
    encode = String.to_atom("#{opts[:name]}encode")
    decode = String.to_atom("#{opts[:name]}decode")
    fields = opts[:fields]
    values = opts[:defaults] || []
    # join = opts[:join]

    {fields, defs} = consolidate(fields, opts)

    fixed_funcs =
      for {name, args} <- defs,
          do: do_fixed(name, args)

    q =
      quote do
        unquote_splicing(fixed_funcs)

        def unquote(encode)(kw \\ []) do
          kw = Keyword.merge(unquote(values), kw)

          encoded_kw =
            for {field, {encode, _}} <- unquote(fields) do
              # {field, encode.(kw[field] || [])}
              if field == :fixed,
                do: {field, encode.(kw)},
                else: {field, encode.(field, kw[field])}
            end

          if unquote(opts[:join]),
            do: Keyword.values(encoded_kw) |> Enum.join(),
            else: encoded_kw
        end

        def unquote(decode)(offset, bin) do
          state = %{offset: offset, bin: bin, kw: []}

          map =
            Enum.reduce(unquote(fields), state, fn fdef, acc ->
              {field, {_, decode}} = fdef

              {offset, kw, bin} =
                if field == :fixed,
                  do: decode.(acc.offset, acc.bin),
                  else: decode.(field, acc.offset, acc.bin)

              acc
              |> Map.put(:offset, offset)
              |> Map.put(:bin, bin)
              |> Map.put(:kw, acc.kw ++ kw)
            end)

          {map.offset, map.kw, map.bin}
        end
      end

    # IO.puts(Macro.to_string(q))
    q
  end

  @doc """
  Defines encode/decode functions for given `name` and `opts`, which must include
  a list of field definitions.


  """
  defmacro fluid(name, opts) do
    qq = do_fluid(name, opts)
    qq = if opts[:private], do: do_defp(qq), else: qq
    IO.puts(Macro.to_string(qq))
    qq
  end
end
