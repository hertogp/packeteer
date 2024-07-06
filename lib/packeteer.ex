defmodule Packeteer do
  @moduledoc """
  A helper library to make encoding and decoding bitstrings easier.

  """

  @typedoc "A key-value store, either a map or a keyword list"
  @type kv :: Keyword.t() | map

  @typedoc "A field name as atom, unique within a `pack/1` specification."
  @type key :: atom

  @typedoc "Any value for a given field in a `pack/1` specification."
  @type value :: any

  @typedoc "A map containing encoder or decoder state information."
  @type state :: map

  @typedoc "An non negative integer."
  @type offset :: non_neg_integer

  @typedoc "A bitstring, possibly a binary"
  @type bin :: bitstring

  @primitives [:uint, :sint, :float, :bytes, :binary, :bits, :bitstring, :utf8, :utf16, :utf32]
  @endianness [:big, :little, :native]
  # :pattern could be nil by choice, so no default and check absence/presence
  # rather than claiming a value as default
  @defaults [
    name: "",
    fields: [],
    defaults: [],
    before_encode: nil,
    after_decode: nil,
    docstr: true,
    private: false,
    silent: true
  ]

  # [[ PRIMITIVES ]]
  # - https://www.erlang.org/doc/system/expressions#bit-syntax-expressions

  @doc """
  Returns the quoted rhs for an unsigned integer segment for given `size` in _bits_.

  The `size` argument can also be a simple expression like `:a * 8` (yielding an integer) when `:a` is an
  earlier field that decodes an integer.

  Pass in an optional second argument (:little or :native) if the
  default doesn't suit your needs.

  Used as the right hand side of an unsigned integer segment
  in a bitstring match expression.

  """
  @doc section: :fragment
  def uint(size, endian \\ :big) when endian in @endianness do
    endian = var(endian)
    size = var(size)
    quote do: integer - size(unquote(size)) - unquote(endian)
  end

  @doc """
  Returns the quoted rhs for an signed integer segment for given `size` in _bits_.

  The `size` argument can also be a simple expression like `:a * 8` when `:a` is an
  earlier field that decodes an integer.

  Pass in an optional second argument (`:little` or `:native`) if the
  default doesn't suit your needs.

  Used as the right hand side of a signed integer segment
  in a bitstring match expression.

  """
  @doc section: :fragment
  def sint(size, endian \\ :big) when endian in @endianness do
    endian = var(endian)
    size = var(size)
    quote do: integer - size(unquote(size)) - unquote(endian) - signed
  end

  @doc """
  Returns the quoted rhs for a float segment for given `size` in _bits_.

  The `size` argument must be one of `[16, 32, 64]`.

  Pass in an optional second argument (`:little` or `:native`) if the
  default doesn't suit your needs.

  Used as the right hand side of a float segment
  in a bitstring match expression.

  """
  @doc section: :fragment
  def float(size, endian \\ :big) when endian in @endianness do
    endian = var(endian)
    size = var(size)
    quote do: float - size(unquote(size)) - unquote(endian)
  end

  @doc """
  Shorthand for `binary/1`.

  """
  @doc section: :fragment
  def bytes(size \\ nil) do
    if size do
      quote do: bytes - size(unquote(var(size)))
    else
      quote do: bytes
    end
  end

  @doc """
  Returns the quoted rhs for a binary segment of given `size` in _bytes_.

  The `size` argument can also be a simple expression like `:a * 8` when `:a` is an
  earlier field that decodes an integer. When omitted, matches the remaining
  bytes.

  Used as the right hand side of a binary segment
  in a bitstring match expression.

  """
  @doc section: :fragment
  def binary(size \\ nil) do
    bytes(size)
  end

  @doc """
  Shorthand for `bitstring/1`.

  """
  @doc section: :fragment
  def bits(size \\ nil) do
    if size do
      quote do: bits - size(unquote(var(size)))
    else
      quote do: bits
    end
  end

  @doc """
  Returns the quoted rhs for a bitstring segment of given `size` in _bits_.

  The `size` argument can also be a simple expression like `:a * 8` when `:a` is an
  earlier field that decodes an integer. When omitted, matches the remaining
  bits.

  Used as the right hand side of a bitstring segment
  in a bitstring match expression.

  """
  @doc section: :fragment
  def bitstring(size \\ nil) do
    bits(size)
  end

  @doc """
  Returns the quoted rhs for a utf8 segment.

  Utf8 codepoints are encoded in (or decoded from) 1..4 bytes.
  This can encode or decode only 1 utf codepoint at a time.  Be sure to use
  codepoints as values for any defaults, not the utf encoded string.  So, e.g.
  use `c: 8364`, not `c: "€"`.

  """
  @doc section: :fragment
  def utf8() do
    quote do: utf8
  end

  @doc """
  Returns the quoted rhs for a utf16 segment.

  Utf16 codepoints are encoded in (or decoded from) two 16 bit units
  (i.e. 4 bytes). The bitsyntax only supports an endianness modifier, so this
  can encode or decode only 1 utf codepoint at a time.

  The default for `endian` is `:big`, pass in either `:little` or `native`
  as appropriate.

  """
  @doc section: :fragment
  def utf16(endian \\ :big) when endian in @endianness do
    quote do: utf16 - unquote(var(endian))
  end

  @doc """
  Returns the quoted rhs for a utf32 segment.

  Utf32 codepoints are encoded in (or decoded from) 32 bits (i.e. 4 bytes).
  The bitsyntax only supports an endianness modifier, so this can encode or
  decode only 1 utf codepoint at a time.  Only the first (lsb) 21 bits are
  used at the moment.

  The default for `endian` is `:big`, pass in either `:little` or `native`
  as appropriate.

  """
  @doc section: :fragment
  def utf32(endian \\ :big) when endian in @endianness do
    quote do: utf32 - unquote(var(endian))
  end

  # [[ HELPERS ]]

  defp all?(fields) do
    # returns true if expression matches all bits
    {_name, {fld, _, arg}} = List.last(fields)
    fld in [:bits, :bitstring, :bytes, :binary] and arg in [[], [nil]]
  end

  defp set_defaults(opts) do
    Keyword.merge(@defaults, opts)
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
    non_p = Enum.filter(fields, fn field -> ftype(field) != :p end)

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
    vars = Enum.filter(Macro.prewalker(fields), fn x -> is_atom(x) end) |> Enum.uniq()
    IO.inspect(vars, label: :bindings)

    # for {n, _q} <- fields do
    for v <- vars do
      {:=, [],
       [
         var(v),
         {{:., [from_brackets: true], [Access, :get]}, [from_brackets: true], [var(:kw), v]}
       ]}
    end
  end

  # [[ DOC STRINGS ]]

  defp plural(n) do
    # pesky plurals
    if n == 1,
      do: "",
      else: "s"
  end

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
      nf = length(fields)
      values = Keyword.delete(opts[:values] || [], :skip__)
      fixed? = Enum.all?(fields, fn f -> ftype(f) == :p end)

      codec =
        if fixed?,
          do: fragments(fields),
          else: nil

      """
      Encodes #{nf} named field#{plural(nf)} from given `kw` keyword list to a binary
      as per field definition#{plural(nf)} below.

      Fields are encoded in the order listed in the field definition.  All _named_ fields
      must be present in `kw`-list, unless they have a defined default value.

      Field definition#{plural(nf)}:
      ```
      #{pretty_fields(fields)}
      ```

      #{if codec do
        """
        Bitstring expression:
        ```
        #{Macro.to_string(codec)}
        ```
        """
      else
        "The encoder is produced by `pack/1`."
      end}

      #{if values == [] do
        "No default values defined."
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
      fixed? = Enum.all?(fields, fn f -> ftype(f) == :p end)
      nf = length(fields)

      codec =
        if fixed?,
          do: fragments(fields),
          else: nil

      """
      Decodes #{nf} named field#{plural(nf)} from given `bin` binary,
      starting at `offset`, returns `{new_offset, Keyword.t, binary}`.

      Field definition#{plural(nf)}:
      ```
      #{pretty_fields(fields)}
      ```

      #{if codec do
        """
        Bitstring expression:
        ```
        #{Macro.to_string(codec)}
        ```
        """
      else
        "The encoder is produced by `pack/1`."
      end}
      """
    end
  end

  # [[ CHECKS ]]

  # is field :p(rimitive) or a :f(unction)
  defp ftype({_fieldname, v}) do
    if elem(v, 0) in @primitives, do: :p, else: :f
  end

  defp arity({:&, _, [{:/, _, [_, n]}]}),
    do: n

  defp arity(_),
    do: nil

  defp check!(:defs, opts, file, line) do
    # check field definitions (as far as feasible)
    fields = opts[:fields]
    {pdefs, fdefs} = Enum.split_with(fields, fn field -> ftype(field) == :p end)
    IO.inspect(fdefs, label: :custom_codecs)

    # custom encoder/decoders
    for {k, {enc, dec}} <- fdefs do
      if arity(enc) != 3 do
        error =
          ":#{k}, expected an encoder with arity 3, got: " <>
            Macro.to_string(enc)

        raise CompileError, file: file, line: line, description: error
      end

      if arity(dec) != 5 do
        error =
          ":#{k}, expected a decoder with arity 5, got: " <>
            Macro.to_string(dec)

        raise CompileError, file: file, line: line, description: error
      end
    end

    # primitives
    pos = Keyword.keys(fields) |> Enum.with_index()

    for {field, fdef} <- pdefs do
      if fdef == nil do
        ":#{field} has no definition"
      else
        {_type, _, args} = fdef

        frefs = Enum.filter(Macro.prewalker(args), fn x -> is_atom(x) end)

        for fref <- frefs do
          {_, kw, _} = fdef

          if pos[fref] == nil do
            error = ":#{field}, refers to missing field: :#{fref}"
            raise CompileError, file: file, line: kw[:line], description: error
          end

          unless pos[fref] < pos[field] do
            error =
              ":#{field}, can only refer to previous fields, not: :#{fref}"

            raise CompileError, file: file, line: kw[:line], description: error
          end
        end
      end
    end
  end

  defp check!(:dups, opts, file, line) do
    # no duplicates allowed in fields nor defaults

    for kw <- [:fields, :defaults] do
      fields = opts[kw]

      dups = Keyword.keys(fields) |> Enum.with_index()
      dups = Enum.filter(dups, fn {key, pos} -> dups[key] != pos end)

      if dups != [] do
        error =
          Enum.map(dups, fn {k, pos} -> ":#{k} (field ##{pos + 1})" end)
          |> Enum.join(", ")

        error = ":#{kw} has duplicates: " <> error
        raise CompileError, file: file, line: line, description: error
      end
    end
  end

  defp check!(:miss, opts, file, line) do
    # check that :defaults only contains known fields
    fields = opts[:fields]
    values = opts[:defaults]

    missing =
      Enum.filter(values, fn {k, _v} -> fields[k] == nil end)
      |> Enum.map(fn {k, _v} -> k end)

    if missing != [] do
      missing = Enum.map(missing, fn k -> ":#{k}" end) |> Enum.join(", ")
      error = ":defaults has unknown fields: " <> missing
      raise CompileError, file: file, line: line, description: error
    end
  end

  defp check!(opts, file, line) do
    fields = opts[:fields]

    if fields == [] or not Keyword.keyword?(fields) do
      raise CompileError,
        file: file,
        line: line,
        description:
          ":fields must be a keyword list of field definitions, got: #{inspect(opts[:fields])}"
    end

    values = opts[:defaults]

    if not Keyword.keyword?(values) do
      raise CompileError,
        file: file,
        line: line,
        description:
          ":defaults must be a keyword list of field values, got: #{inspect(opts[:fields])}"
    end

    empty = Enum.filter(fields, fn {_k, v} -> v == nil end)

    if empty != [] do
      error =
        Enum.map(empty, fn {k, _v} -> "#{k}: (#{inspect(fields[k])})" end) |> Enum.join(", ")

      raise CompileError,
        file: file,
        line: line,
        description: ":fields has invalid definitions for: " <> error
    end

    check!(:dups, opts, file, line)
    check!(:miss, opts, file, line)
    check!(:defs, opts, file, line)

    # check!(:vals, fields, values, file, line)
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
        {offset, unquote(fun).(kw), bin}
      end
    else
      quote do
        {offset, kw, bin}
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

  defp func_args(:encode, opts) do
    arg = [{:kw, [], Packeteer}]
    pat = opts[:pattern]

    if Keyword.has_key?(opts, :pattern),
      do: [pat | arg],
      else: arg
  end

  defp func_args(:decode, opts) do
    arg = [{:offset, [], Packeteer}, {:bin, [], Packeteer}]
    pat = opts[:pattern]

    if Keyword.has_key?(opts, :pattern),
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

  defp fixed_ast(name, opts) do
    # returns the ast for a single bit-syntax expression
    # used for consecutive primitives in a field definitions
    IO.inspect(opts, label: :fixed_opts)
    # opts = generic_defaults(opts)
    fields = opts[:fields]
    values = opts[:defaults]
    all? = all?(fields)
    maybe_skip = maybe_skip(all?)
    fields = if all?, do: fields, else: fields ++ [{:skip__, {:bits, [], []}}]
    keys = Enum.map(fields, fn {k, _} -> k end)
    vars = Enum.map(keys, fn k -> var(k) end)

    # x_encode(kw) or xyz_encode(pattern, kw)
    encode_fun = String.to_atom("#{name}encode")
    encode_args = func_args(:encode, opts)
    encode_doc = docstring(:encode, opts)
    before_encode = before_encode(opts[:before_encode])

    # x_decode(offset, bin, kw) or xyz_decode(pattern, offset, bin, kw)
    # where kw is the list of fields decoded this far.
    decode_fun = String.to_atom("#{name}decode")
    decode_args = func_args(:decode, opts)
    decode_doc = docstring(:decode, opts)
    after_decode = after_decode(opts[:after_decode])

    codec = fragments(fields)
    binds = bindings(fields)

    ast =
      quote do
        @doc unquote(encode_doc)
        def unquote(encode_fun)(unquote_splicing(encode_args)) do
          try do
            kw = Keyword.merge(unquote(values), kw)
            unquote(before_encode)
            unquote(maybe_skip)
            unquote_splicing(binds)
            unquote(codec)
          rescue
            error -> {:error, Exception.message(error)}
          end
        end

        @doc unquote(decode_doc)
        def unquote(decode_fun)(unquote_splicing(decode_args), kw) when is_bitstring(bin) do
          try do
            unquote(binds)
            <<_::bits-size(offset), rest::bits>> = bin
            unquote(codec) = rest
            kw = Enum.zip(unquote(keys), unquote(vars))
            skipped = kw[:skip__] || <<>>
            offset = bit_size(bin) - bit_size(skipped)
            kw = Keyword.delete(kw, :skip__)
            unquote(after_decode)
          rescue
            error -> {:error, Exception.message(error)}
          end
        end
      end

    ast = if opts[:private], do: do_defp(ast), else: ast

    unless opts[:silent],
      do: IO.puts(Macro.to_string(ast))

    ast
  end

  # [[ MIXED GENERATOR ]]

  # group fields by type in order to turn consecutive primitives into a single
  # private expression using bit syntax (i.e. a single fixed encoder/decoder)
  # returns {funcs, defs}
  # - funcs, list of function calls, including new, private fixed en/decoder
  # - defs, list of ast's that will define the private fixed en/decoders
  defp consolidate(fields, opts) do
    # this is where consolidation starts, only used by `mixed_ast`
    annotated_fields =
      for field <- fields do
        {ftype(field), field}
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
    # turn list of consecutive primitives into single fixed encoder/decoder pair
    plist = for {:p, p} <- h, do: p
    klist = for {k, _} <- plist, do: k

    # private func name fixed_<n>_encode/decode
    n = System.unique_integer([:positive])
    name = String.to_atom("fixed_#{n}_")
    encode = String.to_atom("#{name}encode")
    decode = String.to_atom("#{name}decode")

    # take fields and defaults for these primitives from the given `opts`
    fields = Keyword.take(opts[:fields], klist)
    values = Keyword.take(opts[:defaults], klist)

    # assemble args for call to fixed_ast/2 later on
    bbdef = {name, fields: fields, defaults: values, docstr: false, private: true}

    # can't use quote do &(encode/2) end, for some reason..
    # uses nil so encoder/decoder gets defined in caller's context
    call_enc = {:&, [], [{:/, [context: Elixir, imports: [{2, Kernel}]], [{encode, [], nil}, 1]}]}
    call_dec = {:&, [], [{:/, [context: Elixir, imports: [{2, Kernel}]], [{decode, [], nil}, 3]}]}

    # add consolidated primitives as single fixed expr to list of funcs
    # add fixed args to the list of definitions for later ast generation
    funcs = funcs ++ [{:fixed, {call_enc, call_dec}}]
    defs = defs ++ [bbdef]
    consolidate(tail, opts, funcs, defs)
  end

  defp mixed_ast(name, opts) do
    fields = opts[:fields]
    values = opts[:defaults]

    # xyz_encode(kw, state) or xyz_encode(pattern, kw, state)
    encode_fun = String.to_atom("#{name}encode")
    encode_args = func_args(:encode, opts)
    encode_doc = docstring(:encode, opts)
    before_encode = before_encode(opts[:before_encode])

    # xyz_decode(offset, bin, kw) or xyz_decode(pattern, offset, bin, kw)
    # where kw is the list of fields decoded this far.
    decode_fun = String.to_atom("#{name}decode")
    decode_args = func_args(:decode, opts)
    decode_doc = docstring(:decode, opts)
    after_decode = after_decode(opts[:after_decode])

    {fields, defs} = consolidate(fields, opts)

    fixed_funcs =
      for {name, args} <- defs,
          do: fixed_ast(name, args)

    ast =
      quote do
        unquote_splicing(fixed_funcs)

        @doc unquote(encode_doc)
        def unquote(encode_fun)(unquote_splicing(encode_args)) do
          kw = Keyword.merge(unquote(values), kw)
          unquote(before_encode)
          state = %{encoded: [], state: %{}}

          map =
            Enum.reduce(unquote(fields), state, fn fdef, acc ->
              {field, {encode, _}} = fdef

              if field == :fixed do
                value = encode.(kw)
                %{acc | encoded: acc.encoded ++ [{field, value}]}
              else
                {value, state} = encode.(field, kw, acc.state)
                %{acc | encoded: acc.encoded ++ [{field, value}], state: state}
              end
            end)

          map.encoded
          |> Keyword.values()
          |> Enum.reduce(<<>>, fn v, acc -> <<acc::bits, v::bits>> end)
        end

        # [ ] add kw to decode call (previous fields decoded)
        @doc unquote(decode_doc)
        def unquote(decode_fun)(unquote_splicing(decode_args)) do
          # bin is included in state in case decoders "eat" the binary
          # by returning the unprocessed part (so offset = 0)
          state = %{offset: offset, bin: bin, kw: [], state: %{}}

          map =
            Enum.reduce(unquote(fields), state, fn fdef, acc ->
              {field, {_, decode}} = fdef

              if field == :fixed do
                {offset, kw, bin} = decode.(acc.offset, acc.bin, acc.kw)
                %{acc | offset: offset, bin: bin, kw: acc.kw ++ kw}
              else
                {offset, value, bin, state} =
                  decode.(field, acc.kw, acc.offset, acc.bin, acc.state)

                %{acc | offset: offset, bin: bin, kw: acc.kw ++ [{field, value}], state: state}
              end
            end)

          {offset, kw, bin} = {map.offset, map.kw, map.bin}
          unquote(after_decode)
        end
      end

    ast = if opts[:private], do: do_defp(ast), else: ast

    unless opts[:silent],
      do: IO.puts(Macro.to_string(ast))

    ast
  end

  @doc """
  Defines an encode and decode function for given `specification` in caller's
  context.

  The specification is a keyword list with at least a `:fields` entry
  which contains one or more definitions for fields to be encoded, resp.
  decoded.

  The `:fields` list contains `{:name, definition}`-pairs where definition
  is either:
  - one of the [primitives](#primitives) or
  - a pair of captured custom {`&my_encode/3`, `&my_decode/5`} functions.

  Optional parts of the `specification` include:
  - `:name`, a binary (default `""`), used as prefix for the function name of
  the encode/decode functions.

  - `:defaults`, a keyword list with `{:key,value}`-pairs (default `[]`).
  Used by the encode function to fill in the blanks.

  - `:access`, path to the `:fields` in the given [`kv`](`t:kv/0`) store.  If
  not used, the given `kv` should contain the `:fields` to be encoded or
  decoded.

  - `:before_encode`, if present, must be a function ([`kv`](`t:kv/0`)) ->
  [`kv`](`t:kv/0`) which returns an updated list.  The function is called by
  the encode function, after adding in any default values and before encoding
  starts.  Useful for mapping symbolic names to their numeric value before
  encoding.

  - `:after_decode`, if present, must be a function ([`kv`](`t:kv/0`)) ->
  [`kv`](`t:kv/0`) which returns a possibly modified result. Useful for
  mapping numeric values to their symbolic name after decoding.

  - `:docstr`, if true, `pack/1` will include docstrings for the generated
  encode and decode functions.

  - `:private`, if `true`, the encode/decode functions are defined as private
  functions in the caller's module without docstrings (i.e. this overrides
  the `:docstr` setting). The default is `false`.

  - `:pattern`, if present, must be a literal that will be included in the
  signature of the encode/decode functions as their first argument.  Used
  when generating multiple encode/decode functions with the same name
  which use pattern matching to select the right pair.

  - `:silent`, if `false`, prints the defined functions to the console during
  compilation.  The default is `true`.

  ## The encode function

  `pack/1` defines an encode function with arity of 1 or 2 (if the `:pattern`
  option was used).  Regardless, the encode function returns either a
  {[`bin`](`t:bin/0`), [`state`](`t:state/0`)}-tuple or an error tuple with a
  reason for failure.

  ```
  Defines
  my_encode({kv, bin, state})     -> {kv, bin, state}
  my_encode(:x, {kv, bin, state}) -> {kv, bin, state}
  ```

  ### A custom encoder

  A custom encoder for a field is a function `(key, kv, state) -> {bin, state}`
  where:
  - [`key`](`t:key/0`), the name of the field to be encoded
  - [`kv`](`t:kv/0`), holds all key,value-pairs that are being encoded
  - [`state`](`t:state/0`), which may contain state information from custom encoders
  - [`bin`](`t:bin/0`), is the value encoded as a bitstring


  ## The decode function

  `pack/1` defines a decode function with arity of 1 or 2 (if the `:pattern`
  option is used).

  ```
  Defines
  my_decode({offset, bin, kv, state})     -> {offset, bin, kv, state}
  my_decode(:x, {offset, bin, kv, state}) -> {offset, bin, kv, state}
  ```

  ## Example

  A simple example would be to decode an unsigned integer whose width is
  specified by a preceding byte as a multiple of 4 bits, followed by a binary
  of 5 bytes.

      iex> defmodule M do
      ...>  import Packeteer
      ...>  pack([
      ...>      name: "",
      ...>      fields: [
      ...>        len: uint(8),
      ...>        val: uint(:len * 4),
      ...>        str: binary(5)
      ...>     ],
      ...>     defaults: [
      ...>       str: "stuff"
      ...>     ],
      ...>     docstr: false
      ...>  ])
      ...> end
      iex> bin = M.encode(len: 4, val: 65535) <> "more stuff"
      <<4, 255, 255, "stuff", "more stuff">>
      iex> M.decode(0, bin, %{})
      {64, [len: 4, val: 65535, str: "stuff"], <<4, 255, 255, "stuff", "more stuff">>}
      iex> <<_::bits-size(64), todo::binary>> = bin
      iex> todo
      "more stuff"

  ## Custom encoders/decoders

  Sometimes encoding/decoding bitstrings requires more that what can be achieved
  by straight bit-syntax expressions.  In that case a custom encoder/decoder
  pair can be specified in the fields definition as a two-tuple of:
  `{&custom_encode/3, &custom_decode/5}`.

  `pack/1` will construct private (and uniquely named) encode/decode functions
  for consecutive primitives and arrives at a list of encoders/decoders that
  are called in the order listed in the `:fields` definitions.

  The custom encoder/decoder have the following typespecs:
  ```elixir
  @spec custom_encode(atom, Keyword.t, map) :: {bitstring, map}
  # and
  @spec custom_decode(atom, Keyword.t, offset, bitstring, map) :: {offset, term, bitstring, map}
  ```

  where
  - atom is the field name to be encode/decoded
  - Keyword.t the list of {field,value} to be encoded, resp. decoded thus far
  - offset is a non_negative_integer where decoding either should start or left off
  - term is what will be entered as the value for the field being decoded
  - bitstring is the bitstring as encoded or being decoded
  - map is a state that is passed along the encoder's/decoder's

  The custom encoder takes the name (an atom) of the field to be encoded, the
  list of {field,value}-pairs and a state (map) and must return a `{bitstring, map}`
  tuple where the bitstring is the encoded result and map the (possibly updated)
  state.  Subsequent encoders (maybe itself) will be able to see any relevant
  information left by previous encoders in the list.

  The custom decoder takes the name (an atom) of the field to be decoded, a
  keyword list of {field,value}-pairs decoded thusfar, the offset where decoding
  should commence, the bitstring being decoded and a state in the form of a map.
  It must return `{offset, term, bitstring, map}`-tuple.  Where offset is where
  the next decoder should start, the term will be placed as the value for this
  field in the resulting keyword list, bitstring may be the leftover bits (in
  which case offset should be set to `0`) and map is the, possibly updated, state.
  Note that subsequent decoders will receive the offset, bitstring and map as
  was returned by the custom decoder.

  Not all encoders and/or decoders will need access to the state and/or fields
  decoded thus far, but then again, some do.

  ## Example

  Suppose domain name compression was not a thing and you're defining a module
  `RR` that decodes the rdata section of different resource records, starting
  with a SOA record. So, with one helper encoder/decoder, you could do something
  like this:

      iex> defmodule RR do
      ...>   import Packeteer
      ...>
      ...>   # custom encoder/decoder
      ...>   def name_enc(name, kw, state) do
      ...>   dname =
      ...>     kw[name]
      ...>     |> String.replace("@", ".", global: false)
      ...>     |> String.split(".")
      ...>     |> Enum.map(fn label -> <<byte_size(label)::8, label::binary>> end)
      ...>     |> Enum.join()
      ...>     |> Kernel.<>(<<0>>)
      ...>   {dname, state}
      ...>   end
      ...>
      ...>   def name_dec(name, _kw, offset, bin, state) do
      ...>     # we don't need any previously decoded fields
      ...>     {offset, dname} = do_labels(offset, bin, [])
      ...>     dname =
      ...>       if name == :rname,
      ...>         do: String.replace(dname, ".", "@", global: false),
      ...>         else: dname
      ...>
      ...>     {offset, dname, bin, state}
      ...>   end
      ...>
      ...>   defp do_labels(offset, bin, acc) do
      ...>     <<_::bits-size(offset), len::8, label::binary-size(len), _::bits>> = bin
      ...>     offset = offset + 8 * (1 + len)
      ...>
      ...>     case len do
      ...>       0 -> {offset, Enum.reverse(acc) |> Enum.join(".")}
      ...>       _ -> do_labels(offset, bin, [label | acc])
      ...>     end
      ...>   end
      ...>
      ...>   pack([
      ...>     name: "rdata_",
      ...>     pattern: :soa,
      ...>     fields: [
      ...>       mname: {&name_enc/3, &name_dec/5},
      ...>       rname: {&name_enc/3, &name_dec/5},
      ...>       serial: uint(32),
      ...>       refresh: uint(32),
      ...>       retry: uint(32),
      ...>       expire: uint(32),
      ...>       minimum: uint(32)
      ...>     ],
      ...>     defaults: [
      ...>       mname: "ns.icann.org",
      ...>       rname: "noc@dns.icann.org",
      ...>       serial: 2_024_041_834,
      ...>       refresh: 7200,
      ...>       retry: 3600,
      ...>       expire: 1_209_600,
      ...>       minimum: 3600
      ...>     ]
      ...>   ])
      ...> end
      iex> bin = RR.rdata_encode(:soa, [])  #=> <<2, 110, 115, 5, 105, 99, 97, ...>>
      iex> {offset, kw, _bin} = RR.rdata_decode(:soa, 0, bin)
      iex> offset
      424
      iex> kw
      [
        mname: "ns.icann.org",
        rname: "noc@dns.icann.org",
        serial: 2024041834,
        refresh: 7200,
        retry: 3600,
        expire: 1209600,
        minimum: 3600
      ]
  """
  defmacro pack(specification) do
    opts = set_defaults(specification)
    check!(opts, __CALLER__.file, __CALLER__.line)

    case Enum.all?(opts[:fields], fn f -> ftype(f) == :p end) do
      true -> fixed_ast(opts[:name], opts)
      _ -> mixed_ast(opts[:name], opts)
    end
  end
end
