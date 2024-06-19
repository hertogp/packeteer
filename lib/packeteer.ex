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
      # don't want the hidden field to show up
      fields = Keyword.delete(opts[:fields], :skip__)
      values = Keyword.delete(opts[:values], :skip__)
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
      fields = Keyword.delete(opts[:fields], :skip__)
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

  # [[ SIMPLEX GENERATOR ]]

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

  defp do_simplex(name, opts) do
    # build the entire ast for the simplex macro to use
    # [ ] use private to decide whether to generate docstrings or not instead of docstr
    opts = Keyword.put_new(opts, :docstr, true)
    fields = opts[:fields]
    values = opts[:defaults] || []
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
            offset = offset + bit_size(bin) - bit_size(skipped)
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
  Creates an `encode` and `decode` function for given `name`, _primitive_
  `fields`-definition and some extra options.

  The simplex function takes a `name` and `opts` (a list of options that must
  include a `fields` entry). Using that list of [primitive](#primitve) field
  definitions, it defines the following encode/decode functions in the calling
  module:
  - `\#{name}encode/1` and `\#{name}decode/2`, or
  - `\#{name}encode/2` and `\#{name}decode/3`

  whose signatures (assuming name is "name_") are:
  ```
  name_encode(Keyword.t) :: binary | {:error, binary}
  name_decode(non_neg_integer, binary) :: {non_neg_integer, Keyword.t, binary} | {:error, binary}
  # or
  name_encode(atom, Keyword.t) :: binary | {:error, binary}
  name_decode(atom, non_neg_integer, binary) :: {non_neg_integer, Keyword.t, binary} | {:error, binary}
  ```

  If some module M has only a single call to `simplex/2`, using `name = ""` will define:
  - `M.encode(kw)`
  - `M.decode(offset, bin)`

  The `fields` list of field definitions is used to construct the bitstring
  match expression for both the encode and decode function. Hence,
  the order of field definitons in this list is significant.

  Possible extra options include:

  - `:defaults`, a keyword list, specifying the default values for one or more
  fields used by the encode function when called with an incomplete list of fields
  and values.  Note that fields with the same name will encode to the first default
  value given the nature of keyword lists.

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

  - `:pattern`, a literal that becomes the, additional, first argument of the
  encode and decode functions.  Useful when creating multiple encode/decode
  functions with the same name. Note that unless manually adding a catch all
  version of the encode and decode function, `FunctionClauseError` might occur.


  """
  # See: https://elixirforum.com/t/how-do-i-write-a-macro-that-dynamically-defines-a-public-or-private-function/14351
  # - howto dynamically generate def or defp functions
  defmacro simplex(name, opts) do
    IO.inspect({name, opts[:private]}, label: :simplex_called)
    qq = do_simplex(name, opts)
    IO.puts(Macro.to_string(qq))
    qq
  end

  # [[ COMPLEX GENERATOR ]]

  defp f_type({_fieldname, v}) do
    # check type of value for a given field
    # - :p means it is a primitive
    # - :f means a user supplied function
    if elem(v, 0) in @primitives, do: :p, else: :f
    # case elem(v, 0) do
    #   f when f in @primitives -> :p
    #   _ -> :f
    # end
  end

  # group fields by type in order to turn consecutive primitives into a single
  # private expression using bit syntax (i.e. a single simplex encoder/decoder)
  # returns {funcs, defs}
  # - funcs, list of function calls, including new, private simplex en/decoder
  # - defs, list of ast's that will define the private simplex en/decoders
  defp consolidate([], _opts, funcs, defs, _n),
    do: {funcs, defs}

  defp consolidate(fields, opts, [], [], 0) do
    # this is where consolidation starts, only used by `do_complex`
    annotated_fields =
      for field <- fields do
        {f_type(field), field}
      end
      |> Enum.chunk_by(fn t -> elem(t, 0) end)

    consolidate(annotated_fields, opts, [], [], 1)
  end

  defp consolidate([[{:f, _} | _] = h | tail], opts, funcs, defs, n) do
    # plain func calls, so move on
    flist = for {:f, f} <- h, do: f
    consolidate(tail, opts, funcs ++ flist, defs, n + 1)
  end

  defp consolidate([[{:p, _} | _] = h | tail], opts, funcs, defs, n) do
    # turn list of primitives into single simplex encoder/decoder pair
    plist = for {:p, p} <- h, do: p
    klist = for {k, _} <- plist, do: k

    # private func name xxx_part_<n>_encode/decode
    # TODO: perhaps use simplex_n, n = :System.unique_integer([:positive])
    # because name might be ""
    name = String.to_atom("#{opts[:name]}_simplex_#{n}_")
    encode = String.to_atom("#{name}encode")
    decode = String.to_atom("#{name}decode")

    # take fields and defaults for these primitives from the given `opts`
    fields = Keyword.take(opts[:fields], klist)
    values = Keyword.take(opts[:defaults], klist)

    # assemble args for simplex call later on
    bbdef = {name, fields: fields, defaults: values, docstr: false, private: true}
    # IO.inspect({n, bbdef}, label: :hmm)

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

    # add consolidated primitives as single simplex expr to list of funcs
    # add simplex args to the list of definitions for later ast generation
    funcs = funcs ++ [{:simplex, {call_enc, call_dec}}]
    defs = defs ++ [bbdef]
    consolidate(tail, opts, funcs, defs, n + 1)
  end

  defp do_complex(name, opts) do
    # return the ast for complex macro to use
    # opts[:name] is for possible later use when consolidating primitives
    opts = Keyword.put_new(opts, :name, name)
    encode = String.to_atom("#{opts[:name]}encode")
    decode = String.to_atom("#{opts[:name]}decode")
    fields = opts[:fields]
    values = opts[:defaults] || []
    join = opts[:join]

    {fields, defs} =
      consolidate(fields, opts, [], [], 0)

    IO.inspect(defs, label: :defs)

    simplex_funcs =
      for {name, args} <- defs,
          do: do_simplex(name, args)

    q =
      quote do
        unquote_splicing(simplex_funcs)

        def unquote(encode)(kw \\ []) do
          kw = Keyword.merge(unquote(values), kw)

          encoded_kw =
            for {field, {encode, _}} <- unquote(fields) do
              IO.inspect({field, kw}, label: :kw)
              {field, encode.(kw[field] || [])}
            end

          if unquote(join),
            do: Keyword.values(encoded_kw) |> Enum.join(),
            else: encoded_kw
        end

        def unquote(decode)(offset, bin) do
          map =
            Enum.reduce(unquote(fields), %{offset: offset, bin: bin, kw: []}, fn e, acc ->
              {field, {_, decode}} = e
              {offset, value, bin} = decode.(acc.offset, acc.bin)

              kw =
                if field == :simplex,
                  do: acc.kw ++ value,
                  else: acc.kw ++ [{field, value}]

              acc
              |> Map.put(:offset, offset)
              |> Map.put(:bin, bin)
              |> Map.put(:kw, kw)
            end)

          {map.offset, map.kw, map.bin}
        end
      end

    # IO.puts(Macro.to_string(q))
    q
  end

  @doc """
  A macro that creates \#{name}encode and \#{name}decode functions for given
  name, _complex_ `fields`-definition and some extra options.


  """
  defmacro complex(name, opts) do
    qq = do_complex(name, opts)
    if opts[:private], do: do_defp(qq), else: qq
  end
end
