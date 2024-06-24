defmodule Packeteer do
  @moduledoc """
  Declaratively define encode/decode functions for binary data.

  """

  @primitives [:uint, :sint, :float, :bytes, :binary, :bits, :bitstring, :utf8, :utf16, :utf32]
  @endianness [:big, :little, :native]

  # [[ PRIMITIVES ]]

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

  defp generic_defaults(opts) do
    # join only used by mixed/2 encoder
    # [ ] use @defaults and simply do kw = Keyword.merge(@default, kw)
    opts
    |> Keyword.put_new(:docstr, true)
    |> Keyword.put_new(:silent, true)
    |> Keyword.put_new(:join, true)
    |> Keyword.put_new(:private, false)
    |> Keyword.put_new(:defaults, [])
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
    for {n, _q} <- fields do
      {:=, [],
       [
         var(n),
         {{:., [from_brackets: true], [Access, :get]}, [from_brackets: true], [var(:kw), n]}
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
        "The encoder is produced by `mixed2`."
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
        "The encoder is produced by `mixed2`."
      end}
      """
    end
  end

  # [[ CHECKS ]]

  # is field :p(rimitive) or a :f(unction)
  defp ftype({_fieldname, v}) do
    if elem(v, 0) in @primitives, do: :p, else: :f
  end

  defp get_size([size | _]) when is_integer(size) do
    size
  end

  # size is an expression, get the field names used (atoms)
  defp get_size(ast) do
    Enum.filter(Macro.prewalker(ast), fn x ->
      IO.inspect(x, label: :prewalker)
      is_atom(x)
    end)
  end

  # check primitive definitions -> nil | error string
  defp check_pdef(pos, max, type, [])
       when pos == max and type in [:bits, :bitstring, :bytes, :binary] do
    # last field of this type may omit its size
    nil
  end

  defp check_pdef(pos, _max, type, size)
       when not is_integer(size) and type in [:uint, :sint, :bits, :bitstring, :bytes, :binary] do
    # [ ] check if its an expression referencing earlier fields
    "field #{pos}, #{type} requires a pos_integer size, got: #{inspect(size)}"
  end

  defp check_pdef(pos, _max, :float, size) when size not in [16, 32, 64] do
    "field #{pos}, float valid sizes are: [16, 32, 64], got: #{inspect(size)}"
  end

  defp check_pdef(_pos, _max, type, _size) when type in [:utf8, :utf16, :utf32] do
    nil
  end

  # all good
  defp check_pdef(_pos, _max, _type, _size) do
    nil
  end

  defp check!(:fdef, fields, file, line) do
    # check field definitions (as far as feasible)
    {pdefs, fdefs} = Enum.split_with(fields, fn fld -> ftype(fld) == :p end)
    IO.inspect(fdefs, label: :custom_codecs)

    pdefs = Enum.with_index(pdefs)
    max = length(pdefs) - 1

    error =
      for {{fname, fdef}, pos} <- pdefs do
        if fdef == nil do
          "#{fname} has no definition"
        else
          {type, _, args} = fdef

          IO.inspect(fdef, label: :fdef)

          case get_size(args) do
            size when is_integer(size) ->
              check_pdef(pos, max, type, size)

            fnames when is_list(fnames) ->
              non_uint =
                Enum.map(fnames, fn fld -> {fld, elem(fields[fld], 0)} end)
                |> Enum.filter(fn {_, t} -> t != :uint end)
                |> Enum.map(fn {fld, type} -> "#{fld}: is a #{type}" end)
                |> Enum.join(", ")

              if non_uint != "" do
                fdef = Macro.to_string(fdef)
                "field #{fname}: #{inspect(fdef)}, #{non_uint}"
              else
                nil
              end
          end
        end
      end
      |> Enum.filter(fn k -> k end)
      |> Enum.join(", ")

    if error != "" do
      error = "definition error: " <> error
      raise CompileError, file: file, line: line, description: error
    end
  end

  defp check!(:dups, kv, file, line) do
    # no duplicates allowed in kv
    error =
      kv
      |> Enum.frequencies_by(fn {k, _} -> k end)
      |> Enum.filter(fn {_, v} -> v > 1 end)
      |> Enum.map(fn {k, _} -> "#{k}" end)
      |> Enum.join(", ")

    if error != "" do
      error = "duplicate field names: "
      raise CompileError, file: file, line: line, description: error
    end
  end

  defp check!(:miss, fields, values, file, line) do
    # check for missing field definitions mentioned in defaults
    error =
      for {k, _} <- values do
        if fields[k], do: nil, else: "#{inspect(k)}"
      end
      |> Enum.filter(fn v -> v end)
      |> Enum.uniq()
      |> Enum.join(", ")

    if error != "" do
      error = "defaults without field definition: " <> error
      raise CompileError, file: file, line: line, description: error
    end
  end

  defp check!(opts, file, line) do
    # check for:
    # - duplicate field names in fields & defaults
    # - names in default that are not in fields
    # - default values for primitives that don't match their spec
    fields = opts[:fields]
    values = opts[:defaults] || []

    # fields: [:x, a: 1, b: 2] is not caught by dialixer/compiler
    # [ ] check for non-empty keyword list
    # check!(:iskw, fields, file, line)
    # check!(:iskw, values, file, line)
    check!(:dups, fields, file, line)
    check!(:dups, values, file, line)
    check!(:miss, fields, values, file, line)
    check!(:fdef, fields, file, line)
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
        unquote(fun).(offset, kw, bin)
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

  defp fixed_ast(name, opts) do
    # returns the ast for the fixed macro to use
    opts = generic_defaults(opts)
    fields = opts[:fields]
    values = opts[:defaults]
    all? = all?(fields)
    maybe_skip = maybe_skip(all?)
    fields = if all?, do: fields, else: fields ++ [{:skip__, {:bits, [], []}}]
    keys = Enum.map(fields, fn {k, _} -> k end)
    vars = Enum.map(keys, fn k -> var(k) end)

    encode_fun = String.to_atom("#{name}encode")
    encode_args = maybe_pattern(:encode, opts)
    encode_doc = docstring(:encode, opts)
    before_encode = before_encode(opts[:before_encode])

    decode_fun = String.to_atom("#{name}decode")
    decode_args = maybe_pattern(:decode, opts)
    decode_doc = docstring(:decode, opts)
    after_decode = after_decode(opts[:after_decode])

    codec = fragments(fields)
    binds = bindings(fields)

    ast =
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

  @doc """
  Defines encode/decode functions for given `name` and `opts`, which must
  include a list of [_primitive_](#primitives) fields.

  The following encode/decode functions will be defined in the calling module:
  - `\#{name}encode/1` and `\#{name}decode/2`, or
  - `\#{name}encode/2` and `\#{name}decode/3`

  whose signatures (assuming name is "name_") are:
  ```
  name_encode(Keyword.t) :: binary | {:error, reason}
  name_decode(offset, binary) :: {offset, Keyword.t, binary} | {:error, reason}

  # or when using the option `pattern: literal`:
  name_encode(literal, Keyword.t) :: binary | {:error, binary}
  name_decode(literal, offset, binary) :: {offset, Keyword.t, binary} | {:error, reason}

  # where:
  # - offset is a non_negative_integer
  # - reason a, hopefully, informative  binary
  # - literal is an expression that is its own ast
  ```

  See [literal](https://hexdocs.pm/elixir/typespecs.html#literals) for more
  information. If some module M has only a single call to `fixed/2`, using
  `name = ""` will define: `M.encode(kw)` and `M.decode(offset, bin)`.

  > #### Info {: .info}
  > If the last field in the list of definitions does not match the remaining
  > bits of any given binary, a hidden field `:skip__` is appended to the
  > expression to ensure matching won't fail.  It is removed from the resulting
  > keyword list prior to being handed to the `:after_decoding` function (if any).
  > Upon encoding it encodes an empty string so it won't add any bits to
  > the encoded binary.  Hidden also means it won't show up in the docstrings.


  - `:fields` a mandatory list of [primitive](#primitives) field definitions
  used to construct the bitstring match expression for both the encode and
  decode functions. Hence, the order of field definitions in this list is
  significant.

  Other options include:

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
  fun(offset, Keyword.t, binary) :: {offset, Keyword.t, binary}
  ```
  If specified, this function will be called with the results of decoding and its
  return value(s) are returned as-is. Although the assumption is it will adhere
  to `{offset, Keyword.t, binary}` that need not be the case.

  - `:docstr`, either `true` (the default) or `false`, specifies whether
  or not docstrings are to be generated for the encode/decode functions. By
  default the encode/decode functions are generated as public functions,
  setting `docstr` to `false` allows for eliminating them from documentation
  while still be available as public functions in your en/decoder module.

  - `:private`, either `true` or `false` (default), specifies whether the
  encode/decode functions are defined as private or public functions. If
  `true`, this overrides the `:docstr` option without warning.

  - `:pattern`, a literal expression that becomes the, additional, first argument of the
  encode and decode functions.  Allows for creating multiple encode/decode
  functions with the same name.  Additional, manual catch-all encode/decode
  functions might be required to handle any `FunctionClauseError`.  Note that if
  the generated encode/decode functions are private, the manual encode/decode
  function names will need to be different.

  - `:silent`, either `true` (default) or `false`, specifies whether the generated ast's
  for the encode/decode functions are printed to stdout during compilation.

  ## Example

  A simple example would be to decode an unsigned integer whose width is
  specified by a preceding byte as a multiple of 4 bits, followed by a binary
  of 5 bytes.

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
      iex> <<_::bits-size(64), todo::binary>> = bin
      iex> todo
      "more stuff"

  """
  defmacro fixed(name, opts) do
    check!(opts, __CALLER__.file, __CALLER__.line)
    fixed_ast(name, opts)
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
    # turn list of primitives into single fixed encoder/decoder pair
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

  defp mixed_ast(name, opts) do
    # returns the ast for the mixed macro to use
    opts = generic_defaults(opts)
    fields = opts[:fields]
    values = opts[:defaults]
    IO.inspect(fields, label: :fields)
    IO.inspect(values, label: :values)

    encode_fun = String.to_atom("#{name}encode")
    encode_args = maybe_pattern(:encode, opts)
    encode_doc = docstring(:encode, opts)
    before_encode = before_encode(opts[:before_encode])

    decode_fun = String.to_atom("#{name}decode")
    decode_args = maybe_pattern(:decode, opts)
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
          unquote(before_encode)
          kw = Keyword.merge(unquote(values), kw)

          encoded_kw =
            for {field, {encode, _}} <- unquote(fields) do
              if field == :fixed,
                do: {field, encode.(kw)},
                else: {field, encode.(field, kw[field])}
            end

          if unquote(opts[:join]),
            do: Keyword.values(encoded_kw) |> Enum.join(),
            else: encoded_kw
        end

        @doc unquote(decode_doc)
        def unquote(decode_fun)(unquote_splicing(decode_args)) do
          # bin is included in state in case decoders "eat" the binary
          # by returning the unprocessed part (so offset = 0)
          state = %{offset: offset, bin: bin, kw: []}

          map =
            Enum.reduce(unquote(fields), state, fn fdef, acc ->
              {field, {_, decode}} = fdef

              case field do
                :fixed ->
                  {offset, kw, bin} = decode.(acc.offset, acc.bin)
                  %{acc | offset: offset, bin: bin, kw: acc.kw ++ kw}

                _ ->
                  {offset, kw, bin} = decode.(field, acc.kw, acc.offset, acc.bin)
                  %{acc | offset: offset, bin: bin, kw: kw}
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
  Defines encode/decode functions for given `name` and `opts`, which must include
  a list of field definitions.

  Sometimes binary protocols require more logic than what can be achieved
  through bitstring match expressions alone.  `mixed/2` allows for
  non-primitive field definitions consisting of captured encoder/decoder
  function pairs.

  The signatures of the `mixed/2` generated encode/decode functions are:
  ```
  encode(Keyword.t) :: binary | Keyword.t | {:error, reason}
  decode(offset, Keyword.t, binary) :: Keyword.t | {:error, reason}

  # or, when option `:pattern` is set to a literal
  encode(literal, Keyword.t) :: binary | Keyword.t | {:error, reason}
  decode(literal, offset, Keyword.t, binary) :: Keyword.t | {:error, reason}

  # where:
  # - offset is a non_neg_integer
  # - reason is a binary
  # - literal is an expression that is its own ast (e.g. :id, or %{id: 42})
  ```

  The `name` and `opts` work the same as in `fixed/2`: the `name` is used as
  prefix in the mixed generated encode/decode function names and `opts` must
  have the mandatory `fields` entry, listing the field definitions.  All the
  other options of `fixed/2` are also supported by `mixed/2`.

  One additional option is supported by `mixed/2`:

  - `:join`, either `true` (default) or `false`, specifies whether the binary
  parts are joined together by the mixed encoder or not.  If:
    - `true`, the binary parts are joined and returned by the mixed encoder
    - `false`, the list of fieldnames and their binary representation is returned instead.

  Finally, `mixed/2` allows for a mixture of primitive and non-primitive field
  definitions.  Consecutive primitives are collected and turned into a private `fixed/2`
  generated encoder/decoder pair.  If the last field is a primitive, the fixed
  encode/decode function will have a hidden `:skip__` field so the binary
  matching won't fail and remaining bits are always matched out.

  ## encoder/decoder functions

  The encoder/decoder functions are expected to have signatures:

  ```
  encoder(name, any) :: binary
  decoder(name, kw, offset, binary) :: {offset, kw, binary}

  # where:
  # - kw is [{field, value}] built thusfar resp. updated with a new field upon return
  # - name is an atom denoting the field being encoded/decoded
  # - offset is a non_negative_integer
  ```

  In addition to the `offset` and `bin` being decoded, the decoder also receives
  the `name` of the field to be decoded as well as the list `[{field, value}]`
  of decoded fields thusfar.  Hence, the decoder could take different actions based on
  those.

  Note that the return value of the decoder:
  - must specify the new offset in _bits_, not bytes (!)
  - return an updated keyword list, e.g. using `kw ++ [{name, value}]`
  - return the binary (or remainder, with offset=0) for subsequent decoding

  Appending the new field maintains the order of the decoded fields in the
  resulting keyword list.  `Keyword.put/3` would put the new field in front of
  what was decoded earlier and remove duplicates (if any). Returning only the
  unprocessed part of the `binary` is certainly possible, but requires the
  returned `offset` to be set to `0` bits.

  > ### Warning {: .warning}
  >
  > non-primitive decoders should always take care of matching out the remainder of
  > given binary, if using bitstring expression, via `<<..., _::bits>>`,
  > otherwise their decoding will fail.  If the last field definition is a
  > primitive, then `fixed/2` will add the hidden `:skip__` field to ensure a
  > match will match the remainder.

  ## Example

  Suppose domain name compression was not a thing and you're defining a module
  `RR` that decodes the rdata section of different resource records, starting
  with a SOA record. So, with one helper encoder/decoder, you could do something
  like this:

      iex> defmodule RR do
      ...>   import Packeteer
      ...>
      ...>   mixed("rdata_",
      ...>     pattern: :soa,
      ...>     fields: [
      ...>       mname: {&name_enc/2, &name_dec/4},
      ...>       rname: {&name_enc/2, &name_dec/4},
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
      ...>   )
      ...>
      ...>   # custom encoder/decoder
      ...>   def name_enc(_name, str) do
      ...>     str
      ...>     |> String.replace("@", ".", global: false)
      ...>     |> String.split(".")
      ...>     |> Enum.map(fn label -> <<byte_size(label)::8, label::binary>> end)
      ...>     |> Enum.join()
      ...>     |> Kernel.<>(<<0>>)
      ...>   end
      ...>
      ...>   def name_dec(name, kw, offset, bin) do
      ...>     {offset, dname} = do_name_dec(offset, bin, [])
      ...>     dname =
      ...>       if name == :rname,
      ...>         do: String.replace(dname, ".", "@", global: false),
      ...>         else: dname
      ...>
      ...>     # maintain field order
      ...>     {offset, kw ++ [{name, dname}], bin}
      ...>   end
      ...>
      ...>   defp do_name_dec(offset, bin, acc) do
      ...>     <<_::bits-size(offset), len::8, label::binary-size(len), _::bits>> = bin
      ...>     offset = offset + 8 * (1 + len)
      ...>
      ...>     case len do
      ...>       0 -> {offset, Enum.reverse(acc) |> Enum.join(".")}
      ...>       _ -> do_name_dec(offset, bin, [label | acc])
      ...>     end
      ...>   end
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
  defmacro mixed(name, opts),
    do: mixed_ast(name, opts)
end
