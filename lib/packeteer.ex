defmodule PacketeerError do
  defexception message: "packeteer error"
end

defmodule Packeteer do
  @moduledoc """
  Declaratively create encode/decode functions for binary data.

  """

  @primitives [:uint, :sint, :float, :bytes, :binary, :bits, :bitstring, :utf8, :utf16, :utf32]

  # [[ HELPERS ]]

  defp all?(fields) do
    # returns true if expression matches all bits
    {_name, {fld, _, arg}} = List.last(fields)
    fld in [:bits, :bitstring, :bytes, :binary] and arg in [[], [nil]]
  end

  # [[ QUOTED FRAGMENTS ]]

  @doc """
  Returns quoted fragment for an unsigned integer for given `size`.

  The `size` argument specifies the number of bits that will be matched.

  Pass in an optional second argument (:little or :native) if the
  default doesn't suit your needs.

  """
  def uint(size, endian \\ :big) do
    endian = var(endian)
    size = var(size)
    quote(do: integer - size(unquote(size)) - unquote(endian))
  end

  @doc """
  Returns quoted fragment for an signed integer for given `size`.

  The `size` argument specifies the number of bits that will be matched.

  Pass in an optional second argument (`:little` or `:native`) if the
  default doesn't suit your needs.

  """
  def sint(size, endian \\ :big) do
    endian = var(endian)
    size = var(size)
    quote(do: integer - size(unquote(size)) - unquote(endian) - signed)
  end

  @doc """
  Returns quoted fragment for a float for given `size`.

  The `size` argument specifies the number of bits that will be matched
  and must be in `[16, 32, 64]`.

  Pass in an optional second argument (`:little` or `:native`) if the
  default doesn't suit your needs.

  """
  def float(size, endian \\ :big) do
    endian = var(endian)
    size = var(size)
    quote(do: integer - size(unquote(size)) - unquote(endian) - signed)
  end

  @doc """
  Shorthand for `binary/1`.

  """
  def bytes(size \\ nil) do
    if size,
      do: quote(do: bytes - size(unquote(var(size)))),
      else: quote(do: bytes)
  end

  @doc """
  Returns a quoted fragment for a binary of given `size`.

  The `size` argument specifies the number of bytes to match.
  When omitted, matches the remaining _bytes_.

  """
  def binary(size \\ nil),
    do: bytes(size)

  @doc """
  Shorthand for `bitstring/1`.

  """
  def bits(size \\ nil) do
    if size,
      do: quote(do: bits - size(unquote(var(size)))),
      else: quote(do: bits)
  end

  @doc """
  Returns a quoted fragment for bitstring of given `size`.

  The `size` argument specifies the number of bits to match.
  When omitted, matches the remaining _bits_.

  """
  def bitstring(size \\ nil),
    do: bits(size)

  @doc """
  Returns a quoted fragment for a utf8 codepoint.

  utf8 codepoints are encoded in (or decoded from) 1..4 bytes.
  The bitsyntax only supports an endianness modifier, so this can encode
  or decode only 1 utf codepoint at a time.

  The default for `endian` is `:big`, pass in either `:little` or `native`
  as appropiate.

  """
  def utf8(endian \\ :big),
    do: quote(do: utf8 - unquote(endian))

  @doc """
  Returns a quoted fragment for a utf16 codepoint.

  utf16 codepoints are encoded in (or decoded from) two 16 bit units
  (i.e. 4 bytes). The bitsyntax only supports an endianness modifier, so this
  can encode or decode only 1 utf codepoint at a time.

  The default for `endian` is `:big`, pass in either `:little` or `native`
  as appropiate.

  """
  def utf16(endian \\ :big),
    do: quote(do: utf16 - unquote(endian))

  @doc """
  Returns a quoted fragment for a utf32 codepoint.

  utf16 codepoints are encoded in (or decoded from) 32 bits (i.e. 4 bytes).
  The bitsyntax only supports an endianness modifier, so this can encode or
  decode only 1 utf codepoint at a time.  Only the first (lsb) 21 bits are
  used at the moment.

  The default for `endian` is `:big`, pass in either `:little` or `native`
  as appropiate.

  """
  def utf32(endian \\ :big),
    do: quote(do: utf32 - unquote(endian))

  # [[ BITSTR EXPRESSION ]]

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

  defp pretty_fields(str) do
    str
    |> String.replace(", ", ",\n ")
    |> String.replace("[", "[\n ")
    |> String.replace("]", "\n]")
  end

  defp docstring(:encode, fields, values) do
    codec = fragments(fields)

    """
    Encodes #{length(fields)} _named_ fields from given `kw` keyword list to a binary
    as per field definition below.

    Fields are encoded in the order listed in the field definition.  All _named_ fields
    must be present in `kw`-list, unless they have a defined default value.

    Field definitions:
    ```
    #{Macro.to_string(fields) |> pretty_fields}
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

  defp docstring(:decode, fields, _values) do
    codec = fragments(fields)

    """
    Decodes #{length(fields)} _named_ fields from given `bin` binary, starting at `offset`,
    returns `{new_offset, Keyword.t}`.

    Field definitions:
    ```
    #{Macro.to_string(fields) |> pretty_fields()}
    ```

    Bitstring expression:
    ```
    #{Macro.to_string(codec)}
    ```
    """
  end

  # [[ SIMPLEX GENERATOR ]]

  defp before_encode(fun) do
    if fun do
      quote do
        kw = unquote(fun).(kw)
      end
    end
  end

  defp after_decode(fun) do
    if fun do
      quote do
        {offset, kw, bin} = unquote(fun).(offset, kw, bin)
      end
    end
  end

  defp maybe_skip(all?) do
    unless all? do
      quote do
        kw = Keyword.put(kw, :skip__, "")
      end
    end
  end

  defp do_simplex(name, opts) do
    # TODO:
    # [ ] if docstr is false, function is defined as private
    opts = Keyword.put_new(opts, :docstr, true)
    fields = opts[:fields]
    values = opts[:defaults] || []
    all? = all?(fields)
    encode = String.to_atom("#{name}encode")
    decode = String.to_atom("#{name}decode")

    # docstr needs original fields/values so do them before maybe adding :skip__
    encode_doc = if opts[:docstr], do: docstring(:encode, fields, values), else: false
    decode_doc = if opts[:docstr], do: docstring(:decode, fields, values), else: false
    before_encode = before_encode(opts[:before_encode])
    after_decode = after_decode(opts[:after_decode])
    maybe_skip = maybe_skip(all?)

    fields = if all?, do: fields, else: fields ++ [{:skip__, {:bits, [], []}}]
    keys = Enum.map(fields, fn {k, _} -> k end)
    vars = Enum.map(keys, fn k -> var(k) end)

    codec = fragments(fields)
    binds = bindings(fields)

    quote do
      @doc unquote(encode_doc)
      def unquote(encode)(kw \\ []) when is_list(kw) do
        unquote(before_encode)
        kw = Keyword.merge(unquote(values), kw)
        unquote(maybe_skip)
        unquote_splicing(binds)
        unquote(codec)
      end

      @doc unquote(decode_doc)
      def unquote(decode)(offset \\ 0, bin) do
        <<_::bits-size(offset), rest::bits>> = bin
        unquote(codec) = rest
        kw = Enum.zip(unquote(keys), unquote(vars))
        skipped = kw[:skip__] || <<>>
        offset = offset + bit_size(bin) - bit_size(skipped)
        kw = Keyword.delete(kw, :skip__)
        unquote(after_decode)
        # fun = unquote(decode_fun)
        # # # this should be a function returning either nil or ast for kw=fun.(offset, kw, bin)
        # kw = if fun, do: fun.(offset, kw), else: kw
        {offset, kw, bin}
      end
    end
  end

  defmacro simplex(name, opts) do
    qq = do_simplex(name, opts)
    IO.puts(Macro.to_string(qq))
    IO.inspect(qq)

    Macro.prewalk(qq, fn
      {:def, x, y} -> {:defp, x, y}
      other -> other
    end)
    |> IO.inspect(label: :defpd)
  end

  @doc """
  A macro that creates `\#{name}encode` and `\#{name}decode` functions for given
  `name`, `field`-definitions and some extra options.

  Arguments include
  - `fields`, a mandatory keyword list of field definitions
  - `defaults` is an optional (keyword) list defining default values for one or more fields
  - `before_encode`, a function that takes a keyword list and returns a (modified) keyword list
  - `after_decode`, an function that takes `offset`, `kw`, `bin` and returns them, possibly modified
  - `docstr`, if true docstrings will be generated

  """
  # See: https://elixirforum.com/t/how-do-i-write-a-macro-that-dynamically-defines-a-public-or-private-function/14351
  # - howto dynamically generate def or defp functions
  defmacro fixed(name, opts) do
    # TODO:
    # [ ] if docstr is false, function is defined as private
    opts = Keyword.put_new(opts, :docstr, true)
    fields = opts[:fields]
    values = opts[:defaults] || []
    all? = all?(fields)
    encode = String.to_atom("#{name}encode")
    decode = String.to_atom("#{name}decode")

    # docstr needs original fields/values so do them before maybe adding :skip__
    encode_doc = if opts[:docstr], do: docstring(:encode, fields, values), else: false
    decode_doc = if opts[:docstr], do: docstring(:decode, fields, values), else: false
    before_encode = before_encode(opts[:before_encode])
    after_decode = after_decode(opts[:after_decode])
    maybe_skip = maybe_skip(all?)

    fields = if all?, do: fields, else: fields ++ [{:skip__, {:bits, [], []}}]
    keys = Enum.map(fields, fn {k, _} -> k end)
    vars = Enum.map(keys, fn k -> var(k) end)

    codec = fragments(fields)
    binds = bindings(fields)

    qq =
      quote do
        @doc unquote(encode_doc)
        def unquote(encode)(kw \\ []) when is_list(kw) do
          unquote(before_encode)
          kw = Keyword.merge(unquote(values), kw)
          unquote(maybe_skip)
          unquote_splicing(binds)
          unquote(codec)
        end

        @doc unquote(decode_doc)
        def unquote(decode)(offset \\ 0, bin) do
          <<_::bits-size(offset), rest::bits>> = bin
          unquote(codec) = rest
          kw = Enum.zip(unquote(keys), unquote(vars))
          skipped = kw[:skip__] || <<>>
          offset = offset + bit_size(bin) - bit_size(skipped)
          kw = Keyword.delete(kw, :skip__)
          unquote(after_decode)
          # fun = unquote(decode_fun)
          # # # this should be a function returning either nil or ast for kw=fun.(offset, kw, bin)
          # kw = if fun, do: fun.(offset, kw), else: kw
          {offset, kw, bin}
        end
      end

    IO.puts(Macro.to_string(qq))
    qq
  end

  defp f_type({_field, v}) do
    case elem(v, 0) do
      f when f in @primitives ->
        :p

      v ->
        IO.inspect(v)
        :f
        # _ ->
        #   raise "not a Packeteer field spec '#{field}: #{Macro.to_string(v)}'"
    end
  end

  defp consolidate([], _opts, funcs, defs, _n),
    do: {funcs, defs}

  defp consolidate(fields, opts, [], [], 0) do
    annotated_fields =
      for field <- fields do
        {f_type(field), field}
      end
      |> Enum.chunk_by(fn t -> elem(t, 0) end)

    # IO.inspect(annotated_fields, label: :annotated_fields)
    consolidate(annotated_fields, opts, [], [], 1)
  end

  defp consolidate([[{:f, _} | _] = h | tail], opts, funcs, defs, n) do
    flist = for {:f, f} <- h, do: f
    # plain func calls, so move on
    consolidate(tail, opts, funcs ++ flist, defs, n + 1)
  end

  defp consolidate([[{:p, _} | _] = h | tail], opts, funcs, defs, n) do
    plist = for {:p, p} <- h, do: p
    klist = for {k, _} <- plist, do: k
    name = String.to_atom("#{opts[:name]}_part_#{n}_")

    values = Keyword.take(opts[:values], klist)
    fields = Keyword.take(opts[:fields], klist)

    bbdef = {name, fields: fields, values: values, docstr: false}
    # IO.inspect({n, bbdef}, label: :hmm)

    encode = String.to_atom("#{name}encode_")
    decode = String.to_atom("#{name}decode_")

    call_enc =
      quote do
        &(__MODULE__.unquote(encode) / 2)
      end

    call_dec =
      quote do
        &(__MODULE__.unquote(decode) / 2)
      end

    funcs = funcs ++ [{name, {call_enc, call_dec}}]
    defs = defs ++ [bbdef]
    consolidate(tail, opts, funcs, defs, n + 1)
  end

  # TODO
  # [ ] add before_encode option: fn kw -> kw
  # [ ] add after_decode option: fn (offset, kw, bin) -> (offset, kw, bin)
  # [ ] add join_encode true/false -> shorthand for Keyword.values |> Enum.join
  # [ ] add docstr_encode option
  # [ ] add docstr_decode option
  # [ ] add as_defp true/false, if true, no docstr will be generated
  # [ ] move ast building into functions
  # [ ] use a maybe_fun(after_decode) -> which only inserts call if defined
  defmacro fluid(name, opts) do
    # IO.inspect(name, label: "#{name}")
    # IO.inspect(opts, label: :opts)
    opts = Keyword.put_new(opts, :name, name)
    encode = String.to_atom("#{opts[:name]}encode")
    decode = String.to_atom("#{opts[:name]}decode")
    fields = opts[:fields]
    # values = opts[:values]
    join = opts[:join]

    {fields, defs} =
      consolidate(fields, opts, [], [], 0)

    # |> IO.inspect(label: :consolidated)

    # Macro.to_string(defs)
    # |> IO.inspect(label: :defs)

    bbs =
      for {name, args} <- defs do
        do_bb(name, args)
        # IO.inspect(ast, label: :ast)
      end

    q =
      quote do
        for bb <- unquote(bbs),
            do: bb

        def unquote(encode)(kw \\ []) do
          encoded_kw =
            for {field, {encode, _}} <- unquote(fields) do
              {field, encode.(field, kw[field])}
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

              acc
              |> Map.put(:offset, offset)
              |> Map.put(:bin, bin)
              |> Map.put(:kw, Keyword.put(acc.kw, field, value))
            end)

          {map.offset, map.kw, map.bin}
        end
      end

    IO.puts(Macro.to_string(q))
    q
  end

  defmacro bb(name, args) do
    quote do
      unquote(do_bb(name, args))
    end
  end

  def do_bb(name, opts) do
    # TODO:
    # [x] add docstr?: true/[false] to generate docstrings or not
    #     if not, function is defined as private
    # [ ] add function_name: rdata, causes rdata_decode(id, ..) instead of #{id}_decode/encode
    # [?] add result_as_map?: true/[false] as shorthand for decode: fn kw -> Map.new(kw)
    # IO.inspect({name, opts}, label: :bitblock)
    # IO.inspect({Macro.expand(name, __ENV__), Macro.expand(opts, __ENV__)}, label: :expanded)
    encode = String.to_atom("#{name}encode")
    decode = String.to_atom("#{name}decode")

    fields = opts[:fields]
    values = opts[:values] || []

    encode_doc = if opts[:docstr], do: docstring(:encode, fields, values), else: "@doc false"
    encode_fun = opts[:encode]
    decode_doc = if opts[:docstr], do: docstring(:decode, fields, values), else: "@doc false"
    decode_fun = opts[:decode]

    all? = all?(fields)
    fields = if all?, do: fields, else: fields ++ [{:skip__, {:bits, [], []}}]
    keys = Enum.map(fields, fn {k, _} -> k end)
    vars = Enum.map(keys, fn k -> var(k) end)

    codec = fragments(fields)
    binds = bindings(fields)

    quote do
      @doc unquote(decode_doc)
      def unquote(decode)(offset \\ 0, bin) do
        <<_::bits-size(offset), rest::bits>> = bin
        unquote(codec) = rest
        kw = Enum.zip(unquote(keys), unquote(vars))
        skipped = kw[:skip__] || <<>>
        offset = offset + bit_size(bin) - bit_size(skipped)
        kw = Keyword.delete(kw, :skip__)
        fun = unquote(decode_fun)
        kw = if fun, do: fun.(offset, kw), else: kw
        {offset, kw, bin}
      end

      @doc unquote(encode_doc)
      def unquote(encode)(kw \\ []) when is_list(kw) do
        fun = unquote(encode_fun)
        kw = Keyword.merge(unquote(values), kw)
        kw = if fun, do: fun.(kw), else: kw
        kw = if unquote(all?), do: kw, else: Keyword.put(kw, :skip__, "")
        # introduce required var bindings to kw entries
        unquote_splicing(binds)
        unquote(codec)
      end
    end
  end
end
