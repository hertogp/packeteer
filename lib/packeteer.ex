defmodule Packeteer do
  @moduledoc """
  Declaratively create encode/decode functions for binary data.

  """

  # [[ HELPERS ]]

  defp all?(fields) do
    # returns true if expression matches all bits
    {_name, {fld, _, arg}} = List.last(fields)
    fld in [:bits, :bitstring, :bytes, :binary] and arg in [[], [nil]]

    # if fld in [:bits, :bitstring, :bytes, :binary] do
    #   case arg do
    #     [] -> true
    #     [nil] -> true
    #     _ -> false
    #   end
    # else
    #   false
    # end
  end

  # turns atom's into var in an expression
  defp walk(ast) do
    Macro.prewalk(ast, fn
      v when is_atom(v) -> var(v)
      v -> v
    end)
  end

  # turn atom's into vars, keeping literals as-is
  defp var(v) do
    case v do
      v when is_atom(v) -> Macro.var(v, __MODULE__)
      v when is_tuple(v) -> walk(v)
      v -> v
    end
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

  # [[ GENERATOR ]]

  @doc """
  A macro that creates `\#{name}encode` and `\#{name}decode` functions for given
  `name`, `field`-definitions and `opts`.


  `args` is a keyword list, where:
  - `fields` is a mandatory (keyword) list of named fields to encode/decode
  - `values` is an optional (keyword) list defining default values for one or more fields
  - `beforehand`, an anonymous function that transforms input keyword list prior to encoding
  - `afterwards`, an anonymous function that transforms output keyword list to its final result


  """
  defmacro create(name, fields, opts, body) do
    IO.inspect(body)
    encode = String.to_atom("#{name}encode")
    decode = String.to_atom("#{name}decode")

    # fields = args[:fields]
    values = opts[:values] || []

    encode_doc = docstring(:encode, fields, values)
    encode_fun = opts[:encode]
    decode_doc = docstring(:decode, fields, values)
    decode_fun = opts[:decode]

    all? = all?(fields)
    fields = if all?, do: fields, else: fields ++ [{:skip__, {:bits, [], []}}]
    keys = Enum.map(fields, fn {k, _} -> k end)
    vars = Enum.map(keys, fn k -> var(k) end)

    codec = fragments(fields)
    binds = bindings(fields)

    qq =
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
          if fun, do: fun.(offset, kw), else: {offset, kw}
        end

        @doc unquote(encode_doc)
        def unquote(encode)(kw) when is_list(kw) do
          fun = unquote(encode_fun)
          kw = Keyword.merge(unquote(values), kw)
          kw = if fun, do: fun.(kw), else: kw
          kw = if unquote(all?), do: kw, else: Keyword.put(kw, :skip__, "")
          # introduce required var bindings to kw entries
          unquote_splicing(binds)
          unquote(codec)
        end
      end

    IO.puts(Macro.to_string(qq))
    qq
  end
end
