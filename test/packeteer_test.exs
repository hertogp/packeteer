defmodule PacketeerTest do
  use ExUnit.Case
  doctest Packeteer

  defmodule Primitives do
    import Packeteer
    fixed("uint_", fields: [a: uint(8)], defaults: [a: 128])
    fixed("sint_", fields: [a: sint(8)], defaults: [a: 127])
  end

  describe "fixed primitives" do
    test "uint" do
      alias Primitives, as: P

      # encode using default values
      assert <<128>> == P.uint_encode([])

      # encode using keyword list
      for n <- 0..255,
          do: assert(<<n>> == P.uint_encode(a: n))

      # ignores unspecified fields
      assert <<127>> == P.uint_encode(a: 127, b: 42)

      # encode unsigned
      assert <<255>> == P.uint_encode(a: -1)
      assert <<128>> == P.uint_encode(a: -128)
      assert <<0>> == P.uint_encode(a: -256)

      # decode returns {offset, kw-list, binary}
      for n <- 0..255,
          do: assert({8, [a: n], <<n>>} == P.uint_decode(0, <<n>>))

      # matches part of binary
      assert {8, [a: 0], <<0, 42>>} == P.uint_decode(0, <<0, 42>>)
    end

    test "sint" do
      alias Primitives, as: P

      assert <<127>> = P.sint_encode([])
      assert <<255>> = P.sint_encode(a: -1)
      # matches part of binary
      assert {8, [a: 0], <<0, 42>>} == P.uint_decode(0, <<0, 42>>)

      for n <- -128..127 do
        assert(<<n>> == P.sint_encode(a: n))
        assert({8, [a: n], <<n>>} == P.sint_decode(0, <<n>>))
      end
    end
  end
end
