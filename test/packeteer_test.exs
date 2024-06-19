defmodule PacketeerTest do
  use ExUnit.Case
  doctest Packeteer

  describe "fixed primitives" do
    test "uint" do
      code = """
      defmodule Uint do
        import Packeteer

        fixed("",
        fields: [
          a: uint(8)
        ],
        defaults: [
          a: 128
        ]
        )
        end
      """

      [{uint, _}] = Code.compile_string(code)

      # encode using default values
      assert <<128>> == uint.encode([])

      # encode using keyword list
      for n <- 0..255,
          do: assert(<<n>> == uint.encode(a: n))

      # encode ignores unspecified fields
      assert <<127>> == uint.encode(a: 127, b: 42)

      # encode unsigned
      assert <<255>> == uint.encode(a: -1)
      assert <<128>> == uint.encode(a: -128)
      assert <<0>> == uint.encode(a: -256)

      # decode returns {offset, kw-list, binary}
      for n <- 0..255,
          do: assert({8, [a: n], <<n>>} == uint.decode(0, <<n>>))

      # decode ignores remaining bits
      assert {8, [a: 0], <<0, 42>>} == uint.decode(0, <<0, 42>>)
    end
  end
end
