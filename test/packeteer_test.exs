defmodule PacketeerTest do
  use ExUnit.Case
  doctest Packeteer

  defmodule Primitives do
    import Packeteer
    fixed("uint_", fields: [a: uint(8)], defaults: [a: 128])
    fixed("sint_", fields: [a: sint(8)], defaults: [a: 127])
    fixed("float16_", fields: [a: float(16)], defaults: [a: 42.5])
    fixed("float32_", fields: [a: float(32)], defaults: [a: 42.5])
    fixed("float64_", fields: [a: float(64)], defaults: [a: 42.5])
    fixed("binary_", fields: [a: binary(8)], defaults: [a: "01234567"])
    fixed("bytes_", fields: [a: bytes(8)], defaults: [a: "01234567"])
    fixed("bits_", fields: [a: bits(16)], defaults: [a: "42"])
    fixed("bitstring_", fields: [a: bitstring(16)], defaults: [a: "42"])
    fixed("utf8_", fields: [a: utf8()], defaults: [a: 8364])
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

    test "float16" do
      alias Primitives, as: P
      assert <<81, 80>> == P.float16_encode([])
      assert {16, [a: 42.5], <<81, 80>>} == P.float16_decode(0, <<81, 80>>)
      assert <<209, 80>> == P.float16_encode(a: -42.5)
      assert {16, [a: -42.5], <<209, 80>>} == P.float16_decode(0, <<209, 80>>)
    end

    test "float32" do
      alias Primitives, as: P
      assert <<66, 42, 0, 0>> == P.float32_encode([])
      assert {32, [a: 42.5], <<66, 42, 0, 0>>} == P.float32_decode(0, <<66, 42, 0, 0>>)
      assert <<194, 42, 0, 0>> == P.float32_encode(a: -42.5)
      assert {32, [a: -42.5], <<194, 42, 0, 0>>} == P.float32_decode(0, <<194, 42, 0, 0>>)
    end

    test "float64" do
      alias Primitives, as: P
      assert <<64, 69, 64, 0, 0, 0, 0, 0>> == P.float64_encode([])

      assert {64, [a: 42.5], <<64, 69, 64, 0, 0, 0, 0, 0>>} ==
               P.float64_decode(0, <<64, 69, 64, 0, 0, 0, 0, 0>>)

      assert <<192, 69, 64, 0, 0, 0, 0, 0>> == P.float64_encode(a: -42.5)

      assert {64, [a: -42.5], <<192, 69, 64, 0, 0, 0, 0, 0>>} ==
               P.float64_decode(0, <<192, 69, 64, 0, 0, 0, 0, 0>>)
    end

    test "binary" do
      # binary is shorthand for bytes
      alias Primitives, as: P
      assert "01234567" == P.binary_encode([])
      assert "76543210" == P.binary_encode(a: "76543210")
      # binary encoder only takes what it needs, no complaintss
      assert "76543210" == P.binary_encode(a: "76543210---")
      assert {64, [a: "76543210"], "76543210"} = P.binary_decode(0, "76543210")
      assert {64, [a: "76543210"], "76543210---"} = P.binary_decode(0, "76543210---")

      {:error, _reason} = P.binary_encode(a: "---")
    end

    test "bytes" do
      alias Primitives, as: P
      assert "01234567" == P.bytes_encode([])
      assert "76543210" == P.bytes_encode(a: "76543210")
      # bytes encoder only takes what it needs, no complaintss
      assert "76543210" == P.bytes_encode(a: "76543210---")
      assert {64, [a: "76543210"], "76543210"} = P.bytes_decode(0, "76543210")
      assert {64, [a: "76543210"], "76543210---"} = P.bytes_decode(0, "76543210---")

      {:error, reason} = P.bytes_encode(a: "---")
      assert is_binary(reason)
    end

    test "bits" do
      # bits is shorthand for bitstring
      alias Primitives, as: P
      assert "42" == P.bits_encode([])
      assert "24" == P.bits_encode(a: "24")
      # bits takes what it needs
      assert "24" == P.bits_encode(a: "24--")
      assert {16, [a: "99"], "99---"} = P.bits_decode(0, "99---")
      assert {40, [a: "99"], "---99"} = P.bits_decode(24, "---99")
      assert {40, [a: "99"], "---99---"} = P.bits_decode(24, "---99---")
    end

    test "bitstring" do
      alias Primitives, as: P
      assert "42" == P.bitstring_encode([])
      assert "24" == P.bitstring_encode(a: "24")
      # bitstring takes what it needs
      assert "24" == P.bitstring_encode(a: "24--")
      assert {16, [a: "99"], "99---"} = P.bitstring_decode(0, "99---")
      assert {40, [a: "99"], "---99"} = P.bitstring_decode(24, "---99")
      assert {40, [a: "99"], "---99---"} = P.bitstring_decode(24, "---99---")
    end

    test "utf8" do
      alias Primitives, as: P
      assert <<226, 130, 172>> == P.utf8_encode([])
      assert "€" == P.utf8_encode([])
    end
  end
end
