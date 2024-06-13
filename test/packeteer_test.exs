defmodule PacketeerTest do
  use ExUnit.Case
  doctest Packeteer

  test "greets the world" do
    assert Packeteer.hello() == :world
  end
end
