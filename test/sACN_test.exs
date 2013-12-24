defmodule SACNTest do
  use ExUnit.Case

  test "correct port" do
    assert 5568 = SACN.port
  end

  test "computes multicast ip" do
    assert {239, 255, 0, 0} = SACN.ip_from_universe(0)
    assert {239, 255, 255, 255} = SACN.ip_from_universe(65535)
    assert {239, 255, 1, 0} = SACN.ip_from_universe(256)
  end
end
