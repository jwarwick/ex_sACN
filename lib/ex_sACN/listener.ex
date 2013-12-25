defmodule ExSACN.Listener do
  use GenServer.Behaviour

  @default_universe 1

  def start_link, do: start_link([])
  def start_link(options) do
    universe = Keyword.get(options, :universe, @default_universe)
    :gen_server.start_link(__MODULE__, [universe: universe], [])
  end

  def init([universe: universe]) do
    {:ok, socket} = :gen_udp.open(SACN.port, [:binary, {:active, true}, {:broadcast, true}, {:reuseaddr, true},
                                 {:recbuf, 128000}, {:read_packets, 256}, {:multicast_loop, false}])
    lc ip inlist get_interfaces(), do: multicast_subscribe(socket, ip, universe)
    {:ok, socket}
  end

  def get_interfaces do
    {:ok, ifList} = :inet.getif
    ifList
      |> Enum.map(fn({ip, _broadcast, _subnet}) -> ip end)
      |> Enum.filter(&(!localhost?(&1)))
  end

  defp localhost?({127, 0, 0, 1}), do: true
  defp localhost?(_), do: false

  defp multicast_subscribe(socket, ipAddress, universeNumber) do
    multicastAddr = SACN.ip_from_universe(universeNumber)
    :ok = :inet.setopts(socket, [{:add_membership, {multicastAddr, ipAddress}}])
    IO.puts "Subscribing #{inspect ipAddress} to universe #{universeNumber} (#{inspect multicastAddr})"
  end

  def handle_info(_msg = {:udp, socket, _ip, _send_port, data}, state) do
    :sacn_parser <- {:sacn_msg, data}
    {:noreply, state}
  end
end

