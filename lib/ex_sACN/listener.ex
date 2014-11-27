defmodule ExSACN.Listener do
  @moduledoc """
  GenServer that listens for multicast sACN packets.
  You must subscribe to a given universe number to receive packets.
  All packets received are forwarded to `ExSACN.Parser`.
  """

  use GenServer
  require Logger

  @server_name :sacn_listener

  @doc """
  Listen for sACN packets multicast to the given universe
  """
  def subscribe(universe) do
    GenServer.cast(@server_name, {:subscribe, universe})
  end

  @doc """
  Stop listening for sACN packets multicast to the given universe
  """
  def unsubscribe(universe) do
    GenServer.cast(@server_name, {:unsubscribe, universe})
  end
  
  def start_link, do: start_link([])
  def start_link(_options) do
    GenServer.start_link(__MODULE__, [], name: @server_name)
  end

  def init(_args) do
    {:ok, socket} = :gen_udp.open(SACN.port, [:binary, {:active, true}, {:broadcast, true}, {:reuseaddr, true},
                                 {:recbuf, 128000}, {:read_packets, 256}, {:multicast_loop, false}])
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
    Logger.info "Subscribing #{inspect ipAddress} to universe #{universeNumber} (#{inspect multicastAddr})"
  end

  defp multicast_unsubscribe(socket, ipAddress, universeNumber) do
    multicastAddr = SACN.ip_from_universe(universeNumber)
    :ok = :inet.setopts(socket, [{:drop_membership, {multicastAddr, ipAddress}}])
    Logger.info "Unsubscribing #{inspect ipAddress} to universe #{universeNumber} (#{inspect multicastAddr})"
  end

  def handle_info(_msg = {:udp, _socket, _ip, _send_port, data}, state) do
    send :sacn_parser, {:sacn_msg, data}
    {:noreply, state}
  end

  def handle_cast({:subscribe, universe}, socket) do
    Enum.each(get_interfaces(), &multicast_subscribe(socket, &1, universe))
    {:noreply, socket}
  end
  def handle_cast({:unsubscribe, universe}, socket) do
    Enum.each(get_interfaces(), &multicast_unsubscribe(socket, &1, universe))
    {:noreply, socket}
  end

end

