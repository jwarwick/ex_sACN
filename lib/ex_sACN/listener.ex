defmodule ExSACN.Listener do
  use GenServer.Behaviour

  def start_link, do: start_link([])
  def start_link(options) do
    :gen_server.start_link(__MODULE__, [], [])
  end

  def init([]) do
    :gen_udp.open(SACN.port, [:binary, {:active, true}])
  end
  
  def handle_info(_msg = {:udp, _port, _ip, _send_port, data}, socket) do
    :sacn <- {:sacn_msg, data}
    {:noreply, socket}
  end
end

