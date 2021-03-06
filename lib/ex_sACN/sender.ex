defmodule ExSACN.Sender do
  use GenServer

  # def send_message(ip_tuple, port, {path, args}) do
  #   data = OSC.Message.construct(path, args)
  #   :gen_server.cast(:osc_sender, {:osc_message, ip_tuple, port, data})
  # end

  def start_link() do
    GenServer.start_link(__MODULE__, [], name: :sacn_sender)
  end

  def init(_args) do
    :gen_udp.open(0, [:binary, {:active, true}])
  end
  
  def handle_cast({:sacn_message, ip, port, data}, socket) do
    :ok = :gen_udp.send(socket, ip, port, data)
    {:noreply, socket}
  end
end

