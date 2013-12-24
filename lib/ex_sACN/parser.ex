defmodule ExSACN.Parser do
  use GenServer.Behaviour

  def start_link, do: start_link([])
  def start_link(_options) do
    :gen_server.start_link({:local, :sacn_parser}, __MODULE__, [], [])
  end

  def handle_info({:sacn_msg, data}, state) do
    result = SACN.Message.parse data
    ExSACN.Events.send_event(result)
    {:noreply, state}
  end

end

