defmodule ExSACN.Parser do
  use GenServer

  def start_link, do: start_link([])
  def start_link(_options) do
    GenServer.start_link(__MODULE__, [], name: :sacn_parser)
  end

  def handle_info({:sacn_msg, data}, state) do
    result = SACN.Message.parse data
    ExSACN.Events.send_event(result)
    {:noreply, state}
  end

end

