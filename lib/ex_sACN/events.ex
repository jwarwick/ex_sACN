defmodule ExSACN.Events do

  def start_link(), do: start_link([])
  def start_link(_args) do
    {:ok, _pid} = :gen_event.start_link({:local, :sacn_events})
  end

  def subscribe(handler), do: subscribe(handler, [])
  def subscribe(handler, args) do
    :gen_event.add_handler(:sacn_events, handler, args)
  end

  def unsubscribe(handler), do: unsubscribe(handler, [])
  def unsubscribe(handler, args) do
    :gen_event.delete_handler(:sacn_events, handler, args)
  end

  def send_event(msg) do
    :gen_event.notify(:sacn_events, {:sacn_event, msg})
  end

end

