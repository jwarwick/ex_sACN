defmodule ExSACN.Logger do
  use GenEvent.Behaviour

  def start_logger do
    pid = {ExSACN.Logger, make_ref}
    :ok = ExSACN.Events.subscribe(pid)
    {:ok, pid}
  end

  def stop_logger(pid) do
    ExSACN.Events.unsubscribe(pid) 
  end

  def handle_event(event, state) do
    IO.inspect event
    {:ok, state}
  end
end

