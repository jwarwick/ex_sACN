defmodule ExSACN.Logger do
  @moduledoc """
  Simple logging module that demonstrates how to 
  subscribe to and handle events.
  """
  use GenEvent.Behaviour

  def start_logger do
    pid = {ExSACN.Logger, make_ref}
    :ok = ExSACN.Events.subscribe(pid)
    {:ok, pid}
  end

  def stop_logger(pid) do
    ExSACN.Events.unsubscribe(pid) 
  end

  def handle_event(event, event), do: {:ok, event}
  def handle_event(event, _state) do
    IO.puts "Logger: #{inspect event}"
    {:ok, event}
  end
end

