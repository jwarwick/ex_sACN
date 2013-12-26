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

  def init(_args) do
    {:ok, {[], 0}}
  end

  def handle_event(event, {event, cnt}), do: {:ok, {event, cnt+1}}
  def handle_event(event, {_state, cnt}) do
    if 0 != cnt do
      IO.puts "Logger: Last message repeated #{cnt} times"
    end
    IO.puts "Logger: #{inspect event}"
    {:ok, {event, 0}}
  end
end

