defmodule ExSACN.Supervisor do
  use Supervisor

  def start_link do
    :supervisor.start_link(__MODULE__, [])
  end

  def init([]) do
    children = [
      # Define workers and child supervisors to be supervised
      # worker(ExSACN.Worker, [])
      worker(ExSACN.Parser, []),
      worker(ExSACN.Listener, []),
      worker(ExSACN.Sender, []),
    ]

    {:ok, _pid} = ExSACN.Events.start_link

    # See http://elixir-lang.org/docs/stable/Supervisor.Behaviour.html
    # for other strategies and supported options
    supervise(children, strategy: :one_for_one)
  end
end
