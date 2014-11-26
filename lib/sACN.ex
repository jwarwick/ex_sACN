defmodule SACN do
  @moduledoc """
  Streaming ACN constants and helpers
  """

  @acn_sdt_port 5568

  def port, do: @acn_sdt_port

  def ip_from_universe(universe) do
    <<hi::8, lo::8>> = <<universe::size(16)-big>>
    {239, 255, hi, lo}
  end

end

