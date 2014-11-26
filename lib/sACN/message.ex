defmodule SACN.Message do
  @moduledoc """
  Parse and construct Streaming ACN messages (ANSI E1.31).
  Protocol specification here: 
  http://tsp.plasa.org/tsp/documents/published_docs.php
  """

# @preamble_size 0x0010::[size(16), big]
# -define(POSTAMBLE_SIZE, 16#0000:16/big).
# -define(ACN_PACKET_ID, 16#4153432D45312E3137000000:96/big).

  # ANSI E1.31-2009
  def parse(<<0x0010::size(16)-big, 0x0000::size(16)-big, 0x4153432D45312E3137000000::size(96)-big, 
              0x7::4, pdu_length::size(12)-big, 
              0x00000004::size(32)-big, cid::size(128)-big, 
              flag_high::4, frame_pdu_length::size(12)-big, 0x00000002::size(32)-big, source_name::size(512)-big,
              priority::8, _reserved::size(16)-big, sequence_number::8, preview_data::1, stream_terminated::1, options_rest::6,
              universe::size(16)-big,
              0x7::4, dmp_pdu_length::size(12)-big, 0x02, 0xA1, 0x0000::size(16)-big,
              0x00001::size(16)-big, property_value_count::size(16)-big,
              start_code::8, data::binary>>) do
          # PrintableName = lists:filter(fun(X) -> X /= 0 end, binary_to_list(<<SourceName:512/big>>)),
          [printable_name, _rest] = :binary.split(<<source_name::size(512)-big>>, [<<0>>])
          #io:format("Got sacn packet seq:~w, universe:~w, length:~w from ~p~n", [SequenceNumber, Universe, PropertyValueCount, PrintableName]),
          {:sacn, universe, data, preview_data, stream_terminated}
  end

# draft E1-31R0-2 (2006)
# parse(<<?PREAMBLE_SIZE, ?POSTAMBLE_SIZE, ?ACN_PACKET_ID, 16#7:4, PDU_Length:12/big, 
#         16#00000003:32/big, CID:(16*8)/big, 
#         FlagHigh:4, Frame_PDU_Length:12/big, 16#00000002:32/big, SourceName:(32*8)/big,
#         Priority:8, SequenceNumber:8, 
#         Universe:16/big,
#         16#7:4, DMP_PDU_Length:12/big, 16#02, 16#A1, 16#0000:16/big,
#         16#00001:16/big, PropertyValueCount:16/big,
#         StartCode:8, Data/binary>>) ->
#         PrintableName = lists:filter(fun(X) -> X /= 0 end, binary_to_list(<<SourceName:512/big>>)),
#         %io:format("Got draft sacn packet seq:~w, universe:~w, length:~w from ~p~n", [SequenceNumber, Universe, PropertyValueCount, PrintableName]),
#         {sacn, Universe, Data, 0, 0};
# 
# parse(Data) ->
#   io:format("Unknown sacn data: ~w~n", [Data]).

  def construct(source_name, cid, priority, sequence_number, preview_data, stream_terminated, universe, start_code, data) do
    pdu_length = 0
    flag_high = 0
    frame_pdu_length = 0
    reserved = 0
    options_rest = 0
    dmp_pdu_length = 0
    property_value_count = 0
    
    <<0x0010::size(16)-big, 0x0000::size(16)-big, 0x4153432D45312E3137000000::size(96)-big, 
    0x7::4, pdu_length::size(12)-big, 
    0x00000004::size(32)-big, cid::size(128)-big, 
    flag_high::4, frame_pdu_length::size(12)-big, 0x00000002::size(32)-big, source_name::size(512)-big,
    priority::8, reserved::size(16)-big, sequence_number::8, preview_data::1, stream_terminated::1, options_rest::6,
    universe::size(16)-big,
    0x7::4, dmp_pdu_length::size(12)-big, 0x02, 0xA1, 0x0000::size(16)-big,
    0x00001::size(16)-big, property_value_count::size(16)-big,
    start_code::8, data::binary>>
  end
end

