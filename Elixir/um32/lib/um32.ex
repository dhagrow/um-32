import Bitwise

defmodule State do
  defstruct [
    :memory,
    finger: 0,
    reg: List.duplicate(0, 8) |> Enum.into(Arrays.new()),
    cycle: 0,
    halt: false,
    index: 0,
  ]
end

defmodule UM32 do
  def load(program) do
    File.stream!(program, [], 4) |> Enum.into(Arrays.new())
  end

  def process(%State{halt: true} = _state) do IO.puts("halt") end
  def process(state) do
    %{memory: memory, finger: finger, reg: reg, cycle: cycle, index: index} = state

    finger = finger + 1
    cycle = cycle + 1
    state = %{state | finger: finger, cycle: cycle}
    # dump_state(state)

    platter = memory[0][finger]
    <<code::4, _rest::bits>> = platter

    case code do
      _ ->
        <<code::4, _rest::19, a::3, b::3, c::3>> = platter

        state = case code do
          0 -> # cmv
            unless reg[c] == 0 do
              %{state | reg: put_in(reg[a], reg[b])}
            else
              state
            end
          1 -> # aix
            <<val::32>> = memory[reg[b]][reg[c]]
            %{state | reg: put_in(reg[a], val)}
          2 -> # aam
            %{state | memory: put_in(memory[reg[a]][reg[b]], <<reg[c]::32>>)}
          3 -> # add
            <<val::32>> = <<(reg[b] + reg[c])::32>>
            %{state | reg: put_in(reg[a], val)}
          4 -> # mul
            <<val::32>> = <<(reg[b] * reg[c])::32>>
            %{state | reg: put_in(reg[a], val)}
          5 -> # dvi
            <<val::32>> = <<div(reg[b], reg[c])::32>>
            %{state | reg: put_in(reg[a], val)}
          6 -> # nad
            <<val::size(32)-unsigned>> = <<(~~~(reg[b] &&& reg[c]))::size(32)-unsigned>>
            %{state | reg: put_in(reg[a], val)}
          7 -> # hlt
            %{state | halt: true}
          8 -> # alc
            index = index + 1
            memory = Map.put(memory, index, List.duplicate(<<0::32>>, reg[c]) |> Enum.into(Arrays.new()))
            %{state | memory: memory, reg: put_in(reg[b], index), index: index}
          9 -> # abd
            %{state | memory: Map.delete(memory, reg[c])}
          10 -> # otp
            IO.write(<<reg[c]::utf8>>)
            state
          # 11 -> # inp
          # Not implemented. See README
          12 -> # lod
            %{state | memory: put_in(memory[0], memory[reg[b]]), finger: reg[c] - 1}
          13 -> # ort
            <<_code::4, a::3, val::25>> = platter
            %{state | reg: put_in(reg[a], val)}
        end

        UM32.process(state)
    end
  end

  def dump_state(state) do
    %{memory: memory, finger: finger, reg: reg, cycle: cycle, index: index} = state

    platter = memory[0][finger]
    <<code::4, _rest::bits>> = platter

    IO.puts("## finger: #{finger} (#{cycle}) i: #{index}")
    case code do
      13 -> # ort
        <<_code::4, a::3, val::25>> = platter
        IO.puts(" #{code}(#{a}, #{val})")
      _ ->
        <<code::4, _rest::19, a::3, b::3, c::3>> = platter
        IO.puts(" #{code}(#{a}, #{b}, #{c})")
    end

    IO.write(" reg ")
    IO.inspect(Enum.into(reg, []))

    IO.write(" memory ")
    IO.inspect(Enum.map(memory, fn {_k, v} -> Arrays.size(v) end))

  end

end
