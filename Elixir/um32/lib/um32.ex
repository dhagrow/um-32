import Bitwise

defmodule State do
  defstruct [
    memory: Arrays.new(),
    finger: 0,
    reg: List.duplicate(0, 8) |> Enum.into(Arrays.new()),
    cycle: -1,
  ]
end

defmodule UM32 do
  def load(program) do
    File.stream!(program, [], 4)
    |> Enum.into(Arrays.new())
  end

  def process(state) do
    state = %{state | cycle: state.cycle + 1}
    # IO.puts("* finger: #{state.finger} (#{state.cycle})")

    %{memory: memory, finger: finger, reg: reg} = state

    platter = memory[0][finger]
    <<code::4, _rest::bits>> = platter

    case code do
      13 -> # ort
        <<_code::4, a::3, val::25>> = platter
        # IO.puts(" #{code}(#{a}, #{val})")
        UM32.process(%{state | finger: finger + 1, reg: put_in(reg[a], val)})
      _ ->
        <<code::4, _rest::19, a::3, b::3, c::3>> = platter
        # IO.puts(" #{code}(#{a}, #{b}, #{c})")
        # IO.write(" reg ")
        # IO.inspect(Enum.into(reg, []))

        state = case code do
          0 -> # cmv
            unless reg[c] == 0 do
              %{state | reg: put_in(reg[a], reg[b])}
            else
              state
            end
          1 -> # aix
            <<val>> = <<memory[reg[b]][reg[c]]>>
            %{state | reg: put_in(reg[a], val)}
          2 -> # aam
            %{state | memory: put_in(memory[reg[a]][reg[b]], reg[c])}
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
            <<val::unsigned>> = <<(~~~(reg[b] &&& reg[c]))::unsigned>>
            %{state | reg: put_in(reg[a], val)}
          10 -> # otp
            IO.write(<<reg[c]::utf8>>)
            state
          12 -> # lod
            %{state | memory: put_in(memory[0], memory[reg[b]]), finger: reg[c] - 1}
        end

        UM32.process(%{state | finger: finger + 1})
    end
  end
end
