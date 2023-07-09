use Arrays

defmodule UM32 do
  def load(program) do
    {:ok, file} = File.open(program, [:binary, :read])

    IO.binread(file, 4)
  end
end

[program] = System.argv
IO.binwrite(:stdio, UM32.load(program))
IO.puts(Arrays.empty())
IO.puts("")
