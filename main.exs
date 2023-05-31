defmodule Main do
  @num_cpu System.schedulers_online()

  def count_numbers(start, end_num) do
    Enum.reduce(start..end_num, 0, fn _, acc -> acc + 1 end)
  end

  def spawn_counter(start, end_num) do
    parent = self()
    pid = spawn_link(fn -> send(parent, {:result, count_numbers(start, end_num)}) end)
    {pid, start, end_num}
  end

  def collect_results(0, acc) do
    Enum.reverse(acc)
  end

  def collect_results(n, acc) do
    receive do
      {:result, count} ->
        remaining = n - 1
        collect_results(remaining, [{count, self()} | acc])
    end
  end

  def main do
    per_chunk = div(1000000000, @num_cpu)
    remainder = rem(1000000000, @num_cpu)

    start_time = :os.system_time(:millisecond)

    threads =
      for i <- 0..@num_cpu - 1 do
        start = i * per_chunk + 1
        end_num = if i == @num_cpu - 1, do: start + per_chunk + remainder - 1, else: start + per_chunk - 1
        spawn_counter(start, end_num)
      end

    results = collect_results(length(threads), [])
    total_count = Enum.sum(Enum.map(results, &elem(&1, 0)))

    end_time = :os.system_time(:millisecond)
    execution_time = end_time - start_time

    IO.puts("Count: #{total_count}")
    IO.puts("Execution Time: #{execution_time} milliseconds")
  end
end

Main.main()
