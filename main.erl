-module(main).
-export([main/0]).

count_numbers(Start, End) ->
    Count = lists:seq(Start, End),
    length(Count).

spawn_counter(Start, End) ->
    Parent = self(),
    Pid = spawn(fun() -> Parent ! {self(), count_numbers(Start, End)} end),
    {Pid, Start, End}.

collect_results(0, Acc) ->
    Acc;
collect_results(N, Acc) ->
    receive
        {Pid, Count} ->
            Remaining = N - 1,
            collect_results(Remaining, [{Pid, Count} | Acc])
    end.

main() ->
    NumCores = erlang:system_info(logical_processors),
    PerChunk = 1000000000 div NumCores,
    Remainder = 1000000000 rem NumCores,

    StartTime = os:timestamp(),

    Threads = [spawn_counter(I * PerChunk + 1, (I + 1) * PerChunk + (if I =:= NumCores - 1 -> Remainder; 0 end)) || I <- lists:seq(0, NumCores - 1)],
    Results = collect_results(length(Threads), []),
    TotalCount = lists:sum([Count || {_, Count} <- Results]),

    EndTime = os:timestamp(),
    ExecutionTime = timer:now_diff(EndTime, StartTime),

    io:format("Count: ~p~n", [TotalCount]),
    io:format("Execution Time: ~p microseconds~n", [timer:now_diff(ExecutionTime) div 1000]).
