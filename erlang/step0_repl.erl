-module(step0_repl).
-export([main/1]).

read(Arg) -> Arg.

eval(Arg) -> Arg.

print(Arg) -> Arg.

rep(Data) -> print(eval(read(Data))).
    
main(_) ->
    case io:get_line("user> ") of
        {error, Description} ->
            io:write(Description),
            io:nl();
        eof -> io:nl();
        Data ->
            io:fwrite(rep(Data)),
            main(0)
    end.
