-module(step1_read_print).
-export([main/1]).
-record(malval, {type=symbol, val}).

build_ast([], Ast) -> Ast;

build_ast([[[], Token]|Rest], Ast) ->
    if
        Token =:= ";" ->
            Ast;
        Token =:= "{" ->
            case build_ast(Rest, []) of
                {Subast, More} ->
                    build_ast(More, [#malval{type=map, val=Subast}|Ast]);
                Subast ->
                    [#malval{type=map, val=Subast}|Ast]
            end;
        Token =:= "[" ->
            case build_ast(Rest, []) of
                {Subast, More} ->
                    build_ast(More, [#malval{type=vector, val=Subast}|Ast]);
                Subast ->
                    [#malval{type=vector, val=Subast}|Ast]
            end;
        Token =:= "(" ->
            case build_ast(Rest, []) of
                {Subast, More} ->
                    build_ast(More, [Subast|Ast]);
                Subast ->
                    [Subast|Ast]
            end;
        Token =:= "]" ->
            {lists:reverse(Ast), Rest};
        Token =:= "}" ->
            {lists:reverse(Ast), Rest};
        Token =:= ")" ->
            {lists:reverse(Ast), Rest};
        Token =:= "" ->
            build_ast(Rest, Ast);
        Token =:= "'" ->
            case build_ast(Rest, []) of
                {Subast, More} ->
                    Qast = [#malval{type=sp, val="quote"}|Subast],
                    build_ast(More, [Qast|Ast]);
                Subast ->
                    Qast = [#malval{type=sp, val="quote"}|Subast],
                    [Qast|Ast]
            end;
        Token =:= "`" ->
            case build_ast(Rest, []) of
                {Subast, More} ->
                    Qast = [#malval{type=sp, val="quasiquote"}|Subast],
                    build_ast(More, [Qast|Ast]);
                Subast ->
                    Qast = [#malval{type=sp, val="quasiquote"}|Subast],
                    [Qast|Ast]
            end;
        Token =:= "~@" ->
            case build_ast(Rest, []) of
                {Subast, More} ->
                    QAst = [#malval{type=sp, val="splice-unquote"}|Subast],
                    build_ast(More, [QAst|Ast]);
                Subast ->
                    QAst = [#malval{type=sp, val="splice-unquote"}|Subast],
                    [QAst|Ast]
            end;
        Token =:= "~" ->
            case build_ast(Rest, []) of
                {Subast, More} ->
                    QAst = [#malval{type=sp, val="unquote"}|Subast],
                    build_ast(More, [QAst|Ast]);
                Subast ->
                    QAst = [#malval{type=sp, val="unquote"}|Subast],
                    [QAst|Ast]
            end;
        Token =:= "^" ->
            case build_ast(Rest, []) of
                {Subast, More} ->
                    QAst = [#malval{type=sp, val="with-meta"}|Subast],
                    build_ast(More, [QAst|Ast]);
                Subast ->
                    QAst = [#malval{type=sp, val="with-meta"}|Subast],
                    [QAst|Ast]
            end;
        Token =:= "@" ->
            case build_ast(Rest, []) of
                {Subast, More} ->
                    QAst = [#malval{type=sp, val="deref"}|Subast],
                    build_ast(More, [QAst|Ast]);
                Subast ->
                    QAst = [#malval{type=sp, val="deref"}|Subast],
                    [QAst|Ast]
            end;
        true ->
            case re:run(Token, "^[0-9]+$") of
                {match, _} ->
                    {N, _Ntail} = string:to_integer(Token),
                    build_ast(Rest, [#malval{type=integer, val=N}|Ast]);
                _ ->
                    build_ast(Rest, [#malval{val=Token}|Ast])
            end
    end.

read(Arg) -> 
    Tokens = re:split(Arg, "[\\s,]*(~@|[\\[\\]{}()'`~^@]|\"(?:\\\\.|[^\\\\\"])*\"|;|[^\\s\\\[\\]{}('\"`,;)]*)",  [{return, list}, group, trim]),
    try lists:reverse(build_ast(Tokens, []))
    catch _:_ -> [#malval{type=err, val="Invalid input"}]
    end.

eval(Ast) -> Ast.

print([]) -> shallow;
print([M|T]) when is_record(M, malval) ->
    V = if
            M#malval.type =:= integer ->
                integer_to_list(M#malval.val);
            M#malval.type =:= vector ->
                io:fwrite("["),
                print(M#malval.val),
                io:fwrite("]"),
                [];
            M#malval.type =:= map ->
                io:fwrite("{"),
                print(M#malval.val),
                io:fwrite("}"),
                [];
            true ->
                M#malval.val
        end,
    lists:foreach(fun(S) -> io:fwrite([S]) end, V),
    if
        T =/= [] ->
            io:fwrite(" ");
        true ->
            ok
    end,
    print(T);

print([H|T]) ->
    io:fwrite("("),
    print(H),
    io:fwrite(")"),
    print(T).

rep(Data) -> print(eval(read(Data))), io:nl().
    
main(_) ->
    case io:get_line("user> ") of
        {error, Description} ->
            io:write(Description),
            io:nl();
        eof -> io:nl();
        Data ->
            rep(Data),
            main(0)
    end.
