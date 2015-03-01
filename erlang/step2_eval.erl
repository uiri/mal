-module(step2_eval).
-export([main/1]).
-record(malval, {type=symbol, val}).

build_ast([], Ast) -> Ast;

build_ast([[[], Token]|Rest], Ast) ->
    %% io:fwrite("Building..."),
    %% io:write(Ast),
    %% io:nl(),
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
            case re:run(Token, "^\".*\"$") of
                {match, _} ->
                    build_ast(Rest, [#malval{type=string, val=Token}|Ast]);
                _ ->
                    build_ast(Rest, [#malval{val=Token}|Ast])
            end
            end
    end.

read(Arg) -> 
    Tokens = re:split(Arg, "[\\s,]*(~@|[\\[\\]{}()'`~^@]|\"(?:\\\\.|[^\\\\\"])*\"|;|[^\\s\\\[\\]{}('\"`,;)]*)",  [{return, list}, group, trim]),
    try lists:reverse(build_ast(Tokens, []))
    catch _:_ -> [#malval{type=err, val="Invalid input"}]
    end.

eval_ast(M, Env) when is_record(M, malval) ->
    if
        M#malval.type =:= symbol ->
            try dict:fetch(M#malval.val, Env)
            catch _:_ -> fun(_A) -> 
                Mq = string:concat("'", string:concat(M#malval.val, "'")),
                ErrMsg = string:concat(Mq, " not found"),
                #malval{type=err, val=ErrMsg}
                end
            end;
        M#malval.type =:= vector ->
            #malval{type=vector, val=eval_ast(M#malval.val, Env)};
        M#malval.type =:= map ->
            #malval{type=map, val=eval_ast(M#malval.val, Env)};
        true ->
            M
    end;

eval_ast([], _Env) -> [];
eval_ast([H|T], Env) ->
    Tail = eval_ast(T, Env),
    [eval(H, Env)|Tail].

eval([H|T], Env) ->
    [F|Tail] = eval_ast([H|T], Env),
    F(Tail);

eval(Ast, Env) when is_record(Ast, malval) ->
    eval_ast(Ast, Env);

eval([], _Env) -> [].

print([]) -> ok;
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
    print(T);
print(F) ->
    io:nl(),
    io:fwrite((F([]))#malval.val),
    io:nl().

rep(Data) ->
    Env = dict:store("*", fun(A) ->
             Fun = fun(_F, [], S) -> S;
                      (F, [H|T], S) when is_record(H, malval) ->
                     R = if H#malval.type =:= integer ->
                                 H#malval.val * S;
                             true ->
                                 S
                         end,
                     F(F, T, R) end,
             #malval{type=integer, val=Fun(Fun, A, 1)} end,
          dict:store("+", fun(A) ->
             Fun = fun(_F, [], S) -> S;
                      (F, [H|T], S) when is_record(H, malval) ->
                     R = if H#malval.type =:= integer ->
                                 H#malval.val + S;
                             true ->
                                 S
                         end,
                     F(F, T, R) end,
             #malval{type=integer, val=Fun(Fun, A, 0)} end,
          dict:store("-", fun([A|T]) ->
             [B|_Tail] = T,
             #malval{type=integer, val=A#malval.val-B#malval.val} end,
          dict:store("/", fun([A|T]) ->
             [B|_Tail] = T,
             #malval{type=integer, val=A#malval.val div B#malval.val} end,
        dict:new())))),
    Res = lists:map(fun(S) -> eval(S, Env) end, read(Data)),
    print(Res),
    io:nl().
    
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
