-module(step3_env).
-export([main/1]).
-record(malval, {type=symbol, val}).
-record(malenv, {outer=nil, dict}).

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

env_new(O, L) ->
    #malenv{outer = O,
        dict = lists:foldl(fun([M|[V]], D) when is_record(M, malval) ->
                                dict:store(M#malval.val, V, D);
                            ([K|[V]], D) -> dict:store(K, V, D) end,
            dict:new(), L)}.

env_fetch(K, E) ->
    try dict:fetch(K, E#malenv.dict)
    catch _:_ ->
        if
            E#malenv.outer =:= nil ->
                fun(_A) -> 
                    Mq = string:concat("'", string:concat(K, "'")),
                    ErrMsg = string:concat(Mq, " not found"),
                    #malval{type=err, val=ErrMsg}
                end;
            true ->
                env_fetch(K, E#malenv.outer)
        end
    end.

env_store(K, V, E) ->
    #malenv{outer=E#malenv.outer,
        dict=dict:store(K#malval.val, V, E#malenv.dict)}.

eval_ast(M, Env) when is_record(M, malval) ->
    if
        M#malval.type =:= symbol ->
            {env_fetch(M#malval.val, Env), Env};
        M#malval.type =:= vector ->
            {V, NewEnv} = eval_ast(M#malval.val, Env),
            {#malval{type=vector, val=V}, NewEnv};
        M#malval.type =:= map ->
            {V, NewEnv} = eval_ast(M#malval.val, Env),
            {#malval{type=map, val=V}, NewEnv};
        true ->
            {M, Env}
    end;

eval_ast([], Env) -> {[], Env};
eval_ast([H|T], Env) ->
    {R, NewEnv} = eval(H, Env),
    {Tail, NEnv} = eval_ast(T, NewEnv),
    {[R|Tail], NEnv}.

eval_let([M|[V|[]]], Env) ->
    {R, NewEnv} = eval(V, Env),
    env_new(NewEnv, [[M#malval.val|[R]]|[]]);
eval_let([M|[V|T]], Env) ->
    {R, NewEnv} = eval(V, Env),
    NEnv = eval_let(T, env_new(NewEnv, [[M#malval.val|[R]]|[]])),
    NEnv.

eval([H|T], Env) ->
    if
        H#malval.val =:= "def!" ->
            [K|[J|_Tail]] = T,
            {V, NEnv} = eval(J, Env),
            {V, env_store(K, V, NEnv)};
        H#malval.val =:= "let*" ->
            [Binds|[Block|_Junk]] = T,
            BindList = if
                        is_record(Binds, malval) ->
                            Binds#malval.val;
                        true ->
                            Binds
                       end,
            NewEnv = eval_let(BindList, Env),
            if
                is_record(Block, malval) and Block#malval.type =:= vector ->
                    {R, _} = eval_ast(Block, NewEnv),
                    {#malval{type=vector, val=R}, Env};
                true ->
                    {R, _} = eval(Block, NewEnv),
                    {R, Env}
            end;
        true ->
            {[F|Tail], NEnv} = eval_ast([H|T], Env),
            {F(Tail), NEnv}
    end;
eval(Ast, Env) when is_record(Ast, malval) ->
    eval_ast(Ast, Env);

eval([], Env) -> {[], Env}.

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

rep(Data, Env) ->
    [NewEnv|Res] = lists:foldr(
            fun(S, [E|T]) ->
                {R, N} = eval(S, E), [N|[R|T]] end, [Env], read(Data)),
    print(Res),
    io:nl(),
    NewEnv.

main_loop(Env) ->
    case io:get_line("user> ") of
        {error, Description} ->
            io:write(Description),
            io:nl();
        eof -> io:nl();
        Data ->
            NewEnv = rep(Data, Env),
            main_loop(NewEnv)
    end.

main(_) ->
    Env = env_new(nil, [["*", fun(A) ->
                Fun = fun(_F, [], S) -> S;
                      (F, [H|T], S) when is_record(H, malval) ->
                    R = if
                            H#malval.type =:= integer ->
                                H#malval.val * S;
                            true ->
                                S
                            end,
                    F(F, T, R) end,
                #malval{type=integer, val=Fun(Fun, A, 1)} end],
            ["+", fun(A) ->
                Fun = fun(_F, [], S) -> S;
                      (F, [H|T], S) when is_record(H, malval) ->
                    R = if
                            H#malval.type =:= integer ->
                                H#malval.val + S;
                            true ->
                                S
                        end,
                    F(F, T, R) end,
                #malval{type=integer, val=Fun(Fun, A, 0)} end],
            ["-", fun([A|T]) ->
                [B|_Tail] = T,
                #malval{type=integer, val=A#malval.val-B#malval.val} end],
            ["/", fun([A|T]) ->
                [B|_Tail] = T,
                #malval{type=integer, val=A#malval.val div B#malval.val} end]
          ]),
    main_loop(Env).
