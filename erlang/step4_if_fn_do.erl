-module(step4_if_fn_do).
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
        Token =:= "nil" ->
            build_ast(Rest, [#malval{type=sp, val="nil"}|Ast]);
        Token =:= "true" ->
            build_ast(Rest, [#malval{type=sp, val="true"}|Ast]);
        Token =:= "false" ->
            build_ast(Rest, [#malval{type=sp, val="false"}|Ast]);
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

base_env() ->
    env_new(nil, [
            ["*", #malval{type=function, val=fun(A, _E) ->
                Fun = fun(_F, [], S) -> S;
                      (F, [H|T], S) when is_record(H, malval) ->
                    R = if
                            H#malval.type =:= integer ->
                                H#malval.val * S;
                            true ->
                                S
                            end,
                    F(F, T, R) end,
                #malval{type=integer, val=Fun(Fun, A, 1)} end}],
            ["+", #malval{type=function, val=fun(A, _E) ->
                Fun = fun(_F, [], S) -> S;
                      (F, [H|T], S) when is_record(H, malval) ->
                    R = if
                            H#malval.type =:= integer ->
                                H#malval.val + S;
                            true ->
                                S
                        end,
                    F(F, T, R) end,
                #malval{type=integer, val=Fun(Fun, A, 0)} end}],
            ["-", #malval{type=function, val=fun([A|T], _E) ->
                [B|_Tail] = T,
                #malval{type=integer, val=A#malval.val-B#malval.val} end}],
            ["/", #malval{type=function, val=fun([A|T], _E) ->
                [B|_Tail] = T,
                #malval{type=integer, val=A#malval.val div B#malval.val} end}],
            ["list", #malval{type=function, val=fun(A, _E) -> A end}],
            ["list?", #malval{type=function, val=fun([[_A]|_B], _E) ->
                                                #malval{type=sp, val="true"};
                                                    ([[]|_B], _E) ->
                                                #malval{type=sp, val="true"};
                                                    (_, _E) ->
                                                #malval{type=sp, val="false"}
                                            end}],
            ["empty?", #malval{type=function, val=fun([[]|_A], _E) ->
                                                #malval{type=sp, val="true"};
                                                    (_, _E) ->
                                                #malval{type=sp, val="false"}
                                            end}],
            ["count", #malval{type=function, val=fun([A|_B], _E) when is_record(A, malval) ->
                                                Falsehood = is_false(A),
                                                if Falsehood ->
                                                    #malval{type=integer, val=0};
                                                   true ->
                                                    #malval{type=integer, val=1}
                                                end;
                                                    ([A|_B], _E) ->
                                                #malval{type=integer,
                                                        val=length(A)}
                                            end}],
            ["=", #malval{type=function, val=fun([A|[B|_C]], _E) ->
                                                equals(A, B) end}],
            [">", #malval{type=function, val=fun([A|[B|_C]], _E) when is_record(A, malval) and is_record(B, malval)->
                if (A#malval.type =:= integer) and (B#malval.type =:= integer) ->
                        if
                            (A#malval.val > B#malval.val) ->
                                #malval{type=sp, val="true"};
                            true ->
                                #malval{type=sp, val="false"}
                        end
                    end;
                                                ([_A|[_B|_C]], _E) -> #malval{type=sp, val="false"}
                                            end}],
            [">=", #malval{type=function, val=fun([A|[B|_C]], _E) when is_record(A, malval) and is_record(B, malval)->
                if (A#malval.type =:= integer) and (B#malval.type =:= integer) ->
                        if
                            (A#malval.val >= B#malval.val) ->
                                #malval{type=sp, val="true"};
                            true ->
                                #malval{type=sp, val="false"}
                        end
                    end;
                                                ([_A|[_B|_C]], _E) -> #malval{type=sp, val="false"}
                                            end}],
            ["<", #malval{type=function, val=fun([A|[B|_C]], _E) when is_record(A, malval) and is_record(B, malval)->
                if (A#malval.type =:= integer) and (B#malval.type =:= integer) ->
                        if
                            (A#malval.val < B#malval.val) ->
                                #malval{type=sp, val="true"};
                            true ->
                                #malval{type=sp, val="false"}
                        end
                    end;
                                                ([_A|[_B|_C]], _E) -> #malval{type=sp, val="false"}
                                            end}],
            ["<=", #malval{type=function, val=fun([A|[B|_C]], _E) when is_record(A, malval) and is_record(B, malval)->
                if (A#malval.type =:= integer) and (B#malval.type =:= integer) ->
                        if
                            (B#malval.val >= A#malval.val) ->
                                #malval{type=sp, val="true"};
                            true ->
                                #malval{type=sp, val="false"}
                        end
                    end;
                                                ([_A|[_B|_C]], _E) -> #malval{type=sp, val="false"}
                                            end}],
            ["pr-str", #malval{type=function, val=fun(A, _E) -> #malval{type=string, val=pr_str(A, true)} end}],
            ["str", #malval{type=function, val=fun(A, _E) -> #malval{type=string, val=pr_str(A, false)} end}],
            ["prn", #malval{type=function, val=fun(A, _E) -> io:fwrite(malval_to_str(A, true)), io:nl(), #malval{type=sp, val="nil"} end}],
            ["println", #malval{type=function, val=fun(A, _E) -> io:fwrite(malval_to_str(A, false)), io:nl(), #malval{type=sp, val="nil"} end}]
          ]).

pr_str(A, B) ->
    S = if
            not B ->
                string:join(lists:map(fun(M) -> malval_to_str(M, B) end, A), "");
            true ->
                malval_to_str(A, B)
        end,
    string:concat("\"", string:concat(re:replace(re:replace(S, "\\\\", "\\\\\\\\", [{return, list}, global]), "\"", "\\\\\"", [{return, list}, global]), "\"")).

is_false(M) when is_record(M, malval) ->
    (M#malval.type =:= sp) and
    ((M#malval.val =:= "nil") or (M#malval.val =:= "false"));
is_false(_) -> false.

equals([], []) -> #malval{type=sp, val="true"};
equals(A, []) when is_record(A, malval) ->
    if A#malval.type =:= vector ->
        equals(A#malval.val, []);
       true ->
        #malval{type=sp, val="false"}
    end;
equals(A, [BH|BT]) when is_record(A, malval) ->
    if A#malval.type =:= vector ->
        equals(A#malval.val, [BH|BT]);
       BT =:= [] ->
        equals(A, BH);
       true ->
        #malval{type=sp, val="false"}
    end;
equals([], B) when is_record(B, malval) ->
    if B#malval.type =:= vector ->
        equals([], B#malval.val);
       true ->
        #malval{type=sp, val="false"}
    end;
equals([AH|AT], B) when is_record(B, malval) ->
    if B#malval.type =:= vector ->
        equals([AH|AT], B#malval.val);
       AT =:= [] ->
        equals(AH, B);
       true ->
        #malval{type=sp, val="false"}
    end;
equals([AH|AT], [BH|BT]) ->
    Alen = length(AT),
    Blen = length(BT),
    if Alen =:= Blen ->
        HeadEq = equals(AH, BH),
        if HeadEq#malval.val =:= "true" -> equals(AT, BT);
           true -> #malval{type=sp, val="false"}
        end;
       true -> #malval{type=sp, val="false"}
    end;
equals(A, B) when is_record(A, malval) and is_record(B, malval) ->
    if (A#malval.type =:= B#malval.type) and (A#malval.val =:= B#malval.val) ->
        #malval{type=sp, val="true"};
       true -> #malval{type=sp, val="false"}
    end;
equals(_A, _B) -> #malval{type=sp, val="false"}.

env_merge(N, E) when is_record(E, malenv) and is_record(N, malenv) ->
    if  E#malenv.outer =:= nil -> #malenv{outer=N, dict=E#malenv.dict};
        true -> env_merge(N, E#malenv.outer)
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
                Mq = string:concat("'", string:concat(K, "'")),
                ErrMsg = string:concat(Mq, " not found"),
                #malval{type=err, val=ErrMsg};
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
    NEnv = eval_let(T, env_new(NewEnv, [[M#malval.val, R]])),
    NEnv.

bind_exprs_list([], []) -> [];
bind_exprs_list([Bind|Binds], []) -> [[Bind, []]|bind_exprs_list(Binds, [])];
bind_exprs_list([Bind|Binds], [Expr|Exprs]) ->
    if
        Bind#malval.val =:= "&" ->
            [FinBind|_FinBinds] = Binds,
            [[FinBind, [Expr|Exprs]]];
        true ->
            [[Bind, Expr]|bind_exprs_list(Binds, Exprs)]
    end.

eval([H|T], Env) ->
    if
        H#malval.val =:= "def!" ->
            [K|[J|_Tail]] = T,
            {V, NEnv} = eval(J, Env),
            {V, env_store(K, V, NEnv)};
        H#malval.val =:= "do" ->
            {L, NEnv} = eval_ast(T, Env),
            {lists:last(L), NEnv};
        H#malval.val =:= "if" ->
            [Cond|[Then|Junk]] = T,
            {Test, NewEnv} = eval(Cond, Env),
            Falsehood = is_false(Test),
            if
                Falsehood ->
                    if
                        Junk =:= [] ->
                            {#malval{type=sp, val="nil"}, NewEnv};
                        true ->
                            [Else|_Misc] = Junk,
                            eval(Else, NewEnv)
                    end;
                true ->
                    eval(Then, NewEnv)
            end;
        H#malval.val =:= "fn*" ->
            Fun = fun([A|[B|_C]]) ->
                fun(E, N) ->
                    Binds = if is_record(A, malval) ->
                                A#malval.val;
                            true ->
                                A
                            end,
                    {R, _} = eval(B, env_new(env_merge(N, Env),
                                bind_exprs_list(Binds, E))),
                    R
                end
            end,
            {#malval{type=function, val=Fun(T)}, Env};
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
            R = if is_record(F, malval) ->
                    if F#malval.type =:= err ->
                            fun(_A, _E) -> F end;
                        true ->
                            F#malval.val
                    end;
                   true ->
                    [In] = F,
                    In#malval.val
                end,
            {R(Tail, Env), NEnv}
    end;
eval(Ast, Env) when is_record(Ast, malval) ->
    eval_ast(Ast, Env);

eval([], Env) -> {[], Env}.

malval_to_str([], _B) -> "";
malval_to_str(M, B) when is_record(M, malval) ->
    if
        M#malval.type =:= integer ->
            integer_to_list(M#malval.val);
        M#malval.type =:= function ->
            "#";
        M#malval.type =:= vector ->
            string:concat("[", string:concat(malval_to_str(M#malval.val, B), "]"));
        M#malval.type =:= map ->
            string:concat("{", string:concat(malval_to_str(M#malval.val, B), "}"));
        (M#malval.type =:= string) and (not B) ->
            S = M#malval.val,
            re:replace(re:replace(S, "(^\"|\"$)", "", [{return, list}, global]), "\\\\(.)", "\\1", [{return, list}, global]);
        true ->
            M#malval.val
    end;
malval_to_str([[M]|[]], B) when is_record(M, malval) -> malval_to_str(M, B);
malval_to_str([M|[]], B) when is_record(M, malval) -> malval_to_str(M, B);
malval_to_str([M|T], B) when is_record(M, malval) ->
    string:concat(malval_to_str(M, B), string:concat(" ", malval_to_str(T, B)));
malval_to_str([H|[]], B) ->
    string:concat("(", string:concat(malval_to_str(H, B), ")"));
malval_to_str([H|T], B) ->
    string:concat("(", string:concat(malval_to_str(H, B),
        string:concat(") ", malval_to_str(T, B)))).

print([]) -> ok;
print([[M]|[]]) when is_record(M, malval) -> print([M]);
print([M|T]) when is_record(M, malval) ->
    V = malval_to_str(M, true),
    lists:foreach(fun(S) -> io:fwrite([S]) end, V),
    if
        T =/= [] ->
            io:fwrite(" ");
        true ->
            ok
    end,
    print(T);

print([H|T]) ->
    io:fwrite(malval_to_str([H|T], false)).
    %% print(H),
    %% io:fwrite(")"),
    %% print(T).

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
    Env = base_env(),
    main_loop(rep("(def! not (fn* (a) (if a false true)))", Env)).
