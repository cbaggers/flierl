-module(flierl).
-export([compile/1]).

sexp(List) when is_list(List) ->
    io_lib:format("(~s)", [string:join(List, " ")]).

process_error_info({ErrorLine, Module, ErrorDescriptor}) ->
    Line = case ErrorLine of 
               none -> 0;
               L -> L
           end,
    io_lib:format("(~B ~p)", [Line, lists:flatten(Module:format_error(ErrorDescriptor))]).

transform_for_emacs(CompileResult) ->
    Sexp = 
        case CompileResult of
            {ok, _} -> "(nil nil)";
            {ok, _, []} -> "(nil nil)";
            {ok, _, [{_File, Warnings}]} -> 
                io_lib:format("(nil ~s)", [sexp([process_error_info(W) || W <- Warnings])]);
           
            error -> 
                "(((0 \"unknown error\")) nil)";
            {error, [{_File0, Errors}]} -> 
                io_lib:format("(~s nil)", [sexp([process_error_info(E) || E <- Errors])]);
            {error, [{_File0, Errors}], []} -> 
                io_lib:format("(~s nil)", [sexp([process_error_info(E) || E <- Errors])]);
            {error, [{_File0, Errors}, {_File1, Warnings}]} -> 
                io_lib:format("(~s ~s)", [sexp([process_error_info(E) || E <- Errors]),
                                          sexp([process_error_info(W) || W <- Warnings])]);
            {error, [{_File0, Errors}, {_File1, Warnings}], _} -> 
                io_lib:format("(~s ~s)", [sexp([process_error_info(E) || E <- Errors]),
                                          sexp([process_error_info(W) || W <- Warnings])])
        end,
    lists:flatten(Sexp).


compile_and_report(Path, ReportTo) ->
    Self = self(),
    Result = 
        try
            Report = compile:file(Path, [strong_validation, return]),
            transform_for_emacs(Report)
        catch
            Err ->
                erlang:display("SHI"),
                io_lib:format("(((0 \"Flierl Compile Error: ~p\")) nil)", 
                              [Err])
        end,
    erlang:display({"CockFace! reporting to", ReportTo, {Self, Result}}),
    ReportTo ! {Self, Result}.

clear_mailbox() ->
    receive
        _Any ->
            clear_mailbox()
    after 0 ->
        ok
    end.

await_compile_result() ->
    receive
        {_Pid, News} ->
            News;
        {'DOWN', _Ref, _Type, _Object, normal} ->
            await_compile_result();
        {'DOWN', _Ref, Type, Object, Info} ->
            erlang:display({flierl_monitored_error, Type, Object, Info}),
            "(((0 \"unknown flierl error\")) nil)";
        Err -> 
            erlang:display({flierl_error, Err}),
            "(((0 \"unknown flierl error\")) nil)"
    after
        3000 ->
            "(((0 \"compile timed out\")) nil)"
    end.
    

compile(Path) ->
    Self = self(),
    {Pid, Ref} = spawn_monitor(fun () -> compile_and_report(Path, Self) end),
    erlang:display({i, Self, spawned, Pid, with, Ref}),
    Result = await_compile_result(),
    clear_mailbox(),
    io:format("~s", [Result]).
