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
                "((0 \"unknown error\") nil)";
            {error, [{_File0, Errors}], [{_File1, Warnings}]} -> 
                io_lib:format("(~s ~s)", [sexp([process_error_info(E) || E <- Errors]),
                                          sexp([process_error_info(W) || W <- Warnings])]);
            {error, [{_File0, Errors}], []} -> 
                io_lib:format("(~s nil)", [sexp([process_error_info(E) || E <- Errors])])
        end,
    lists:flatten(Sexp).


compile_and_report(Path, ReportTo) ->
    Result = 
        try
            Report = compile:file(Path, [strong_validation, return]),
            transform_for_emacs(Report)
        catch
            Err ->
                io_lib:format("((0 \"Flierl Compile Error: ~p\") nil)", 
                              [Err])
        end,
    ReportTo ! {self(), Result}.

compile(Path) ->
    Self = self(),
    Pid = spawn(fun () -> compile_and_report(Path, Self) end),
    Result = receive
                 {Pid, News} ->
                     News;
                 _ -> 
                     "((0 \"unknown flierl error\") nil)"
             after
                 3000 ->
                     "((0 \"compile timed out\") nil)"
             end,
    io:format("~s", [Result]).
