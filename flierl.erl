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

compile(Path) ->
    Sexp = 
        case compile:file(Path, [return_errors, strong_validation, return_warnings]) of
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
    io:format("~s", [lists:flatten(Sexp)]).
