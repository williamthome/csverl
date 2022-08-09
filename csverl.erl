%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2022, williamthome
%%% @doc CSV module.
%%%
%%% @author William Fank Thomé [https://github.com/williamthome/]
%%% @since 2022
%%% @end
%%%-----------------------------------------------------------------------------
-module(csverl).

%% API
-export([
    scan_file/1,
    scan_file/2
]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Scans a comma-separated values (CSV) file.
%% @end
%%------------------------------------------------------------------------------
-spec scan_file(Filename) -> ScanResult when
    Filename   :: csverl_scan:filename(),
    ScanResult :: csverl_scan:result().

scan_file(Filename) ->
    csverl_scan:file(Filename).

%%------------------------------------------------------------------------------
%% @doc Scans a comma-separated values (CSV) file passing options.
%% @end
%%------------------------------------------------------------------------------
-spec scan_file(Filename, Options) -> ScanResult when
    Filename   :: csverl_scan:filename(),
    Options    :: csverl_scan:options(),
    ScanResult :: csverl_scan:result().

scan_file(Filename, Options) ->
    csverl_scan:file(Filename, Options).
