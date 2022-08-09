%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2022, williamthome
%%% @doc CSV module.
%%%
%%% @author William Fank Thomé [https://github.com/williamthome/]
%%% @since 2022
%%% @end
%%%-----------------------------------------------------------------------------
-module(csv).

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
    Filename   :: csv_scan:filename(),
    ScanResult :: csv_scan:result().

scan_file(Filename) ->
    csv_scan:file(Filename).

%%------------------------------------------------------------------------------
%% @doc Scans a comma-separated values (CSV) file passing options.
%% @end
%%------------------------------------------------------------------------------
-spec scan_file(Filename, Options) -> ScanResult when
    Filename   :: csv_scan:filename(),
    Options    :: csv_scan:options(),
    ScanResult :: csv_scan:result().

scan_file(Filename, Options) ->
    csv_scan:file(Filename, Options).
