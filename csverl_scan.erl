%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2022, williamthome
%%% @doc CSV scan module.
%%%
%%% @author William Fank Thomé [https://github.com/williamthome/]
%%% @since 2022
%%% @end
%%%-----------------------------------------------------------------------------
-module(csverl_scan).

%% API
-export([
    file/1,
    file/2
]).

%% Types that can be used from other modules -- alphabetically ordered.
-export_type([filename/0, options/0, result/0]).

%% Data types
-type filename()  :: file:name_all().
-type options()   :: #{first_row_index     => pos_integer(),
                       first_column_index  => pos_integer(),
                       first_row_is_header => boolean(),
                       transform           => transform(),
                       headers             => [binary()]}.
-type result()    :: {ok, [map()]}
                     | {error, file:posix() | badarg | terminated | system_limit}.
-type transform() :: fun((map()) -> map())
                     | fun((pos_integer(), map()) -> map())
                     | undefined.

% Defines
-define(NEW_LINE_BYTE_SIZE, byte_size(<<"\n">>)).
-define(SPLIT_COLUMNS_RE,
    begin
        Pattern = ",(?!(?=[^\"]*\"[^\"]*(?:\"[^\"]*\"[^\"]*)*$))",
        {ok, Re} = re:compile(Pattern),
        Re
    end
).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Scans a comma-separated values (CSV) file.
%% @end
%%------------------------------------------------------------------------------
-spec file(Filename) -> ScanResult when
    Filename   :: filename(),
    ScanResult :: result().

file(Filename) ->
    Options = maps:new(),
    file(Filename, Options).

%%------------------------------------------------------------------------------
%% @doc Scans a comma-separated values (CSV) file passing options.
%% @end
%%------------------------------------------------------------------------------
-spec file(Filename, Options) -> ScanResult when
    Filename   :: filename(),
    Options    :: options(),
    ScanResult :: result().

file(Filename, Options0) ->
    Options = maps:merge(do_default_options(), Options0),
    case file:open(Filename, [raw, read, read_ahead, binary]) of
        {ok,FileDescriptor} ->
            Context = do_context(Options),
            Table = do_new_table(Filename),
            case do_read_all_lines(FileDescriptor, Table, 1, Options, Context) of
                {eof, Context1} ->
                    case do_new_table_data_to_map(Table, Options, Context1) of
                        {ok, Data} ->
                            {ok, Data};
                        {error, Reason} ->
                            ets:delete(Table),
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

do_default_options() -> #{first_row_index     => 1,
                          first_column_index  => 1,
                          first_row_is_header => false,
                          transform           => undefined,
                          headers             => []}.

do_context(Options) -> #{headers => maps:get(headers, Options),
                         errors  => []}.

do_new_table(Filename) ->
    Tablename = erlang:list_to_atom(Filename),
    do_delete_table_if_exists(Tablename),
    Options = [ordered_set, {write_concurrency, true}, named_table, public],
    ets:new(Tablename, Options).

do_delete_table_if_exists(Table) ->
    case ets:whereis(Table) of
        undefined ->
            false;
        Pid ->
            ets:delete(Pid),
            true
    end.

do_read_all_lines(FileDescriptor, Table, Index, Options, Context) ->
    case do_read_line(FileDescriptor, Table, Index, Options, Context) of
        {ok, {_Row, Context1}} ->
            do_read_all_lines(FileDescriptor, Table, Index + 1, Options, Context1);
        eof ->
            {eof, Context};
        {error, Reason} ->
            {error, Reason}
    end.

do_read_line(FileDescriptor, Table, RowIndex,
             #{first_row_index := FirstRow,
               first_row_is_header := FirstRowIsHeader} = Options,
             Context0) ->
    case file:read_line(FileDescriptor) of
        {ok, Row} ->
            Context =
                case RowIndex < FirstRow of
                    true -> Context0;
                    false ->
                        case RowIndex =:= FirstRow andalso FirstRowIsHeader of
                            true ->
                                Context0#{headers => do_split_row(Row, Options)};
                            false ->
                                erlang:spawn(fun() ->
                                    do_insert_row(Row, RowIndex, Table, Options)
                                end),
                                Context0
                        end
                end,
            {ok, {Row, Context}};
        eof ->
            file:close(FileDescriptor),
            eof;
        {error, Reason} ->
            file:close(FileDescriptor),
            {error, Reason}
    end.

do_split_row(Row0, #{first_column_index := FirstColumn}) ->
    Row =
        case byte_size(Row0) of
            0 -> <<>>;
            Bytes ->
                Size = Bytes - ?NEW_LINE_BYTE_SIZE,
                <<Row1:Size/binary, _>> = Row0,
                Row1
        end,
    List0 = re:split(Row, ?SPLIT_COLUMNS_RE),
    case FirstColumn =:= 1 of
        true -> List0;
        false -> lists:sublist(List0, FirstColumn, length(List0))
    end.

do_insert_row(Row, RowIndex, Table, Options) ->
    Columns = [RowIndex | do_split_row(Row, Options)],
    Data = erlang:list_to_tuple(Columns),
    ets:insert(Table, Data).

do_new_table_data_to_map(Table, Options, Context0) ->
    Data = ets:tab2list(Table),
    {Rows, Context} = lists:foldl(
        fun(Columns, {Acc, #{headers := Headers, errors := Errors0} = Context1}) ->
            [RowIndex | Values] = tuple_to_list(Columns),
            Keys = do_row_keys(Headers, Values),
            Proplist = lists:zip(Keys, Values),
            Row = maps:from_list(Proplist),
            case do_transform(Row, RowIndex, Options) of
                {ok, Transformed} ->
                    {[Transformed | Acc], Context1};
                {error, Errors1} ->
                    Errors2 =
                        lists:map(
                            fun({Col, Reason}) ->
                                [{row, {RowIndex, Row}}, {col, Col}, {reason, Reason}]
                            end,
                            Errors1
                        ),
                    Errors = lists:merge(Errors0, Errors2),
                    {Acc, Context1#{errors => Errors}}
            end
        end,
        {[], Context0},
        Data
    ),
    case Context of
        #{errors := []} ->
            {ok, Rows};
        #{errors := Errors} ->
            {error, Errors}
    end.

do_row_keys([], Values) -> lists:seq(1, length(Values));
do_row_keys(Headers, _Values) -> Headers.

do_transform(Row, _Index, #{transform := undefined}) ->
    {ok, Row};
do_transform(Row, _Index, #{transform := Transf}) when is_function(Transf, 1) ->
    Transf(Row).
