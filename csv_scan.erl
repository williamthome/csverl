-module(csv_scan).

%% API
-export([
    file/1,
    file/2
]).

-define(NEW_LINE_BYTE_SIZE, byte_size(<<"\n">>)).
-define(SPLIT_COLUMNS_RE,
    begin
        Pattern = ",(?!(?=[^\"]*\"[^\"]*(?:\"[^\"]*\"[^\"]*)*$))",
        {ok, Re} = re:compile(Pattern),
        Re
    end
).

file(Filename) ->
    Options = maps:new(),
    file(Filename, Options).

file(Filename, Options0) ->
    Options = maps:merge(default_scan_options(), Options0),
    case do_file(Filename) of
        {ok,FileDescriptor} ->
            Context = #{
                headers => []
            },
            Table = do_table(Filename),
            case do_read_all_lines(FileDescriptor, Table, 1, Options, Context) of
                {eof, Context1} ->
                    {ok, do_table_data_to_map(Table, Options, Context1)};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

default_scan_options() -> #{first_row_index     => 1,
                            first_column_index  => 1,
                            first_row_is_header => false,
                            transform           => undefined}.

do_file(Filename) ->
    file:open(Filename, [raw, read, read_ahead, binary]).

do_table(Filename) ->
    Tablename = list_to_atom(Filename),
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

do_read_line(FileDescriptor, Table, RowIndex, #{first_row_index := FirstRow, first_row_is_header := FirstRowIsHeader} = Options, Context0) ->
    case file:read_line(FileDescriptor) of
        {ok, Row} ->
            Context =
                case RowIndex < FirstRow of
                    true -> Context0;
                    false ->
                        case RowIndex =:= FirstRow andalso FirstRowIsHeader of
                            true ->
                                Context0#{headers => do_columns(Row, Options)};
                            false ->
                                do_spawn_insert_row(Row, RowIndex, Table, Options),
                                Context0
                        end
                end,
            {ok, {Row, Context}};
        eof ->
            do_close(FileDescriptor),
            eof;
        {error, Reason} ->
            do_close(FileDescriptor),
            {error, Reason}
    end.

do_columns(Row0, #{first_column_index := FirstColumn}) ->
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
    Columns = [RowIndex | do_columns(Row, Options)],
    Data = list_to_tuple(Columns),
    ets:insert(Table, Data).

do_spawn_insert_row(Row, RowIndex, Table, Options) ->
    spawn(fun() -> do_insert_row(Row, RowIndex, Table, Options) end).

do_read_all_lines(FileDescriptor, Table, Index, Options, Context) ->
    case do_read_line(FileDescriptor, Table, Index, Options, Context) of
        {ok, {_Row, Context1}} ->
            do_read_all_lines(FileDescriptor, Table, Index + 1, Options, Context1);
        eof ->
            {eof, Context};
        {error, Reason} ->
            {error, Reason}
    end.

do_close(FileDescriptor) ->
    file:close(FileDescriptor).

do_table_data(Table) ->
    ets:tab2list(Table).

do_table_data_to_map(Table, Options, #{headers := Headers}) ->
    Data = do_table_data(Table),
    lists:map(
        fun(Columns) ->
            [RowIndex | Values] = tuple_to_list(Columns),

            Keys =
                case Headers of
                    [] ->
                        lists:seq(1, length(Values));
                    Headers ->
                        Headers
                end,

            Proplist = lists:zip(Keys, Values),
            Row = maps:from_list(Proplist),

            do_transform(Row, RowIndex, Options)
        end,
        Data
    ).

do_transform(Row, _Index, #{transform := undefined}) ->
    Row;
do_transform(Row, _Index, #{transform := Transform}) when is_function(Transform, 1) ->
    Transform(Row);
do_transform(Row, Index, #{transform := Transform}) when is_function(Transform, 2) ->
    Transform(Index, Row).
