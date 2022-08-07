%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2022, williamthome
%%% @doc CSV scan module.
%%%
%%% @author William Fank Thomé [https://github.com/williamthome/]
%%% @since 2022
%%% @end
%%%-----------------------------------------------------------------------------
-module(csv_scan).

%% API
-export([
    file/1,
    file/2,
    bin/1,
    bin/2
]).

%% Types that can be used from other modules -- alphabetically ordered.
-export_type([filename/0, options/0, result/0]).

%% Data types
-type column()    :: binary() | integer().
-type filename()  :: file:name_all().
-type options()   :: #{first_row_index     => pos_integer(),
                       first_column_index  => pos_integer(),
                       first_row_is_header => boolean(),
                       transform => transform()}.
-type result()    :: {ok, [#{column() => term()}]}
                     | {error, file:posix() | badarg | terminated | system_limit}.
-type transform() :: #{column() => fun((binary()) -> term())}.

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

file(Filename, Options) ->
    case file:read_file(Filename) of
        {ok, Bin} -> bin(Bin, Options);
        {error, Reason} -> {error, Reason}
    end.

%%------------------------------------------------------------------------------
%% @doc Scans a comma-separated values (CSV) binary.
%% @end
%%------------------------------------------------------------------------------
-spec bin(Bin) -> ScanResult when
    Bin        :: binary(),
    ScanResult :: result().

bin(Bin) ->
    Options = maps:new(),
    bin(Bin, Options).

%%------------------------------------------------------------------------------
%% @doc Scans a comma-separated values (CSV) binary passing options.
%% @end
%%------------------------------------------------------------------------------
-spec bin(Bin, Options) -> ScanResult when
    Bin        :: binary(),
    Options    :: options(),
    ScanResult :: result().

bin(Bin, Options0) ->
    In = data,
    Cursor = {1, 1},
    Cache = {#{}, <<>>},
    Buffer = <<>>,
    Headers = [],
    Options = maps:merge(default_scan_options(), Options0),
    Acc = [],
    do_scan(Bin, In, Cursor, Cache, Buffer, Headers, Options, Acc).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

default_scan_options() -> #{first_row_index     => 1,
                            first_column_index  => 1,
                            first_row_is_header => false,
                            transform           => #{}}.

do_scan(Bin0, In, {Row, Col}, Cache, Buffer, Headers,
        #{first_row_index := FirstRow} = Options, Acc)
        when FirstRow > 1, Row < FirstRow ->
    Bin = case binary:split(Bin0, <<"\n">>) of
              [_, X] -> X;
              [X] -> X
          end,
    do_scan(Bin, In, {Row + 1, Col}, Cache, Buffer, Headers, Options, Acc);

do_scan(Bin0, In, {Row, Col}, Cache, Buffer, Headers,
        #{first_column_index := FirstCol} = Options, Acc)
        when Col < FirstCol ->
    RePattern = ",(?!(?=[^\"]*\"[^\"]*(?:\"[^\"]*\"[^\"]*)*$))",
    ReOptions = [{parts, 2}],
    Bin = case re:split(Bin0, RePattern, ReOptions) of
              [_, X] -> X;
              [X] -> X
          end,
    do_scan(Bin, In, {Row, Col + 1}, Cache, Buffer, Headers, Options, Acc);

do_scan(<<",\"", Bin/binary>>, data, Cursor, Cache, Buffer, Headers, Options, Acc) ->
    put_column_and_do_scan(<<",\"">>, Bin, text, Cursor, Cache, Buffer, Headers, Options, Acc);

do_scan(<<"\",", Bin/binary>>, text, Cursor, Cache, Buffer, Headers, Options, Acc) ->
    put_column_and_do_scan(<<"\",">>, Bin, data, Cursor, Cache, Buffer, Headers, Options, Acc);

do_scan(<<"\"", Bin/binary>>, text, Cursor, Cache, Buffer, Headers, Options, Acc) ->
    concat_head_and_do_scan(<<"\"">>, Bin, text, Cursor, Cache, Buffer, Headers, Options, Acc);

do_scan(<<",", Bin/binary>>, data, Cursor, Cache, Buffer, Headers, Options, Acc) ->
    put_column_and_do_scan(<<",">>, Bin, data, Cursor, Cache, Buffer, Headers, Options, Acc);

do_scan(<<"\n", Bin/binary>>, _In, {Row, _Col} = Cursor0, Cache0, Buffer0,
        Headers0, #{first_row_index := FirstRow} = Options, Acc0) ->
    Headers1 = headers(Cursor0, Cache0, Headers0, Options),
    Headers = case Row =:= FirstRow of
                  true -> lists:reverse(Headers1);
                  false -> Headers1
              end,
    ColName = cursor_colname(Cursor0, Headers, Options),
    case maybe_put_column(Cursor0, Cache0, ColName, Options) of
        {ok, RowAcc} ->
            Cache = {#{}, <<>>},
            Cursor = {Row + 1, 1},
            Acc = case map_size(RowAcc) =:= 0 of
                    true -> Acc0;
                    false -> [RowAcc | Acc0]
                end,
            Buffer = <<Buffer0/binary, "\n">>,
            do_scan(Bin, data, Cursor, Cache, Buffer, Headers, Options, Acc);
        {error, Reason} ->
            {error, Reason}
    end;

do_scan(<<H, Bin/binary>>, In, Cursor, Cache, Buffer, Headers, Options, Acc) ->
    concat_head_and_do_scan(H, Bin, In, Cursor, Cache, Buffer, Headers, Options, Acc);

do_scan(<<>>, _In, _Cursor, _Cache, _Buffer, _Headers, _Options, Acc) ->
    ScanResult = lists:reverse(Acc),
    {ok, ScanResult}.

put_column_and_do_scan(H, Bin, In, {FirstRow, _Col} = Cursor, Cache, Buffer,
                       Headers0, #{first_row_index := FirstRow} = Options, Acc) ->
    Headers = headers(Cursor, Cache, Headers0, Options),
    ColName = colname(Cursor, Cache, Options),
    do_put_column_and_do_scan(H, Bin, In, Cursor, Cache, Buffer,
                              ColName, Headers, Options, Acc);
put_column_and_do_scan(H, Bin, In, Cursor, Cache, Buffer, Headers, Options, Acc) ->
    ColName = cursor_colname(Cursor, Headers, Options),
    do_put_column_and_do_scan(H, Bin, In, Cursor, Cache, Buffer,
                              ColName, Headers, Options, Acc).

headers({FirstRow, _Col} = Cursor, Cache, Headers,
        #{first_row_index := FirstRow} = Options) ->
    ColName = colname(Cursor, Cache, Options),
    [ColName | Headers];
headers(_Cursor, _Cache, Headers, _Options) ->
    Headers.

colname({_Row, Col}, {_RowAcc, ColAcc}, Options) ->
    case maps:get(first_row_is_header, Options) of
        true -> ColAcc;
        false -> Col
    end.

cursor_colname({_Row, Col}, Headers, #{first_column_index := FirstCol}) ->
    ColIndex = Col - FirstCol + 1,
    do_cursor_colname(ColIndex, Headers).

do_cursor_colname(ColIndex, Headers) when ColIndex > length(Headers) ->
    ColIndex;
do_cursor_colname(ColIndex, Headers) ->
    lists:nth(ColIndex, Headers).

do_put_column_and_do_scan(H, Bin, In, {Row, Col} = Cursor0, Cache0, Buffer0,
                          ColName, Headers, Options, Acc) ->
    case maybe_put_column(Cursor0, Cache0, ColName, Options) of
        {ok, RowAcc} ->
            Cache = {RowAcc, <<>>},
            Cursor = {Row, Col + 1},
            Buffer = <<Buffer0/binary, H/binary>>,
            do_scan(Bin, In, Cursor, Cache, Buffer, Headers, Options, Acc);
        {error, Reason} ->
            {error, Reason}
    end.

maybe_put_column({FirstRow, _Col}, _Cache, _ColName,
                 #{first_row_index := FirstRow, first_row_is_header := true}) ->
    {ok, maps:new()};
maybe_put_column(Cursor, {RowAcc, _ColAcc0} = Cache, ColName, Options) ->
    case transform(Cursor, Cache, ColName, Options) of
        {ok, ColAcc} -> {ok, maps:put(ColName, ColAcc, RowAcc)};
        {error, Reason} -> {error, Reason}
    end.

transform({Row, Col}, {RowAcc, ColAcc}, ColName,
          #{transform := Transform} = Options) ->
    case maps:get(ColName, Transform, undefined) of
        undefined -> {ok, ColAcc};
        ColTransform ->
            try ColTransform(ColAcc) of
                Transformed -> {ok, Transformed}
            catch
                Exception:Reason ->
                    Info = #{exception => Exception,
                             reason    => Reason,
                             row       => #{index => Row, value => RowAcc},
                             column    => #{index => Col, value => ColAcc},
                             options   => Options},
                    {error, {transform, Info}}
            end
    end.

concat_head_and_do_scan(H, Bin, In, Cursor, {RowAcc, ColAcc0}, Buffer0,
                        Headers, Options, Acc) ->
    ColAcc = concat(ColAcc0, H),
    Cache = {RowAcc, ColAcc},
    Buffer = concat(Buffer0, H),
    do_scan(Bin, In, Cursor, Cache, Buffer, Headers, Options, Acc).

concat(B1, B2) when is_integer(B2) ->
    <<B1/binary, B2>>;
concat(B1, B2) when is_binary(B2) ->
    <<B1/binary, B2/binary>>.
