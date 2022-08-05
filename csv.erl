-module(csv).

-export([
    scan_file/0,
    scan/1,
    scan/2
]).

scan_file() ->
    Filename = "simpsons.csv",
    Content = simpsons(),
    Options = #{first_row_index     => 1,
                first_column_index  => 1,
                first_row_is_header => true},
    case rwrite(Filename, Content) of
        {ok, Bin} -> scan(Bin, Options);
        {error, Reason} -> {error, Reason}
    end.

scan(Bin) ->
    Options = maps:new(),
    scan(Bin, Options).

scan(Bin, Options0) ->
    In = data,
    Cursor = {1, 1},
    Cache = {#{}, <<>>},
    Buffer = <<>>,
    Acc = [],
    Options = maps:merge(default_scan_options(), Options0),
    do_scan(Bin, In, Cursor, Cache, Buffer, Options, Acc).

default_scan_options() -> #{first_row_index     => 1,
                            first_column_index  => 1,
                            first_row_is_header => true}.

do_scan(Bin0, In, {Row, Col}, Cache, Buffer, #{first_row_index := FirstRow} = Options, Acc)
    when Row < FirstRow ->
      Bin =
        case binary:split(Bin0, <<"\n">>) of
            [_, X] -> X;
            [X] -> X
        end,
      do_scan(Bin, In, {Row + 1, Col}, Cache, Buffer, Options, Acc);

do_scan(Bin, In, {Row, Col}, Cache, Buffer, #{first_column_index := FirstCol} = Options, Acc)
    when Col < FirstCol ->
      do_scan(Bin, In, {Row, Col + 1}, Cache, Buffer, Options, Acc);

do_scan(<<",\"", Bin/binary>>, data, Cursor, Cache, Buffer, Options, Acc) ->
    put_column_and_do_scan(<<",\"">>, Bin, text, Cursor, Cache, Buffer, Options, Acc);

do_scan(<<"\",", Bin/binary>>, text, Cursor, Cache, Buffer, Options, Acc) ->
    put_column_and_do_scan(<<"\",">>, Bin, data, Cursor, Cache, Buffer, Options, Acc);

do_scan(<<"\"", Bin/binary>>, text, Cursor, Cache, Buffer, Options, Acc) ->
    concat_head_and_do_scan(<<"\"">>, Bin, text, Cursor, Cache, Buffer, Options, Acc);

do_scan(<<",", Bin/binary>>, data, Cursor, Cache, Buffer, Options, Acc) ->
    put_column_and_do_scan(<<",">>, Bin, data, Cursor, Cache, Buffer, Options, Acc);

do_scan(<<"\n", Bin/binary>>, _In, {Row, Col}, {RowAcc0, ColAcc}, Buffer0, Options, Acc0) ->
    RowAcc = maps:put(Col, ColAcc, RowAcc0),
    Cache = {#{}, <<>>},
    Cursor = {Row + 1, 1},
    Acc = [RowAcc | Acc0],
    Buffer = <<Buffer0/binary, "\n">>,
    do_scan(Bin, data, Cursor, Cache, Buffer, Options, Acc);

do_scan(<<H, Bin/binary>>, In, Cursor, Cache, Buffer, Options, Acc) ->
    concat_head_and_do_scan(H, Bin, In, Cursor, Cache, Buffer, Options, Acc);

do_scan(<<>>, _In, _Cursor, _Cache, _Buffer, _Options, Acc) ->
    lists:reverse(Acc).

put_column_and_do_scan(H, Bin, In, {Row, Col}, {RowAcc0, ColAcc}, Buffer0, Options, Acc) ->
    RowAcc = maps:put(Col, ColAcc, RowAcc0),
    Cache = {RowAcc, <<>>},
    Cursor = {Row, Col + 1},
    Buffer = <<Buffer0/binary, H/binary>>,
    do_scan(Bin, In, Cursor, Cache, Buffer, Options, Acc).

concat_head_and_do_scan(H, Bin, In, Cursor, {RowAcc, ColAcc0}, Buffer0, Options, Acc) ->
    ColAcc = <<ColAcc0/binary, H>>,
    Cache = {RowAcc, ColAcc},
    Buffer = <<Buffer0/binary, H>>,
    do_scan(Bin, In, Cursor, Cache, Buffer, Options, Acc).

rwrite(Filename, Content) ->
    case file:read_file(Filename) of
        {ok, Bin} -> {ok, Bin};
        {error, enoent} ->
            case file:write_file(Filename, Content) of
                ok ->
                    case file:read_file(Filename) of
                        {ok, Bin} -> {ok, Bin};
                        {error, Reason} -> {error, Reason}
                    end;
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} -> {error, Reason}
    end.

simpsons() ->
    <<"Name,Gentle,Phone Number,Email
Homer Simpson,\"Simpson, Homer\",5551234422,homer@springfield.com
Seymour Skinner,\"Skinner, Seymour\",1235663322,a@b.c
Bart Simpson,\"Simpson, Bart\",2675465026,bart@spring.field
Montgomery Burns,\"Burns, Montgomery\",2233459922,hi@bye.cya
Mayor Quimby,\"Quimby, Mayor\",2222222222,mayor@springfield.gov
Waylon Smithers,\"Smithers, Waylon\",3333333333,ok@hey.bye
Barney Gumble,\"Gumble, Barney\",111111111111,barney@gumble.gum
Marge Simpson,\"Simpson, Marge\",2627338461,marge@springfield.com
Edna Krabappel,\"Krabappel, Edna\",2656898220,a@b.c
Lisa Simpson,\"Simpson, Lisa\",2222222222,lisa@bix.com
Maggie Simpson,\"Simpson, Maggie\",2716017739,maggie@spring.field
Linel Hutz,\"Hutz, Linel\",2745577499,hire@now.me
Troy McClure,\"McClure, Troy\",2314928822,troy@acting.now
Rainer Wolfcastle,\"Wolfcastle, Rainer\",2221114455,rainer@acting.now
Krusty Clown,\"Clown, Krusty\",2321221188,krusty@acting.now">>.
