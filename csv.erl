-module(csv).

-export([ scan_file/0 ]).

scan_file() ->
    Filename = "simpsons.csv",
    Content = simpsons(),
    case rwrite(Filename, Content) of
        {ok, Bin} -> scan_bin(Bin);
        {error, Reason} -> {error, Reason}
    end.

scan_bin(Bin) ->
    In = data,
    Cursor = {1, 1},
    Cache = {#{}, <<>>},
    Buffer = <<>>,
    Acc = [],
    do_scan_bin(Bin, In, Cursor, Cache, Buffer, Acc).

do_scan_bin(<<",\"", Bin/binary>>, data, Cursor, Cache, Buffer, Acc) ->
    do_scan_bin_column(<<",\"">>, Bin, text, Cursor, Cache, Buffer, Acc);

do_scan_bin(<<"\",", Bin/binary>>, text, Cursor, Cache, Buffer, Acc) ->
    do_scan_bin_column(<<"\",">>, Bin, data, Cursor, Cache, Buffer, Acc);

do_scan_bin(<<"\"", Bin/binary>>, text, Cursor, Cache, Buffer, Acc) ->
    do_scan_bin_append(<<"\"">>, Bin, text, Cursor, Cache, Buffer, Acc);

do_scan_bin(<<",", Bin/binary>>, data, Cursor, Cache, Buffer, Acc) ->
    do_scan_bin_column(<<",">>, Bin, data, Cursor, Cache, Buffer, Acc);

do_scan_bin(<<"\n", Bin/binary>>, _In, {Row, Col}, {RowAcc0, ColAcc}, Buffer0, Acc0) ->
    RowAcc = maps:put(Col, ColAcc, RowAcc0),
    Cache = {#{}, <<>>},
    Cursor = {Row + 1, 1},
    Acc = [RowAcc | Acc0],
    Buffer = <<Buffer0/binary, "\n">>,
    do_scan_bin(Bin, data, Cursor, Cache, Buffer, Acc);

do_scan_bin(<<H, Bin/binary>>, In, Cursor, Cache, Buffer, Acc) ->
    do_scan_bin_append(H, Bin, In, Cursor, Cache, Buffer, Acc);

do_scan_bin(<<>>, _In, _Cursor, _Cache, _Buffer, Acc) ->
    lists:reverse(Acc).

do_scan_bin_column(H, Bin, In, {Row, Col}, {RowAcc0, ColAcc}, Buffer0, Acc) ->
    RowAcc = maps:put(Col, ColAcc, RowAcc0),
    Cache = {RowAcc, <<>>},
    Cursor = {Row, Col + 1},
    Buffer = <<Buffer0/binary, H/binary>>,
    do_scan_bin(Bin, In, Cursor, Cache, Buffer, Acc).

do_scan_bin_append(H, Bin, In, Cursor, {RowAcc, ColAcc0}, Buffer0, Acc) ->
    ColAcc = <<ColAcc0/binary, H>>,
    Cache = {RowAcc, ColAcc},
    Buffer = <<Buffer0/binary, H>>,
    do_scan_bin(Bin, In, Cursor, Cache, Buffer, Acc).

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
