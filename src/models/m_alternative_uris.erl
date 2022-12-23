%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2015-2022 Marc Worrell
%% @doc Model for alternative uri handling
%% @enddoc

%% Copyright 2015-2022 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(m_alternative_uris).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    list_dispatch_host/3,
    get_dispatch/2,
    insert/3,
    install/1
    ]).

% For testing
-export([
    alt_uris/1
    ]).

-define(PATHLEN, 190).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Lookup the rsc id for the given host and path
list_dispatch_host(Host, Path, Context) ->
    z_db:q("select path, rsc_id, is_permanent
            from alternative_uris
            where host = lower($1)
              and (path = $2 or path = '')",
           [Host, z_string:truncatechars(remove_slash(Path), ?PATHLEN, <<>>) ],
           Context).

get_dispatch(Path, Context) ->
    z_db:q_row("select rsc_id, is_permanent
                from alternative_uris
                where host = ''
                  and path = $1",
               [ z_string:truncatechars(remove_slash(Path), ?PATHLEN, <<>>) ],
               Context).

insert(RscId, AltUris, Context) ->
    Current = z_db:q("
                    select host,path 
                    from alternative_uris
                    where rsc_id = $1",
                    [RscId],
                    Context),
    HPs = alt_uris(z_html:unescape(AltUris)),
    New = HPs -- Current,
    Del = Current -- HPs,
    case {New,Del} of
        {[], []} ->
            ok;
        _ ->
            z_db:transaction(
                    fun(Ctx) ->
                        lists:foreach(
                                fun({H,P}) ->
                                    z_db:q("
                                        delete from alternative_uris
                                        where rsc_id = $1
                                          and host = $2
                                          and path = $3",
                                        [RscId, H, P],
                                        Ctx)
                                end,
                                Del),
                        lists:foreach(
                                fun({H,P}) ->
                                    z_db:q("
                                        insert into alternative_uris
                                            (rsc_id, host, path)
                                        values
                                            ($1, $2, $3)",
                                        [RscId, H, P],
                                        Ctx)
                                end,
                                New)
                    end,
                    Context)
    end.

alt_uris(undefined) ->
    [];
alt_uris(<<>>) ->
    [];
alt_uris(Text) ->
    Lines = binary:split(z_string:trim(Text), <<10>>, [global, trim_all]),
    HPs = [ host_path( z_string:trim(Line) ) || Line <- Lines ],
    HPs1 = [ HP || HP <- HPs, HP =/= {<<>>,<<>>} ],
    HPs2 = [ {H, z_string:truncatechars(P, ?PATHLEN, <<>>)} || {H, P} <- HPs1 ],
    lists:usort(HPs2).

host_path(<<>>) ->
    {<<>>,<<>>};
host_path(<<$:, Rest/binary>>) ->
    host_path(Rest);
host_path(<<$/, $/, _/binary>> = HostPath) ->
    host_path_1(HostPath);
host_path(<<"http:", _/binary>> = HostPath) ->
    host_path_1(HostPath);
host_path(<<"https:", _/binary>> = HostPath) ->
    host_path_1(HostPath);
host_path(Path) ->
    {<<>>, remove_slash(Path)}.

host_path_1(HostPath) ->
    case uri_string:parse(HostPath) of
        #{ host := Host, path := Path } ->
            {z_string:to_lower(Host), remove_slash(Path)};
        _ ->
            ?LOG_WARNING(#{
                text => <<"Illegal host/path for alternative uri">>,
                result => error,
                reason => uri,
                uri => HostPath
            }),
            {<<>>, HostPath}
    end.

remove_slash(<<$/,Path/binary>>) -> Path;
remove_slash(Path) when is_binary(Path) -> Path.

install(Context) ->
    case z_db:table_exists(alternative_uris, Context) of
        false ->
            [] = z_db:q("
                    create table alternative_uris (
                        id bigserial not null,
                        rsc_id integer not null,
                        host character varying(80) not null,
                        path character varying(200) not null,
                        is_permanent bool not null default false,
                        created timestamp with time zone not null default current_timestamp,

                        primary key (id),
                        foreign key (rsc_id) references rsc(id)
                        on update cascade on delete cascade
                    )
                ", Context),
            [] = z_db:q("
                    create index fki_alternative_uris_rsc_id on alternative_uris (rsc_id)
                ", Context),
            [] = z_db:q("
                    create index alternative_uris_host_path_key on alternative_uris (host,path)
                ", Context),
            ok;
        true ->
            ok
    end.
