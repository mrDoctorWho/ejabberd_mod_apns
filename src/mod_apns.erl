%% Ejabberd module for the Apple Push Notification Service
%% Created: 07/09/2015 by mrDoctorWho
%% License: MIT/X11

-module(mod_apns).

-author("mrDoctorWho").

-include("ejabberd.hrl").
-include("xmpp.hrl").
-include("mod_apns.hrl").
-include("logger.hrl").

-behaviour(gen_mod).

-record(apns_users, {user :: {binary(), binary()},
		     token :: binary(),
		     last_seen :: pos_integer()}).

-define(NS_APNS, <<"https://apple.com/push">>). %% I hope Apple doesn't mind.

-export([iq/1, message/1, mod_opt_type/1, start/2,
	 stop/1, depends/2]).

-define(Timeout, 10000).

-spec send_payload(binary(), iolist(), binary()) -> ok | {error, any()}.
% partially done by uwe-arzt.de
send_payload(Host, Payload, Token) ->
    Address = gen_mod:get_module_opt(Host, ?MODULE, address),
    Port = gen_mod:get_module_opt(Host, ?MODULE, port),
    Cert = gen_mod:get_module_opt(Host, ?MODULE, certfile),
    Keyfile = gen_mod:get_module_opt(Host, ?MODULE, keyfile),
    Password = gen_mod:get_module_opt(Host, ?MODULE, password),
    ?DEBUG("Trying to send payload with "
	   "these parameters: Address: ~s Port: "
	   "~s Cert: ~s Keyfile: ~s Password ~s",
	   [Address, Port, Cert, Keyfile, Password]),
    case Password of
	undefined ->
	    Options = [{certfile, Cert},
		       {keyfile, Keyfile},
		       {mode, binary}]; %% {verify, verify_none}
	_ ->
	    Options = [{certfile, Cert}, {keyfile, Keyfile},
		       {password, Password}, {mode, binary}]
    end,
    case ssl:connect(Address, Port, Options, ?Timeout) of
	{ok, Socket} ->
	    PayloadBin = list_to_binary(Payload),
	    PayloadLength = size(PayloadBin),
	    TokenNum = erlang:binary_to_integer(Token, 16),
	    TokenBin = <<TokenNum:32/integer-unit:8>>,
	    Packet = <<0:8, 32:16/big, TokenBin/binary,
		       PayloadLength:16/big, PayloadBin/binary>>,
	    ssl:send(Socket, Packet),
	    ssl:close(Socket),
	    ?DEBUG("Successfully sent payload "
		   "to the APNS server", []);
	{error, Reason} = Err ->
	    ?ERROR_MSG("Unable to connect to the APNS "
		       "server: ~s", [ssl:format_error(Reason)]),
	    Err
    end.

create_json(List1, List2) ->
    lists:append(["{\"aps\":{", create_keyvalue(List1),
		  "}, ", create_keyvalue(List2), "}"]).

create_keyvalue([Head]) -> create_pair(Head);
create_keyvalue([Head | Tail]) ->
    lists:append([create_pair(Head), ",", create_keyvalue(Tail)]).

create_pair({Key, Value}) ->
    lists:append([add_quotes(atom_to_list(Key)), ":", add_quotes(Value)]).

add_quotes(String) ->
    lists:append(["\"", String, "\""]).

-spec message({any(), message()}) -> {any(), message()}.
message({_, #message{type = T}} = Acc) when T == normal; T == error ->
    Acc;
message({_, #message{from = From, to = To} = Packet} = Acc) ->
    ?DEBUG("Offline message", []),
    JFrom = jid:encode(jid:remove_resource(From)),
    JTo = jid:encode(jid:remove_resource(To)),
    ToUser = To#jid.luser,
    ToServer = To#jid.lserver,
    Body = xmpp:get_text(Packet#message.body),
    {Subscription, _Groups} =
	ejabberd_hooks:run_fold(roster_get_jid_info, ToServer,
				{none, []}, [ToUser, ToServer, From]),
    case Subscription of
	both ->
	    case Body of
		<<>> -> ok;
		_ ->
		    Result = mnesia:dirty_read(apns_users, {ToUser, ToServer}),
		    case Result of
			[] ->
			    ?DEBUG("No such record found for ~s", [JTo]);
			[#apns_users{token = Token}] ->
			    Sound = "default",
			    %% TODO: Move binary_to_list to create_pair?
			    %% Badges?
			    Msg = [{alert, binary_to_list(Body)},
				   {sound, Sound}],
			    Args = [{source, binary_to_list(JFrom)},
				    {destination, binary_to_list(JTo)}],
			    JSON = create_json(Msg, Args),
			    send_payload(ToServer, JSON, Token)
		    end
	    end;
	_ -> ok
    end,
    Acc.

-spec iq(iq()) -> iq().
iq(#iq{from = #jid{luser = LUser, lserver = LServer},
       sub_els = [#apns_register{token = Token}]} = IQ) ->
    {MegaSecs, Secs, _MicroSecs} = p1_time_compat:timestamp(),
    TimeStamp = MegaSecs * 1000000 + Secs,
    F = fun () ->
		mnesia:write(#apns_users{user = {LUser, LServer},
					 token = Token, last_seen = TimeStamp})
	end,
    case mnesia:dirty_read(apns_users, {LUser, LServer}) of
	[] ->
	    mnesia:transaction(F),
	    ?DEBUG("New user registered ~s@~s", [LUser, LServer]);
	%% Record exists, the key is equal to the one we know
	[#apns_users{user = {LUser, LServer}, token = Token}] ->
	    mnesia:transaction(F),
	    ?DEBUG("Updating last_seen for user ~s@~s",
		   [LUser, LServer]);
	%% Record for this key has been found, but with another token
	[#apns_users{user = {LUser, LServer}, token = _}] ->
	    mnesia:transaction(F),
	    ?DEBUG("Updating token for user ~s@~s",
		   [LUser, LServer])
    end,
    %% We don't need the result, but the handler has to send something
    xmpp:make_iq_result(IQ);
iq(#iq{lang = Lang} = IQ) ->
    Txt = <<"No module is handling this query">>,
    xmpp:make_error(IQ, xmpp:err_service_unavailable(Txt, Lang)).

start(Host, _) ->
    xmpp:register_codec(mod_apns_codec),
    mnesia:create_table(apns_users,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, apns_users)}]),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_APNS, ?MODULE, iq, no_queue),
    ejabberd_hooks:add(offline_message_hook, Host, ?MODULE,
		       message, 49).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_APNS),
    ejabberd_hooks:delete(offline_message_hook, Host, ?MODULE,
			  message, 49).

mod_opt_type(address) ->
    fun binary_to_list/1;
mod_opt_type(port) ->
    fun (I) when is_integer(I), I>0, I<65536 -> I end;
mod_opt_type(certfile) ->
    fun misc:try_read_file/1;
mod_opt_type(keyfile) ->
    fun misc:try_read_file/1;
mod_opt_type(password) ->
    fun iolist_to_binary/1;
mod_opt_type(_) ->
    [address, port, certfile, keyfile, password].

depends(_, _) ->
    [].
