%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Dairon Medina Caro <me@dairon.org>
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%% Created : 08. Nov 2017 7:39 PM
%%%-------------------------------------------------------------------
-module(vernemq_kinesis_worker).
-author("codeadict").

-behaviour(gen_server).


-export([ init/1
  , terminate/2
  , code_change/3
  , handle_call/3
  , handle_cast/2
  , handle_info/2
]).

-record(state, {
  stream_name,
  allowed_topics,
  timer = undefined,
  buffer = []
}).
-type state() :: #state{}.


init([]) ->
  process_flag(trap_exit, true),
  {ok, Key} = application:get_env(vernemq_kinesis, aws_key),
  {ok, SecretKey} = application:get_env(vernemq_kinesis, aws_secret_key),
  {ok, Region} = application:get_env(vernemq_kinesis, region),
  {ok, Topics} = application:get_env(vernemq_kinesis, allowed_topics, "#"),
  {ok, Stream} = application:get_env(vernemq_kinesis, stream),
  Hostname = "kinesis." + Region + ".amazonaws.com",
  ok = erlcloud_kinesis:configure(Key, SecretKey, Hostname),
  StreamBin = list_to_binary(Stream),
  {ok, TRef} = timer:send_after(1000, send),
  {ok, #state{stream_name = StreamBin, allowed_topics = Topics, timer = TRef}}.

handle_cast({put, Topic, Payload}, State = #state{buffer=Buffer, timer=Timer}) ->
  lager:debug("New data coming in"),
  timer:cancel(Timer),
  {noreply, State#state{buffer=Buffer, timer=Timer}};
handle_cast(flush, State = #state{buffer=Buffer}) ->
  lager:debug("Buffer flush requested"),
  {noreply, State#state{buffer=[], timer=undefined}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

-spec terminate(_, state()) -> ok.
terminate(_Reason, _State) ->
  ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

maybe_send_records(Data, Buffer) ->
  {ok, BatchSize} = application:get_env(vernemq_kinesis, batch_size),
  {ok, BatchTime} = application:get_env(vernemq_kinesis, batch_time),
  case lenght(Buffer) >= BatchSize of
    true ->
      lager:debug("Buffer full"),
      flush([Data|Buffer]),
      {[], undefined};
    false ->
      lager:debug("Buffering data"),
      {ok, Timer} = timer:apply_after(BatchTime, gen_server, cast, [self(), flush]),
      {[Data|Buffer], Timer}
  end.

flush(Buffer) ->
  case erlcloud_kinesis:put_records(<<"test">>, Buffer) of
    {ok, [{<<"FailedRecordCount">>, 0}, _]} ->
      lager:info("Put records successful"),
      ok;
    {error, {_, _, _, Json}} ->
      PropList = jsx:decode(Json),
      Msg = proplists:get_value(<<"message">>, PropList),
      lager:info("Put records error. Desc: ~p", [Msg])
  end.

is_allowed_topic([], []) ->
  true;
is_allowed_topic([H|T1], [H|T2]) ->
  is_allowed_topic(T1, T2);
is_allowed_topic([_H|T1], [<<"+">>|T2]) ->
  is_allowed_topic(T1, T2);

is_allowed_topic([_H1|_], [_H2|_]) ->
  false;
is_allowed_topic([], [_H|_T2]) ->
  false;
is_allowed_topic(_, _)
  -> false.

