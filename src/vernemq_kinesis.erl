%%%-------------------------------------------------------------------
%% @doc vernemq_kinesis public API
%% @end
%%%-------------------------------------------------------------------

-module(vernemq_kinesis).
-include("vernemq_dev.hrl").

-behaviour(on_publish_hook).

%% Plugin callbacks
-export([on_publish/6]).

%%====================================================================
%% API
%%====================================================================

on_publish(UserName, {_MountPoint, _ClientId} = SubscriberId, QoS, Topic, Payload, IsRetain) ->
    ok.
