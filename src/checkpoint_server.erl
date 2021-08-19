%%%-------------------------------------------------------------------
%%% @author Will Vining <wfvining@velikaya>
%%% @copyright (C) 2021, Will Vining
%%% @doc
%%% Saves checkpoints of the state of every particle at every iteration.
%%% @end
%%% Created :  5 Jul 2021 by Will Vining <wfvining@velikaya>
%%%-------------------------------------------------------------------
-module(checkpoint_server).

-behaviour(gen_server).

%% API
-export([start_link/2, get_checkpoint/1, save_checkpoint/5]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-type particle_id() :: term().

-type checkpoint_state() :: {particle:position(),
                             particle:value(),
                             particle:velocity()}.

-type checkpoint_data() :: #{particle_id() => checkpoint_state()}.

-record(state, {file :: pid(),
                particles :: [particle_id()],
                next_checkpoint = 1 :: pos_integer(),
                pending_checkpoints = #{} :: #{pos_integer()
                                               => checkpoint_data()}}).

-type state() :: #state{}.


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link(CheckpointFile :: string(),
                 Particles :: [particle_id()]) -> {ok, Pid :: pid()} |
          {error, Error :: {already_started, pid()}} |
          {error, Error :: term()} |
          ignore.
start_link(CheckpointFile, Particles) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE,
                          {CheckpointFile, Particles}, []).

%% @doc Get the most recent checkpoint stat for `Particle'.
-spec get_checkpoint(Particle :: any()) ->
          {pos_integer(), particle:state()} |
          none.
get_checkpoint(Particle) ->
    gen_server:call(?SERVER, {get_checkpoint, Particle}).

%% @doc Save the state of `Particle' at `Iteration'.
-spec save_checkpoint(Particle :: any(),
                      Iteration :: pos_integer(),
                      Position :: particle:position(),
                      Value :: particle:value(),
                      Velocity :: particle:velocity()) -> ok.
save_checkpoint(Particle, Iteration, Position, Value, Velocity) ->
    gen_server:cast(
      ?SERVER,
      {save_checkpoint, Particle, Iteration, {Position, Value, Velocity}}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
          {ok, State :: term(), Timeout :: timeout()} |
          {ok, State :: term(), hibernate} |
          {stop, Reason :: term()} |
          ignore.
init({CheckpointFile, Particles}) ->
    {ok, File} = file:open(CheckpointFile, [read, write, append, binary]),
    CheckpointData = load_checkpoint(File),
    LastCheckpoint = lists:max(maps:keys(CheckpointData)),
    {ok, #state{file = File,
                next_checkpoint = LastCheckpoint + 1,
                particles = Particles,
                pending_checkpoints = CheckpointData}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
          {reply, Reply :: term(), NewState :: term()} |
          {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
          {reply, Reply :: term(), NewState :: term(), hibernate} |
          {noreply, NewState :: term()} |
          {noreply, NewState :: term(), Timeout :: timeout()} |
          {noreply, NewState :: term(), hibernate} |
          {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
          {stop, Reason :: term(), NewState :: term()}.
handle_call({get_checkpoint, ParticleId}, _From,
            State = #state{pending_checkpoints = PendingCheckpoints}) ->
    Reply = get_latest_checkpoint(ParticleId, PendingCheckpoints),
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
          {noreply, NewState :: term()} |
          {noreply, NewState :: term(), Timeout :: timeout()} |
          {noreply, NewState :: term(), hibernate} |
          {stop, Reason :: term(), NewState :: term()}.
handle_cast({save_checkpoint, Particle, Iteration, ParticleState},
            State = #state{ particles = Particles,
                            next_checkpoint = NextCheckpoint })
  when Iteration >= NextCheckpoint ->
    case lists:member(Particle, Particles) of
        true ->
            NewState = update_checkpoint(Particle, Iteration,
                                         ParticleState, State),
            case ready_to_save(NewState) of
                true ->
                    PendingCheckpoints = NewState#state.pending_checkpoints,
                    ok = save_checkpoint(
                           maps:get(NextCheckpoint, PendingCheckpoints),
                           State#state.file),
                    {noreply,
                     NewState#state{
                       pending_checkpoints =
                           drop_checkpoint(NextCheckpoint - 1,
                                           PendingCheckpoints),
                       next_checkpoint = NextCheckpoint + 1}};
                false ->
                    {noreply, NewState}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
          {noreply, NewState :: term()} |
          {noreply, NewState :: term(), Timeout :: timeout()} |
          {noreply, NewState :: term(), hibernate} |
          {stop, Reason :: normal | term(), NewState :: term()}.
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
                  State :: term(),
                  Extra :: term()) -> {ok, NewState :: term()} |
          {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
                    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec ready_to_save(State :: state()) -> boolean().
ready_to_save(#state{pending_checkpoints = PendingCheckpoints,
                     next_checkpoint = NextCheckpoint,
                     particles = Particles}) ->
    Checkpoint = maps:get(NextCheckpoint, PendingCheckpoints),
    lists:all(fun(P) -> maps:is_key(P, Checkpoint) end, Particles).

-spec save_checkpoint(CheckpointData :: checkpoint_data(),
                      CheckpointFile :: pid()) ->
          ok.
save_checkpoint(CheckpointData, CheckpointFile) ->
    Checkpoint = term_to_binary(CheckpointData),
    CheckpointSize = byte_size(Checkpoint),
    ok = file:write(CheckpointFile, << CheckpointSize:32, Checkpoint/binary >>).

-spec drop_checkpoint(
        Iteration :: integer(),
        PendingCheckpoints :: #{pos_integer() => checkpoint_data()})
                     -> #{pos_integer() => checkpoint_data()}.
drop_checkpoint(Iteration, PendingCheckpoints) ->
    maps:remove(Iteration, PendingCheckpoints).

-spec update_checkpoint(Particle :: particle_id(),
                        Iteration :: pos_integer(),
                        ParticleState :: checkpoint_state(),
                        State :: state()) -> state().
update_checkpoint(Particle, Iteration, ParticleState,
                  State = #state{ pending_checkpoints = PendingCheckpoints}) ->
    NewPendingCheckpoints = add_to_checkpoint(
                              Particle,
                              Iteration,
                              ParticleState,
                              PendingCheckpoints),
    State#state{ pending_checkpoints = NewPendingCheckpoints }.

add_to_checkpoint(Particle, Iteration, ParticleState, PendingCheckpoints) ->
    Checkpoint = maps:get(Iteration, PendingCheckpoints, #{}),
    PendingCheckpoints#{Iteration => Checkpoint#{Particle => ParticleState}}.

get_latest_checkpoint(ParticleId, Checkpoints) ->
    maps:fold(
      fun(Iteration, Checkpoint, Acc={I, _}) when I < Iteration ->
              case maps:get(ParticleId, Checkpoint, none) of
                  none -> Acc;
                  ParticleState ->
                      {Iteration, ParticleState}
              end;
         (Iteration, Checkpoint, none) ->
              case maps:get(ParticleId, Checkpoint, none) of
                  none -> none;
                  ParticleState ->
                      {Iteration, ParticleState}
              end;
         (_, _, Acc) ->
              Acc
      end, none, Checkpoints).

next_checkpoint_size(File) ->
    case file:read(File, 4) of
        eof -> 0;
        {ok, <<Size:32/integer>>} -> Size
    end.

load_checkpoint(File, Size) ->
    case file:read(File, Size) of
        {ok, Data} -> binary_to_term(Data)
    end.

load_latest(File, Iteration, Previous) ->
    case next_checkpoint_size(File) of
        Size when Size > 0 ->
            load_latest(File, Iteration + 1, load_checkpoint(File, Size));
        _ -> #{Iteration => Previous}
    end.

-spec load_checkpoint(File :: pid()) -> #{pos_integer() => checkpoint_data()}.
load_checkpoint(File) ->
    load_latest(File, 0, #{}).
