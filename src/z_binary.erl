%% @author Linus Schoemaker
%% @copyright Copyright (c) 2018 Linus Schoemaker
%%
%% @doc Utility functions for dealing with binaries

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

-module(z_binary).
-author("Linus Schoemaker <de.linus@gmail.com>").

-export([
         join/2
        ]).

%% @doc Turn a list of binaries into a single binary separated by the given separator
%%      Example:
%%        z_binary:join([<<"foo">>, <<"bar">>], <<",">>) = <<"foo,bar"">>
-spec join([binary()], binary()) -> binary().
join(List, Sep) ->
    SepSize = byte_size(Sep),
    Result =
        lists:foldr(fun(Bin, Acc) ->
                            <<Bin/binary, Sep/binary, Acc/binary>>
                    end, <<>>, List),
    ResultSize = byte_size(Result),
    case SepSize > ResultSize of
        false ->
            binary:part(Result, 0,  ResultSize - SepSize);
        %% This should only happen when `List` is empty
        true ->
            Result
    end.
