%% @author Marc Worrell
%% @copyright 2014 Marc Worrell
%% @doc Record returned by z_url_metadata:fetch/1

%% Copyright 2014 Marc Worrell
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

-record(url_metadata, {
    final_url :: binary(),
    content_type :: binary(),
    content_type_options :: list(),
    content_length :: integer(),
    metadata :: list(),
    is_index_page = false :: boolean(),
    headers :: list(),
    partial_data :: binary()
}).

