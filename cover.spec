%% Cover data file to export from this session.
%% CoverDataFile = string()
{export, "/tmp/lethink/coverage/ct_data.cover"}.

%% Cover analysis level.
%% Level = details | overview
{level, details}.

%% Specific modules to include in cover.
%% Mods = [atom()]
{incl_mods, [
    lethink,
    lethink_app,
    lethink_ast,
    lethink_server,
    lethink_sup,
    lethink_worker,
    lethink_workers_sup
]}.

%% Cross cover compilation
%% Tag = atom(), an identifier for a test run
%% Mod = [atom()], modules to compile for accumulated analysis
%% {cross,[{Tag,Mods}]}.
