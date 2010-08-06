{application,
      cferl,
        [{description, "Rackspace Cloud Files client application"},
         {vsn, "0.1"},
         {modules, [
                     cferl,
                     cferl_connection,
                     cferl_containers,
                     cferl_lib,
                     mochijson2,
                     mochinum
		               ]},
         {registered, []},
         {applications, [kernel,stdlib,sasl,ssl,ibrowse]},
         {env, []}
        ]}.
