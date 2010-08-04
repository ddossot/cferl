{application,
      cferl,
        [{description, "Rackspace Cloud Files client application"},
         {vsn, "1.0"},
         {modules, [
                     cferl,
                     cferl_connection,
                     cferl_lib
		               ]},
         {registered, []},
         {applications, [kernel,stdlib,sasl,ssl,ibrowse]},
         {env, []}
        ]}.
