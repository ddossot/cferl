{application,
      cferl,
        [{description, "Rackspace Cloud Files client application"},
         {vsn, "1.1"},
         {modules, [
                     cferl,
                     cferl_connection,
                     cferl_container,
                     cferl_object,
                     cferl_lib,
                     mochijson2,
                     mochinum
                   ]},
         {registered, []},
         {applications, [kernel,stdlib,sasl,ssl,ibrowse]},
         {env, []}
        ]}.
