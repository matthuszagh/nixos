{ pkgs
, ...
}:

{
  services.jupyter = {
    enable = true;
    command = "jupyter-notebook";
    group = "users";
    ip = "localhost";
    port = 8888;
    user = "matt";
    # actual password stored in ~/src/nixos/secrets/jupyter_password
    password = "'sha1:2a4ef75c672a:ddc3ce9faa56ae9d4f18a2898a0fb33ba0aaae01'";
    notebookDir = "~/.jupyter";

    kernels = {
      python3 =
        let
          env = (pkgs.python3.withPackages (p: with p;
            [
              numpy
              matplotlib
              cython
              scipy
              ipykernel
              pandas
            ]));
        in
        {
          displayName = "Python 3 Jupyter kernel";
          argv = [
            "${env.interpreter}"
            "-m"
            "ipykernel_launcher"
            "-f"
            "{connection_file}"
          ];
          language = "python";
          # logo32 = "${env.sitePackages}/ipykernel/resources/logo-32x32.png";
          # logo64 = "${env.sitePackages}/ipykernel/resources/logo-64x64.png";
        };
    };
  };
}
