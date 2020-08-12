{ stdenv
, python3Packages
, fetchFromGitHub
, kicad
, kinparse
, pyspice
, python
}:

python3Packages.buildPythonPackage rec {
  pname = "skidl";
  version = "unstable-2020-05-30";

  # src = /home/matt/src/skidl;
  src = fetchFromGitHub {
    owner = "xesscorp";
    repo = "skidl";
    rev = "07544b50c554874d4d99158deba1a987a00898cf";
    sha256 = "0fr27vnhwfy974jdvjzp5ygywin82i743nfczwin6naaxfc9lqam";
  };

  propagatedBuildInputs = (with python3Packages; [
    requests
    future
    kinparse
    enum34
    pyspice
    graphviz
    wxPython_4_0
    pillow
    pytest
    setuptools
    cffi
  ]) ++ [
    kicad
  ];

  # TODO
  # KICAD_SYMBOL_DIR = "${kicad.out}/share/kicad/library";
  doCheck = false;
  pythonImportsCheck = [ "skidl" ];

  # checkPhase = ''
  #   runHook preCheck;
  #   ${python3Packages.pytest.out}/bin/pytest tests
  #   runHook postCheck;
  # '';

  meta = with stdenv.lib; {
    description = "SKiDL is a module that extends Python with the ability to design electronic circuits";
    homepage = "https://xesscorp.github.io/skidl/docs/_site/";
    license = licenses.mit;
    maintainers = with maintainers; [ matthuszagh ];
  };
}
