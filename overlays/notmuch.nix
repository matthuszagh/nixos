final: prev:

{
  notmuch = (prev.notmuch.overrideAttrs (old: {
    doCheck = false;
  }));
}
