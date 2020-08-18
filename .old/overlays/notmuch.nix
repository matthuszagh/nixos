self: super:

{
  notmuch = (super.notmuch.overrideAttrs (old: {
    doCheck = false;
  }));
}
